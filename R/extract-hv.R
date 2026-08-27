#' Load Spectrum Goals RSM module data
#'
#' Read Spectrum Goals Risk-Structured Model (RSM) module data as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @return an unformatted table of module data
#' @examples
#' hv.data = read.data.hv("Antarctica.PJNZ")
#' @export
read.raw.hv = function(pjnz.file) {
  return(read.module.data(pjnz.file, extension="HV"))
}

#' @noRd
extract.hv.tag = function(hv.raw, tag, fmt) {
  fmt$is.modvar = TRUE
  val = extract.raw.tag(hv.raw, tag, fmt)
  if (is.null(val)) {
    val = matrix(NA, nrow=fmt$nrow, ncol=fmt$ncol)
  }
  return(val)
}


#' Spectrum projection time span
#'
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @param direction Ignored; included for compatibility with similar functions.
#' @return the requested year
#' @describeIn hv.inputs.first.year First year of the projection
#' @details This can also be accessed from the DemProj module via \code{dp.inputs.first.year}.
#' @export
hv.inputs.first.year = function(hv.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<FirstYear MV>", fmt)[1,1])
}

#' @describeIn hv.inputs.first.year Final year of the projection
#' @export
hv.inputs.final.year = function(hv.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<FinalYear MV>", fmt)[1,1])
}

#' Get the input reduction in HIV transmission on ART
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{hv.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{hv.inputs.final.year()}
#' @return A data frame.
#' @export
hv.inputs.art.effect = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=4, nrow=1, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<InfectMultiplierOnART MV>", fmt)
  if (direction=="long") {
    dat = data.frame(Year = first.year:final.year, Value=raw[1,])
  } else {
    dat = data.frame(raw)
    colnames(dat) = sprintf("%d", first.year:final.year)
  }
  return(dat)
}

#' Extract data used for model fitting
#' @inheritParams hv.inputs.first.year
#' @return A data frame.
#' @describeIn hv.inputs.calibration.data Survey-based and study-based HIV prevalence estimates
#' @export
hv.inputs.calibration.data = function(hv.raw, direction="wide") {
  tag_v1 = "<FitData MV>"
  tag_v2 = "<FitData MV2>"

  if (tag_v1 %in% hv.raw$Tag) {tag=tag_v1}
  if (tag_v2 %in% hv.raw$Tag) {tag=tag_v2}

  ## 1. Extract the number of rows of calibration data
  nrow_fmt = list(cast=as.numeric, offset=2, offset_col=3, nrow=1, ncol=1)
  nrow_val = extract.hv.tag(hv.raw, tag, nrow_fmt)[1]

  ## 2. Extract the calibration data
  if (tag==tag_v1) {
    cnames=c("Population", "Sex", "Year", "Estimate", "Lower", "Upper", "N", "UseInFit")
    data_fmt = list(cast=as.numeric, offset=3, offset_col=2, nrow=nrow_val, ncol=length(cnames))
    data_raw = as.data.frame(extract.hv.tag(hv.raw, tag, data_fmt))
  }

  if (tag==tag_v2) {
    cnames=c("Population", "Sex", "Year", "Estimate", "Lower", "Upper", "N", "UseInFit", "Source")
    data_fmt = list(cast=as.character, offset=3, offset_col=2, nrow=nrow_val, ncol=length(cnames))
    data_raw = as.data.frame(extract.hv.tag(hv.raw, tag, data_fmt))
    for (k in 1:(length(cnames)-1)) {
      data_raw[,k] = as.numeric(data_raw[,k])
    }
  }
  colnames(data_raw) = cnames

  data_raw$Population = factor(data_raw$Population, levels=0:6, labels=strata.labels$hv.pop)
  data_raw$Sex = factor(data_raw$Sex, levels=0:2, labels=strata.labels$sex.aug)
  data_raw$UseInFit = (data_raw$UseInFit == 1)

  return(data_raw)
}

#' @inheritParams hv.inputs.art.effect
#' @describeIn hv.inputs.calibration.data HIV prevalence time trends
#' @export
hv.inputs.hiv.prevalence = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=40, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<Prevalence MV>", fmt)
  dat = cbind(Population = c(strata.labels$hv.pop.ext,
                             strata.labels$sex.aug[2],
                             strata.labels$hv.pop.ext[1:6],
                             strata.labels$sex.aug[3],
                             strata.labels$sex.aug[1]),
              data.frame(raw[seq(2, 40, 2),]))
  colnames(dat) = c("Population", sprintf("%d", first.year:final.year))
  dat = dat[dat$Population != "All",] # drop unused rows

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Population"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get the estimated numbers of adults ages 15-49 in each model compartment over
#' time.
#' @inheritParams hv.inputs.art.effect
#' @return A data frame.
#' @export
hv.output.adults = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=3, offset_col=6, nrow=3302, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<Adults MV>", fmt)

  lab_sex = strata.labels$sex.aug
  lab_pop = strata.labels$hv.pop.ext
  lab_hiv = c("Negative", "Primary", strata.labels$cd4.adult, "Unused", "Unused", "Unused",
              sprintf("ART_%s", strata.labels$cd4.adult), "All")
  lab_vax = strata.labels$hv.vax

  num_sex = length(lab_sex)
  num_pop = length(lab_pop)
  num_hiv = length(lab_hiv)
  num_vax = length(lab_vax)

  ## The Adults output includes a block for males+females, but no values are
  ## written to that block, so SpectrumUtils ignores it
  num_block = num_pop * num_hiv * num_vax
  rows_m = 1:num_block + 1 + num_block
  rows_f = 1:num_block + 2 + num_block * 2

  block_m = cbind(Sex=strata.labels$sex[1], expand.grid(Vax=lab_vax, HIV=lab_hiv, Population=lab_pop), data.frame(raw[rows_m,]))
  block_f = cbind(Sex=strata.labels$sex[2], expand.grid(Vax=lab_vax, HIV=lab_hiv, Population=lab_pop), data.frame(raw[rows_f,]))

  dat = dplyr::bind_rows(dplyr::filter(block_m, Vax != "All" & Population != "All" & HIV != "All" & HIV != "Unused"),
                         dplyr::filter(block_f, Vax != "All" & Population != "All" & HIV != "All" & HIV != "Unused"))

  dat$ART = FALSE
  dat$ART[grep("ART_", dat$HIV)] = TRUE
  dat$HIV = gsub("ART_", "", dat$HIV)
  dat = dplyr::select(dat, "Sex", "Population", "HIV", "ART", "Vax", dplyr::everything())
  colnames(dat) = c("Sex", "Population", "HIV", "ART", "Vax", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Population", "HIV", "ART", "Vax"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}


## ---------------------------------------------------------------------------
## Goals RSM fitting/behavioural inputs
## ---------------------------------------------------------------------------

#' Base female-to-male per-act HIV transmission probability
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return Numeric scalar
#' @export
hv.inputs.trans.hiv.f = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<TransHIVF MV>", fmt)[1,1])
}

#' Male-to-female HIV transmission multiplier
#' @inheritParams hv.inputs.trans.hiv.f
#' @export
hv.inputs.trans.mult.m = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<TransMultM MV>", fmt)[1,1])
}

#' STI cofactor multiplier for HIV transmission
#' @inheritParams hv.inputs.trans.hiv.f
#' @export
hv.inputs.trans.mult.sti = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<TransMultSTI MV>", fmt)[1,1])
}

#' MSM HIV transmission multiplier
#' @inheritParams hv.inputs.trans.hiv.f
#' @export
hv.inputs.trans.mult.msm = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<TransMultMSM MV>", fmt)[1,1])
}

#' Condom effectiveness (%)
#' @inheritParams hv.inputs.trans.hiv.f
#' @export
hv.inputs.condom.eff = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<CondomEff MV>", fmt)[1,1])
}

#' Epidemic start year
#' @inheritParams hv.inputs.trans.hiv.f
#' @export
hv.inputs.epidemic.start.year = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<EpidemicStYr MV>", fmt)[1,1])
}

#' Initial HIV seed proportion
#' @inheritParams hv.inputs.trans.hiv.f
#' @export
hv.inputs.initial.pulse = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<InitialPulse MV>", fmt)[1,1])
}

#' Duration of primary HIV infection stage (months)
#' @inheritParams hv.inputs.trans.hiv.f
#' @export
hv.inputs.months.primary.stage = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<MonthsInPrimaryStage MV>", fmt)[1,1])
}

#' Mean age at first sex by sex over time
#' @inheritParams hv.inputs.art.effect
#' @return A data frame with columns Sex and one column per year (wide), or
#'   Sex, Year, Value (long).
#' @export
hv.inputs.age.first.sex = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=4, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<AgeFirstSex MV>", fmt)
  dat = cbind(Sex=strata.labels$sex, data.frame(raw[c(2,4),]))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars="Sex", variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Force of infection multiplier by sex over time
#' @inheritParams hv.inputs.art.effect
#' @return A data frame with columns Sex and one column per year (wide), or
#'   Sex, Year, Value (long).
#' @export
hv.inputs.force.inf = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=4, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<ForceInf MV>", fmt)
  dat = cbind(Sex=strata.labels$sex, data.frame(raw[c(2,4),]))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars="Sex", variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Percent of people who inject drugs sharing needles, over time
#' @inheritParams hv.inputs.art.effect
#' @return A data frame with columns Year and Value (long), or a single-row
#'   wide data frame with one column per year.
#' @export
hv.inputs.idu.sharing = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=1, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<PerIDUsharing MV>", fmt)

  if (direction == "long") {
    dat = data.frame(Year=first.year:final.year, Value=raw[1,])
  } else {
    dat = data.frame(raw)
    colnames(dat) = sprintf("%d", first.year:final.year)
  }
  return(dat)
}

## Shared helper for risk-group x time-series MV2 blocks (NumPart, SexActs,
## STIPrev): label and data share a row, with only a subset of rows active.
## Active row offsets from tag (0-indexed): male start=5, female start=16.
#' @noRd
hv.rsm.read.mv2.timeseries = function(hv.raw, tag, first.year, final.year,
                                        male_offset=5, male_nrow=5,
                                        female_offset=16, female_nrow=4) {
  n_years = final.year - first.year + 1
  yr_cols = sprintf("%d", first.year:final.year)

  male_lbl = extract.hv.tag(hv.raw, tag, list(cast=as.character, offset=male_offset, offset_col=2, nrow=male_nrow, ncol=1))[,1]
  male_raw = extract.hv.tag(hv.raw, tag, list(cast=as.numeric, offset=male_offset, nrow=male_nrow, ncol=n_years))

  fem_lbl = extract.hv.tag(hv.raw, tag, list(cast=as.character, offset=female_offset, offset_col=2, nrow=female_nrow, ncol=1))[,1]
  fem_raw = extract.hv.tag(hv.raw, tag, list(cast=as.numeric, offset=female_offset, nrow=female_nrow, ncol=n_years))

  male_df = setNames(cbind(Sex="Male", RiskGroup=male_lbl, data.frame(male_raw)), c("Sex", "RiskGroup", yr_cols))
  fem_df  = setNames(cbind(Sex="Female", RiskGroup=fem_lbl, data.frame(fem_raw)), c("Sex", "RiskGroup", yr_cols))
  return(rbind(male_df, fem_df))
}

#' Average number of partners by risk group over time
#' @inheritParams hv.inputs.art.effect
#' @return A data frame with columns Sex, RiskGroup, and one column per year
#'   (wide), or Sex, RiskGroup, Year, Value (long).
#' @export
hv.inputs.num.partners = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  dat = hv.rsm.read.mv2.timeseries(hv.raw, "<NumPart MV2>", first.year, final.year)

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "RiskGroup"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Sex acts per year by risk group over time
#' @inheritParams hv.inputs.art.effect
#' @return A data frame with columns Sex, RiskGroup, and one column per year
#'   (wide), or Sex, RiskGroup, Year, Value (long).
#' @export
hv.inputs.sex.acts = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  dat = hv.rsm.read.mv2.timeseries(hv.raw, "<SexActs MV2>", first.year, final.year)

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "RiskGroup"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' STI prevalence by risk group over time
#' @inheritParams hv.inputs.art.effect
#' @return A data frame with columns Sex, RiskGroup, and one column per year
#'   (wide), or Sex, RiskGroup, Year, Value (long).
#' @export
hv.inputs.sti.prev = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  dat = hv.rsm.read.mv2.timeseries(hv.raw, "<STIPrev MV2>", first.year, final.year, female_nrow=3)

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "RiskGroup"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Condom use percentage by risk group over time
#' @inheritParams hv.inputs.art.effect
#' @return A data frame with columns RiskGroup and one column per year (wide),
#'   or RiskGroup, Year, Value (long).
#' @export
hv.inputs.condom.percent = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=14, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<CondomPercent MV>", fmt)

  dat = cbind(RiskGroup=strata.labels$rsm.condom.groups, data.frame(raw[seq(2,14,2),]))
  colnames(dat) = c("RiskGroup", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars="RiskGroup", variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Percent married/cohabiting by risk group and sex (static)
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return A data frame with columns Sex, RiskGroup, Value.
#' @export
hv.inputs.perc.married = function(hv.raw) {
  tag = "<PercMarried MV2>"

  male_lbl = extract.hv.tag(hv.raw, tag, list(cast=as.character, offset=5, offset_col=2, nrow=5, ncol=1))[,1]
  male_val = extract.hv.tag(hv.raw, tag, list(cast=as.numeric, offset=5, nrow=5, ncol=1))[,1]
  fem_lbl  = extract.hv.tag(hv.raw, tag, list(cast=as.character, offset=16, offset_col=2, nrow=4, ncol=1))[,1]
  fem_val  = extract.hv.tag(hv.raw, tag, list(cast=as.numeric, offset=16, nrow=4, ncol=1))[,1]

  return(rbind(data.frame(Sex="Male", RiskGroup=male_lbl, Value=male_val),
              data.frame(Sex="Female", RiskGroup=fem_lbl, Value=fem_val)))
}

#' Per-act HIV infectiousness by disease stage (static)
#' @details Only 3 active stages are returned (primary, asymptomatic,
#'   symptomatic without ART). A 4th row present in the file (symptomatic on
#'   ART) is a known data error and is excluded.
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return A data frame with columns Stage, Value.
#' @export
hv.inputs.infectiousness = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=4, nrow=6, ncol=1)
  raw = extract.hv.tag(hv.raw, "<Infectiousness MV>", fmt)
  return(data.frame(Stage=strata.labels$rsm.infectiousness.stages, Value=raw[seq(2,6,2),1]))
}

#' Risk group size and turnover by sex (static)
#' @details Returns the percent of the population in each risk group and the
#'   mean number of months spent in that group.
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return A data frame with columns Sex, RiskGroup, percent, avg_duration_months.
#' @export
hv.inputs.behavior = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=3, nrow=32, ncol=2)
  raw = extract.hv.tag(hv.raw, "<Behavior MV>", fmt)

  ## Males: 10 groups at matrix rows 3,5,...,21; Females: 5 groups at rows 24,26,...,32
  male_data_rows = seq(3, 21, 2)
  female_data_rows = seq(24, 32, 2)

  label_fmt = list(cast=as.character, offset=3, offset_col=2, nrow=32, ncol=1)
  labels_raw = extract.hv.tag(hv.raw, "<Behavior MV>", label_fmt)[,1]
  male_labels = labels_raw[male_data_rows - 1]
  female_labels = labels_raw[female_data_rows - 1]

  male_block = cbind(Sex="Male", RiskGroup=male_labels,
                     percent=raw[male_data_rows,1], avg_duration_months=raw[male_data_rows,2])
  female_block = cbind(Sex="Female", RiskGroup=female_labels,
                       percent=raw[female_data_rows,1], avg_duration_months=raw[female_data_rows,2])

  return(data.frame(rbind(male_block, female_block)))
}

#' Risk group recruitment proportions by sex (static)
#' @details Proportion of new entrants (age 15) recruited into each risk group.
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return A data frame with columns Sex, RiskGroup, Value.
#' @export
hv.inputs.recruitment = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=3, nrow=42, ncol=1)
  raw = extract.hv.tag(hv.raw, "<IncRecruitment MV>", fmt)

  label_fmt = list(cast=as.character, offset=3, offset_col=2, nrow=42, ncol=1)
  labels_raw = extract.hv.tag(hv.raw, "<IncRecruitment MV>", label_fmt)[,1]

  ## Males: sex header row 1, then 10 groups; Females: sex header at row 22, same structure
  male_data_rows = seq(3, 21, 2)
  female_data_rows = seq(24, 42, 2)

  male_labels = labels_raw[male_data_rows - 1]
  female_labels = labels_raw[female_data_rows - 1]

  return(rbind(data.frame(Sex="Male", RiskGroup=male_labels, Value=raw[male_data_rows,1]),
              data.frame(Sex="Female", RiskGroup=female_labels, Value=raw[female_data_rows,1])))
}

#' Intervention impact matrix: percent change in behaviour by intervention and outcome
#' @details 13 interventions (rows) by 14 behaviour-change outcomes (columns).
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return A data frame with column Intervention and one column per outcome.
#' @export
hv.inputs.impact.matrix = function(hv.raw) {
  ## Data starts at col 5 (R 1-indexed) because the <Value> marker occupies
  ## the Description col on the first data row, shifting data one column right.
  fmt = list(cast=as.numeric, offset=2, offset_col=5, nrow=13, ncol=14)
  raw = extract.hv.tag(hv.raw, "<ImpactMatrix MV>", fmt)
  dat = cbind(Intervention=strata.labels$rsm.impact.interventions, data.frame(raw))
  colnames(dat) = c("Intervention", strata.labels$rsm.impact.outcomes)
  return(dat)
}

#' Fitting parameter set: bounds and current values for the RSM's fitted parameters
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return A data frame with columns Parameter, distribution
#'   ("Normal"/"Gamma"/"Beta"), initial_value, mean, sd, final_value, include.
#' @export
hv.inputs.fit.params = function(hv.raw) {
  ## 17 param rows at offset 5 (skip tag, blank, <Value>, nparams, blank)
  fmt = list(cast=as.numeric, offset=5, offset_col=2, nrow=17, ncol=8)
  raw = extract.hv.tag(hv.raw, "<FitParamSet MV>", fmt)
  dat = data.frame(raw)
  colnames(dat) = c("param_index", "distribution", "initial_value", "mean", "sd", "final_value", "include", "col8")
  dat$Parameter = strata.labels$rsm.fit.param.names
  dat$distribution = factor(dat$distribution, levels=0:2, labels=c("Normal", "Gamma", "Beta"))
  dat$include = (dat$include == 1)
  return(dat[,c("Parameter", "distribution", "initial_value", "mean", "sd", "final_value", "include")])
}

#' Fitting algorithm configuration
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @return A named list with max_iterations, error_tolerance, weight.
#' @export
hv.inputs.fit.control = function(hv.raw) {
  fmt = list(cast=as.numeric, offset=3, offset_col=2, nrow=1, ncol=3)
  raw = extract.hv.tag(hv.raw, "<FitControl MV>", fmt)
  return(list(max_iterations=raw[1,1], error_tolerance=raw[1,2], weight=raw[1,3]))
}


## ---------------------------------------------------------------------------
## Goals RSM long-format wrapper
## ---------------------------------------------------------------------------

#' @noRd
hv.rsm.to.snake = function(x) {
  x = tolower(trimws(as.character(x)))
  x = gsub("[^a-z0-9]+", "_", x)
  return(gsub("^_|_$", "", x))
}

#' @noRd
hv.rsm.long.rows = function(variable, year, type, value, sex=NA, risk_group=NA,
                             lower=NA, upper=NA, n_obs=NA, use_in_fit=NA,
                             initial_value=NA, distribution=NA, include=NA) {
  return(data.frame(
    variable=variable, sex=sex, risk_group=risk_group, year=year, type=type,
    value=as.numeric(value), lower=as.numeric(lower), upper=as.numeric(upper),
    n_obs=as.numeric(n_obs), use_in_fit=use_in_fit, initial_value=as.numeric(initial_value),
    distribution=distribution, include=include, stringsAsFactors=FALSE))
}

#' Extract all Goals RSM HV inputs into a named list of two tidy data frames
#'
#' @details Returns \code{list(const, time_varying)} where \code{const} is
#'   long format covering all time-invariant indicators (year=9999 for static
#'   scalars, year=0 for time series confirmed constant across all projection
#'   years) and \code{time_varying} is wide format with years as columns
#'   (currently \code{sti_prev} and \code{condom_percent}).
#'
#'   Columns lower/upper/n_obs/use_in_fit apply only to fit_data_prevalence
#'   rows (NA elsewhere); initial_value/distribution/include apply only to
#'   fit_param_ rows.
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @param file_name Source PJNZ filename, stored for multi-file comparisons
#' @param iso3 ISO3 country code, stored for multi-file comparisons
#' @param write_out If TRUE, write an Excel file with one sheet per data frame
#' @param out_dir Directory for the Excel output (used when write_out=TRUE)
#' @return A named list with elements \code{const} and \code{time_varying}.
#' @export
extract_all_hv = function(hv.raw, file_name=NA, iso3=NA, write_out=FALSE, out_dir=".") {
  fy = hv.inputs.first.year(hv.raw)
  ly = hv.inputs.final.year(hv.raw)
  out = list()

  scalars_epi = list(
    trans_hiv_f = hv.inputs.trans.hiv.f(hv.raw),
    trans_mult_m = hv.inputs.trans.mult.m(hv.raw),
    trans_mult_sti = hv.inputs.trans.mult.sti(hv.raw),
    trans_mult_msm = hv.inputs.trans.mult.msm(hv.raw),
    condom_eff = hv.inputs.condom.eff(hv.raw),
    epidemic_start_year = hv.inputs.epidemic.start.year(hv.raw),
    initial_pulse = hv.inputs.initial.pulse(hv.raw),
    months_primary_stage = tryCatch(hv.inputs.months.primary.stage(hv.raw), error=function(e) NA))
  out[["scalars_epi"]] = hv.rsm.long.rows(variable=names(scalars_epi), year=9999, type="epi", value=unlist(scalars_epi))

  inf = hv.inputs.infectiousness(hv.raw)
  inf_varnames = c("infect_par_primary", "infect_par_asymptomatic", "infect_par_symptomatic_no_art")
  out[["infectiousness"]] = hv.rsm.long.rows(variable=inf_varnames, year=9999, type="epi", value=inf$Value)

  afs = hv.inputs.age.first.sex(hv.raw, direction="long", first.year=fy, final.year=ly)
  afs0 = afs[afs$Year == fy,]
  out[["age_first_sex"]] = hv.rsm.long.rows(variable="age_first_sex", sex=afs0$Sex, year=0, type="behavioural", value=afs0$Value)

  fi = hv.inputs.force.inf(hv.raw, direction="long", first.year=fy, final.year=ly)
  fi0 = fi[fi$Year == fy,]
  out[["force_inf"]] = hv.rsm.long.rows(variable="force_inf", sex=fi0$Sex, year=0, type="epi", value=fi0$Value)

  idu = hv.inputs.idu.sharing(hv.raw, direction="long", first.year=fy, final.year=ly)
  out[["idu_sharing"]] = hv.rsm.long.rows(variable="idu_sharing", year=0, type="behavioural", value=idu$Value[1])

  np = hv.inputs.num.partners(hv.raw, direction="long", first.year=fy, final.year=ly)
  np0 = np[np$Year == fy,]
  out[["num_partners"]] = hv.rsm.long.rows(variable="num_partners", sex=np0$Sex, risk_group=np0$RiskGroup, year=0, type="behavioural", value=np0$Value)

  sa = hv.inputs.sex.acts(hv.raw, direction="long", first.year=fy, final.year=ly)
  sa0 = sa[sa$Year == fy,]
  out[["sex_acts"]] = hv.rsm.long.rows(variable="sex_acts", sex=sa0$Sex, risk_group=sa0$RiskGroup, year=0, type="behavioural", value=sa0$Value)

  sp = hv.inputs.sti.prev(hv.raw, direction="long", first.year=fy, final.year=ly)
  out[["sti_prev"]] = hv.rsm.long.rows(variable="sti_prev", sex=sp$Sex, risk_group=sp$RiskGroup, year=sp$Year, type="behavioural", value=sp$Value)

  cp = hv.inputs.condom.percent(hv.raw, direction="long", first.year=fy, final.year=ly)
  out[["condom_percent"]] = hv.rsm.long.rows(variable="condom_percent", risk_group=cp$RiskGroup, year=cp$Year, type="behavioural", value=cp$Value)

  beh = hv.inputs.behavior(hv.raw)
  beh$percent = as.numeric(beh$percent)
  beh$avg_duration_months = as.numeric(beh$avg_duration_months)
  out[["rg_size_percent"]] = hv.rsm.long.rows(variable="fit_data_pse", sex=beh$Sex, risk_group=beh$RiskGroup, year=9999, type="fit_data", value=beh$percent)
  ## value = 1000 means lifetime (group membership is permanent)
  out[["rg_turnover_months"]] = hv.rsm.long.rows(variable="rg_turnover_months", sex=beh$Sex, risk_group=beh$RiskGroup, year=9999, type="behavioural", value=beh$avg_duration_months)

  pm = hv.inputs.perc.married(hv.raw)
  out[["perc_married"]] = hv.rsm.long.rows(variable="perc_married", sex=pm$Sex, risk_group=pm$RiskGroup, year=9999, type="behavioural", value=pm$Value)

  rec = hv.inputs.recruitment(hv.raw)
  out[["recruitment"]] = hv.rsm.long.rows(variable="recruitment", sex=rec$Sex, risk_group=rec$RiskGroup, year=9999, type="behavioural", value=rec$Value)

  im = hv.inputs.impact.matrix(hv.raw)
  im_long = reshape2::melt(im, id.vars="Intervention", variable.name="Outcome", value.name="Value")
  im_long$variable = paste0("impact__", hv.rsm.to.snake(im_long$Intervention), "__", hv.rsm.to.snake(as.character(im_long$Outcome)))
  out[["impact_matrix"]] = hv.rsm.long.rows(variable=im_long$variable, year=9999, type="behavioural", value=im_long$Value)

  fp = hv.inputs.fit.params(hv.raw)
  out[["fit_params"]] = hv.rsm.long.rows(variable=paste0("fit_param_", hv.rsm.to.snake(fp$Parameter)), year=9999, type="fit_param",
                                          value=fp$final_value, initial_value=fp$initial_value,
                                          distribution=as.character(fp$distribution), include=as.integer(fp$include))

  fc = hv.inputs.fit.control(hv.raw)
  out[["fit_control"]] = hv.rsm.long.rows(variable=c("fit_control_max_iterations", "fit_control_error_tolerance", "fit_control_weight"),
                                           year=9999, type="fit_param", value=c(fc$max_iterations, fc$error_tolerance, fc$weight))

  fd = hv.inputs.calibration.data(hv.raw)
  out[["fit_data"]] = hv.rsm.long.rows(variable="fit_data_prevalence", sex=fd$Sex, risk_group=fd$Population, year=fd$Year, type="fit_data",
                                        value=fd$Estimate, lower=fd$Lower, upper=fd$Upper, n_obs=fd$N, use_in_fit=fd$UseInFit)

  all_long = do.call(rbind, out)
  row.names(all_long) = NULL
  all_long$file_name = file_name
  all_long$iso3 = iso3

  tv_vars = c("sti_prev", "condom_percent")
  const_df = all_long[!all_long$variable %in% tv_vars,]
  tv_long  = all_long[all_long$variable %in% tv_vars,]
  tv_wide  = reshape2::dcast(tv_long, variable + sex + risk_group + type + file_name + iso3 ~ year, value.var="value")

  result = list(const=const_df, time_varying=tv_wide)

  if (write_out) {
    fname_base = if (!is.na(file_name)) sub("\\.PJNZ$", "", file_name, ignore.case=TRUE) else "assumptions"
    xl_path = file.path(out_dir, paste0(fname_base, "_assumptions.xlsx"))
    wb = openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "const")
    openxlsx::addWorksheet(wb, "time_varying")
    openxlsx::writeData(wb, "const", const_df)
    openxlsx::writeData(wb, "time_varying", tv_wide)
    openxlsx::saveWorkbook(wb, xl_path, overwrite=TRUE)
    message("Written: ", xl_path)
  }

  return(result)
}

#' Extract Goals RSM HV inputs from all PJNZ files in a folder
#'
#' @details Calls \code{extract_all_hv} on each .PJNZ file found in
#'   \code{folder}, derives iso3 from the first three characters of the
#'   filename, and combines results into a named list with \code{const}
#'   (long) and \code{time_varying} (wide) data frames.
#' @param folder Path to a folder containing .PJNZ files
#' @param pattern Regex used to filter filenames (default matches all .PJNZ)
#' @param write_out If TRUE, write an Excel file named assumptions.xlsx to \code{folder}
#' @return A named list with elements \code{const} and \code{time_varying}.
#' @export
extract_all_hv_folder = function(folder, pattern="\\.PJNZ$", write_out=FALSE) {
  files = list.files(folder, pattern=pattern, full.names=TRUE, ignore.case=TRUE)
  if (length(files) == 0) {stop("No .PJNZ files found in: ", folder)}

  const_list = vector("list", length(files))
  tv_list    = vector("list", length(files))

  for (i in seq_along(files)) {
    f     = files[[i]]
    fname = basename(f)
    iso3  = toupper(substr(fname, 1, 3))
    message(sprintf("[%d/%d] %s", i, length(files), fname))
    res = tryCatch(
      extract_all_hv(read.raw.hv(f), file_name=fname, iso3=iso3),
      error = function(e) {
        warning(sprintf("Failed for %s: %s", fname, conditionMessage(e)))
        NULL
      })
    if (!is.null(res)) {
      const_list[[i]] = res$const
      tv_list[[i]]    = res$time_varying
    }
  }

  const_df = do.call(rbind, Filter(Negate(is.null), const_list))
  tv_df    = do.call(rbind, Filter(Negate(is.null), tv_list))
  row.names(const_df) = NULL
  row.names(tv_df)    = NULL

  result = list(const=const_df, time_varying=tv_df)

  if (write_out) {
    xl_path = file.path(folder, "assumptions.xlsx")
    wb = openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "const")
    openxlsx::addWorksheet(wb, "time_varying")
    openxlsx::writeData(wb, "const", const_df)
    openxlsx::writeData(wb, "time_varying", tv_df)
    openxlsx::saveWorkbook(wb, xl_path, overwrite=TRUE)
    message("Written: ", xl_path)
  }

  return(result)
}
