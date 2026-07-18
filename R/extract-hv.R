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

#' Helper function for extracting inputs by year, sex, and behavioral risk group
#' @noRd
hv.extract.time.series.by.population = function(hv.raw, direction="wide", first.year, final.year, tag) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  pop_m = strata.labels$hv.pop.ext
  pop_f = strata.labels$hv.pop.ext[1:6]
  num_row_m = length(pop_m)
  num_row_f = length(pop_f)
  years = sprintf("%d", first.year:final.year)

  fmt = list(cast=as.numeric, offset=3, nrow=num_row_m + num_row_f, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, tag, fmt)
  dat = cbind(rep(strata.labels$sex, c(num_row_m, num_row_f)), c(pop_m, pop_f), data.frame(raw))
  colnames(dat) = c("Sex", "Population", years)
  dat = dplyr::filter(dat, Population != "All")

  if (direction == "long") {
    dat = tidyr::pivot_longer(dat, cols=all_of(years), names_to = "Year", values_to="Value") |>
      dplyr::mutate(Year = as.numeric(as.character(Year)))
  }
  return(dat)
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

#' Extract behavioral risk group size and duration inputs
#' @inheritParams hv.inputs.first.year
#' @return A data frame.
#' @export
hv.inputs.population.sizes = function(hv.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=5, nrow=30, ncol=2)
  raw = extract.hv.tag(hv.raw, "<Behavior MV>", fmt)
  dat = data.frame(Population = c(strata.labels$hv.pop.ext[2:11], strata.labels$hv.pop[2:6]),
                   Sex = rep(strata.labels$sex, c(10, 5)),
                   Size = raw[is.finite(raw[,1]),1],
                   Duration = raw[is.finite(raw[,2]), 2])

  if (direction=="long") {
    dat = tidyr::pivot_longer(dat, cols=tidyr::all_of(c("Size", "Duration")), names_to="Indicator", values_to="Value")
  }

  return(dat)
}

#' Get the input by year, sex, and behavioral risk group
#' @param hv.raw Goals module data in raw format, as returned by
#'   \code{read.raw.hv()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{hv.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{hv.inputs.final.year()}
#' @return A data frame.
#' @describeIn hv.inputs.sex.acts sex acts per partnership per year
#' @export
hv.inputs.sex.acts = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  hv.extract.time.series.by.population(hv.raw, direction, first.year, final.year, "<SexActs MV2>")
}

#' @describeIn hv.inputs.sex.acts sexual partners per year
#' @export
hv.inputs.partners = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  hv.extract.time.series.by.population(hv.raw, direction, first.year, final.year, "<NumPart MV2>")
}

#' @describeIn hv.inputs.sex.acts STI prevalence
#' @export
hv.inputs.sti.prevalence = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  hv.extract.time.series.by.population(hv.raw, direction, first.year, final.year, "<STIPrev MV2>")
}

#' Extract data used for model fitting
#' @inheritParams hv.inputs.first.year
#' @return A data frame.
#' @describeIn hv.inputs.calibration.data Survey-based and study-based HIV prevalence estimates
#' @export
hv.inputs.calibration.data = function(hv.raw, direction="wide") {
  tag_v1 = "<FitData MV>"
  tag_v2 = "<FitData MV2>"
  tag_v3 = "<FitData MV3>"

  if (tag_v1 %in% hv.raw$Tag) {tag=tag_v1}
  if (tag_v2 %in% hv.raw$Tag) {tag=tag_v2}
  if (tag_v3 %in% hv.raw$Tag) {tag=tag_v3}

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

  if (tag==tag_v3) {
    cnames=c("Usage", "Population", "Sex", "Year", "Estimate", "Lower", "Upper", "N", "UseInFit", "Source")
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

  if (tag==tag_v3) {
    data_raw$Usage = factor(data_raw$Usage, levels=0:2, labels=c("Training", "Validation", "Exclude"))
  }

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

#' Get parameter configuration used for model calibration
#'
#' Get configuration settings for model parameters used to calibrate Goals. This
#' includes the initial and final calibrated values, prior distribution and its
#' hyperparameter values, whether the parameter was selected for model fitting,
#' and whether it was actually fitted. See the "Details" section for a
#' description of the return dataframe columns.
#' @inheritParams hv.inputs.first.year
#' @return a data frame
#' @section Details:
#'
#'  This returns a dataframe with the following columns:
#'  \describe{
#'  \item{Parameter}{The parameter name}
#'  \item{Prior}{The parameter prior distribution}
#'  \item{IntialValue}{The initial parameter value when fitting started}
#'  \item{Mean}{The prior distribution mean}
#'  \item{StdDev}{The prior distribution standard deviation}
#'  \item{FinalValue}{The best-fitting value found during fitting}
#'  \item{ParSelected}{Indicates whether the parameter was selected for inclusion in model fitting}
#'  \item{ParFitted}{Indicates whether the model fitting was run for the parameter}
#'  }
#' The output has separate fields for `ParSelected` and `ParFitted` because the
#' user could change what parameters are selected for model fitting without
#' actually running a model fit.
#' @export
hv.inputs.calibration.parameters = function(hv.raw, direction="wide") {
  ## <FitParamSet MV> was designed to be extensible. The same ModVar version
  ## stores different numbers of parameter records for different Spectrum
  ## versions. Only the latest (circa Spectrum 6.44, released July 2027) is
  ## supported currently.

  tag = "<FitParamSet MV>"
  fmt_nrow = list(cast=as.numeric, offset=3, offset_col=3, nrow=1, ncol=1)
  raw_nrow = extract.hv.tag(hv.raw, tag, fmt_nrow)[1]

  if (raw_nrow == 19) { # Spectrum 6.44 and later
    fmt_data = list(cast=as.numeric, offset=5, offset_col=3, nrow=raw_nrow-1, ncol=7)
    raw_data = data.frame(extract.hv.tag(hv.raw, tag, fmt_data))
    colnames(raw_data) = c("Prior", "InitialValue", "Mean", "StdDev", "FinalValue", "ParSelected", "ParFitted")
    raw_data$Parameter = c("Transmission of HIV per act",
                           "Transmission multiplier from male to female",
                           "Transmission multiplier for STI",
                           "Transmission multiplier for MSM contacts",
                           "Months in primary stage",
                           "Relative infectiousness during primary infection",
                           "Relative infectiousness during symptomatic infection",
                           "Size of the initial pulse of infection",
                           "Sex acts per partner, low risk heterosexual partnership",
                           "Sex acts per partner, medium risk heterosexual partnership",
                           "Sex acts per partner, high risk heterosexual partnership",
                           "Sex acts per partner, MSM partnership",
                           "Percent of PWID who share needles",
                           "Condom use & STI growth rate",
                           "Condom use & STI growth location",
                           "Condom use & STI years to final value",
                           "Dummy",
                           "Force of infection among PWID")

  } else {
    error("Unsupported FitParamSet version")
  }

  dat = raw_data |>
    dplyr::mutate(Prior = factor(Prior, levels=0:2, labels=c("Normal", "Beta", "Gamma")),
                  ParSelected = plyr::mapvalues(ParSelected, from=0:1, to=c(FALSE, TRUE), warn_missing = FALSE),
                  ParFitted   = plyr::mapvalues(ParFitted,   from=0:1, to=c(FALSE, TRUE), warn_missing = FALSE)) |>
    dplyr::relocate(Parameter) |>
    dplyr::filter(Parameter != "Dummy")

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
