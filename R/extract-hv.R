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
