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

#' Extract survey and study data used for model fitting
#' @inheritParams hv.inputs.first.year
#' @return A data frame.
#' @export
hv.inputs.calibration.data = function(hv.raw, direction="wide") {
  tag = "<FitData MV>"

  ## 1. Extract the number of rows of calibration data
  nrow_fmt = list(cast=as.numeric, offset=2, offset_col=3, nrow=1, ncol=1)
  nrow_val = extract.hv.tag(hv.raw, tag, nrow_fmt)[1]

  ## 2. Extract the calibration data
  data_fmt = list(cast=as.numeric, offset=3, offset_col=2, nrow=nrow_val, ncol=8)
  data_raw = as.data.frame(extract.hv.tag(hv.raw, tag, data_fmt))
  colnames(data_raw) = c("Population", "Sex", "Year", "Estimate", "Lower", "Upper", "N", "UseInFit")

  data_raw$Population = factor(data_raw$Population, levels=0:6, labels=strata.labels$hv.pop)
  data_raw$Sex = factor(data_raw$Sex, levels=0:2, labels=strata.labels$sex.aug)
  data_raw$UseInFit = (data_raw$UseInFit == 1)

  return(data_raw)
}

