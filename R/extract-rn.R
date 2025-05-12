#' Load Spectrum Resource Needs module data
#'
#' Read Spectrum Resource Needs module data as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @return an unformatted table of module data
#' @examples
#' rn.data = read.data.rn("Antarctica.PJNZ")
#' @export
read.raw.rn = function(pjnz.file) {
  return(read.module.data(pjnz.file, extension="RN"))
}

#' @noRd
extract.rn.tag = function(rn.raw, tag, fmt) {
  fmt$is.modvar = TRUE
  val = extract.raw.tag(rn.raw, tag, fmt)
  if (is.null(val)) {
    val = matrix(NA, nrow=fmt$nrow, ncol=fmt$ncol)
  }
  return(val)
}

#' Resource needs module (RNM) coverage inputs inputs
#' @param rn.raw RNM module data in raw format, as returned by
#'   \code{read.raw.rn()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection.
#' @param final.year Final year of the projection.
#' @return A data frame.
#' @export
rn.inputs.coverage = function(rn.raw, direction="wide", first.year, final.year) {
  fmt = list(cast=as.numeric, offset=3, nrow=27, ncol=final.year-first.year+2)
  raw = data.frame(extract.rn.tag(rn.raw, "<Coverage MV>", fmt))
  colnames(raw) = c("ID", first.year:final.year)
  dat = cbind(Program=strata.labels$rn.programs, raw)
  dat$ID = NULL

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Program"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}
