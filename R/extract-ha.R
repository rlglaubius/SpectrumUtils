#' Load Spectrum Goal ASM module data
#'
#' Read Spectrum Goals ASM module data as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @return an unformatted table of module data
#' @examples
#' ha.data = read.data.ha("Antarctica.PJNZ")
#' @export
read.raw.ha = function(pjnz.file) {
  return(read.module.data(pjnz.file, extension="HA"))
}

extract.ha.tag = function(ha.raw, tag, fmt) {
  fmt$is.modvar = FALSE
  val = extract.raw.tag(ha.raw, tag, fmt)
  if (is.null(val)) {
    val = matrix(NA, nrow=fmt$nrow, ncol=fmt$ncol)
  }
  return(val)
}

#' Get the first year of a Spectrum projection
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Ignored; included for compatibility with similar functions.
#' @return the first year of the projection.
#' @section Details:
#'
#' Analogous functions are available for other modules. For example, \code{dp.inputs.first.year} will
#' extract the first year of the projection via the DemProj module. This is
#' expected to match \code{ha.inputs.first.year}
#'
#' @export
ha.inputs.first.year = function(ha.raw, direction="wide") {
  fmt = list(cast = as.character, offset=2, nrow=9, ncol=1)
  raw = extract.ha.tag(ha.raw, "<General>", fmt)[5,1]
  return(as.numeric(raw))
}

#' Get the final year of a Spectrum projection
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Ignored; included for compatibility with similar functions.
#' @return the final year of the projection.
#' @section Details:
#'
#' Analogous functions are available for other modules. For example, \code{dp.inputs.final.year} will
#' extract the final year of the projection via the DemProj module. This is
#' expected to match \code{ha.inputs.final.year}
#'
#' @export
ha.inputs.final.year = function(ha.raw, direction="wide") {
  fmt = list(cast = as.character, offset=2, nrow=9, ncol=1)
  raw = extract.ha.tag(ha.raw, "<General>", fmt)[7,1]
  return(as.numeric(raw))
}

#' Get input key population sizes
#'
#' Extract input key population sizes. These are expressed as percentages of the
#' age-specific population (15-24 or 25+) by sex who are key population members.
#' For example, if the 15-24 MSM population size is 1.1, then 1.1% fo 15-24
#' males are assumed to be MSM.
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{ha.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{ha.inputs.final.year()}
#' @return A data frame.
#' @export
ha.inputs.keypop.size = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  ## The data encoding has several metadata rows, and interleaves unused rows
  ## with data rows. Extraction ignores the metadata and drop the unused rows.
  fmt = list(cast=as.numeric, offset=4, nrow=11, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, "<Key Population Size V2>", fmt)
  raw = raw[c(1:2, 4:5, 7:8, 10:11),]
  dat = cbind(Sex = rep(rev(strata.labels$sex), each=4),
              Age = rep(c("15-24", "25+"), 4),
              Population = rep(c("FSW", "PWID", "MSM", "PWID"), each=2),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", "Population", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age", "Population"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}


