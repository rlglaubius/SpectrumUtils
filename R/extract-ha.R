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

#' Get viral suppression inputs
#'
#' Get input levels of viral suppression among adult PLHIV on ART by year and
#' age group
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{ha.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{ha.inputs.final.year()}
#' @return A data frame.
#' @export
ha.inputs.viral.suppression = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  fmt = list(cast=as.numeric, offset=1, nrow=8, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, "<ARTViralSuppression>", fmt)
  dat = cbind(Sex = rep(rev(strata.labels$sex), each=4),
              Age = rep(strata.labels$age.cd4.adult, 2),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the number of infections transmitted
#'
#' Get the number of infections transmitted by PLHIV according to sex, age,
#' infection stage, and ART status
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{ha.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{ha.inputs.final.year()}
#' @return A data frame.
#' @export
ha.output.transmitted = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  lab_sex = c("Male", "Female") # strata.labels$sex
  lab_age = c(sprintf("%d", 15:79), "80+")
  lab_hiv = c("Primary", "Asymptomatic", "Symptomatic")
  lab_art = c("No ART", "Unused1", "Unused2", "Unused3", "[0,6) months", "[6,12) months", "12+ months")

  num_sex = length(lab_sex)
  num_age = length(lab_age)
  num_hiv = length(lab_hiv)
  num_art = length(lab_art)

  fmt = list(cast=as.numeric, offset=5, nrow=num_sex * num_age * num_hiv * num_art, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, "<Number of infections transmitted>", fmt)
  dat = cbind(expand.grid(ART=lab_art, Stage=lab_hiv, Age=lab_age, Sex=lab_sex),
              data.frame(raw))
  colnames(dat) = c("ART", "Stage", "Age", "Sex", sprintf("%d", first.year:final.year))
  dat = dat[!(dat$ART %in% c("Unused1", "Unused2", "Unused3")),]
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age", "Stage", "ART"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}
