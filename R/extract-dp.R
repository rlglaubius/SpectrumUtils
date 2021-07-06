#' Load Spectrum DemProj module data
#'
#' Read Spectrum DemProj module data as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @return an unformatted table of module data
#' @examples
#' dp.data = read.data.dp("Antarctica.PJNZ")
#' @export
read.raw.dp = function(pjnz.file) {
  return(read.module.data(pjnz.file, extension="DP"))
}

extract.dp.tag = function(dp.raw, tag, fmt) {
  fmt$is.modvar = TRUE
  return(extract.raw.tag(dp.raw, tag, fmt))
}

#' Get the first year of a Spectrum projection
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Ignored; included for compatibility with similar functions.
#' @return the first year of the projection.
#' @export
dp.inputs.first.year = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<FirstYear MV2>", fmt)[1,1])
}

#' Get the final year of a Spectrum projection
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Ignored; included for compatibility with similar functions.
#' @return the final year of the projection.
#' @export
dp.inputs.final.year = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<FinalYear MV2>", fmt)[1,1])
}

#' Get Spectrum's calculated population
#'
#' Get Spectrum's calculated population by age, sex, and year in long or wide
#' format
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @section Details:
#'
#'   If you plan to extract multiple, time-dependent DemProj variables, it is
#'   more efficient to save the first year and final year of the projection,
#'   then pass those values to subsequent function calls. If you leave the first
#'   and final year \code{NULL}, the code will call
#'   \code{dp.inputs.first.year()} and \code{dp.inputs.final.year()} each time
#'   you request a variable.
#'
#' @export
dp.output.bigpop = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=162, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<BigPop MV3>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age)),
              rep(strata.labels$age, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get Spectrum's calculated HIV-positive population
#'
#' Get Spectrum's calculated HIV-positive population by age, sex, and year in long or wide
#' format
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.output.hivpop = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=162, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<HIVBySingleAge MV2>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age)),
              rep(strata.labels$age, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get Spectrum's calculated HIV-positive population on ART
#'
#' Get Spectrum's calculated HIV-positive population on ART by age, sex, and year in long or wide
#' format
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.output.artpop = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=2, nrow=243, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<OnARTBySingleAge MV>", fmt)
  dat = cbind(rep(strata.labels$sex.aug, length(strata.labels$age)),
              rep(strata.labels$age, each=length(strata.labels$sex.aug)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get Spectrum's calculated new HIV infections
#'
#' Get Spectrum's calculated new HIV infections by age, sex, and year in long or
#' wide format
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.output.incident.hiv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=2, nrow=243, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<NewInfectionsBySingleAge MV>", fmt)
  dat = cbind(rep(strata.labels$sex.aug, length(strata.labels$age)),
              rep(strata.labels$age, each=length(strata.labels$sex.aug)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get Spectrum's calculated HIV-related deaths
#'
#' Get Spectrum's calculated HIV-related deaths by age, sex, and year in long or
#' wide format
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.output.deaths.hiv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=162, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<AidsDeathsByAge MV2>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age)),
              rep(strata.labels$age, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get Spectrum's calculated HIV-unrelated deaths
#'
#' Get Spectrum's calculated HIV-unrelated deaths by age, sex, and year in long or
#' wide format
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.output.deaths.nonhiv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=162, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<DeathsByAge MV2>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age)),
              rep(strata.labels$age, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get Spectrum adult ART inputs
#'
#' Get input numbers or percentages of adults on ART entered into Spectrum by
#' sex and year in long or wide format
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.inputs.adult.art = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## Read ART coverage values (numbers and/or percentages)
  data.fmt = list(cast=as.numeric, offset=3, nrow=3, ncol=final.year - first.year + 1)
  data.raw = extract.dp.tag(dp.raw, "<HAARTBySex MV>", data.fmt)

  ## Read ART coverage units (0: number, 1: percent)
  unit.fmt = list(cast=as.numeric, offset=3, nrow=3, ncol=final.year - first.year + 1)
  unit.raw = extract.dp.tag(dp.raw, "<HAARTBySexPerNum MV>", unit.fmt)

  ## Decode and annotate the data
  num.raw = data.raw
  num.raw[unit.raw==1] = NA
  prc.raw = data.raw
  prc.raw[unit.raw==0] = NA

  dat = cbind(rep(strata.labels$sex.aug, 2),
              rep(c("Number", "Percent"), each=3),
              rbind(data.frame(num.raw),
                    data.frame(prc.raw)))
  names(dat) = c("Sex", "Unit", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Unit"), variable.name="Year", value.name="Value")
    View(dat)
    dat$Year = as.numeric(as.character(dat$Year))
    dat = subset(dat, is.finite(Value)) # Prune "NA" values
  }

  ## Drop Male+Female values because these are calculated by Spectrum, and only
  ## for years when inputs are entered as numbers
  return(subset(dat, Sex != "Male+Female"))
}

#' Get Spectrum ART by age inputs
#'
#' Get input numbers of people on ART entered into Spectrum by sex, age and year
#' in long or wide format. These data may be entered by five-year age group, or
#' in age groups specified for UNAIDS Global AIDS Monitoring. The latter
#' stratifies ages 25+ into two age groups, 25-49 and 50+.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.inputs.art.by.age = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## Check whether data are by 5-year (type=0) or GAM (type=1) age groups
  type.fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  type.raw = extract.dp.tag(dp.raw, "<ARTByAgeInputType MV>", type.fmt)[1,1]

  if (!(type.raw %in% c(0,1))) {
    warning(sprintf("Unrecognized format '%s' for ART by age, GAM ages returned", type.raw))
  }

  if (type.raw == 0) {
    nr = length(strata.labels$sex) * length(strata.labels$age.5yr)
    data.fmt = list(cast=as.numeric, offset=3, nrow=nr, ncol=final.year - first.year + 1)
    data.raw = extract.dp.tag(dp.raw, "<ARTByAge5YearAG MV>", data.fmt)
    dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age.5yr)),
                rep(strata.labels$age.5yr, length(strata.labels$sex)),
                data.frame(data.raw))
  } else {
    nr = length(strata.labels$sex) * length(strata.labels$age.gam)
    data.fmt = list(cast=as.numeric, offset=3, nrow=nr, ncol=final.year - first.year + 1)
    data.raw = extract.dp.tag(dp.raw, "<ARTByAgeGAMAG MV>", data.fmt)
    dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age.gam)),
                rep(strata.labels$age.gam, length(strata.labels$sex)),
                data.frame(data.raw))
  }
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

