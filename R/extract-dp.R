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
  val = extract.raw.tag(dp.raw, tag, fmt)
  if (is.null(val)) {
    val = matrix(NA, nrow=fmt$nrow, ncol=fmt$ncol)
  }
  return(val)
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

#' Get the model used to estimate incidence in a Spectrum projection
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Ignored; included for compatibility with similar functions.
#' @return The incidence model name as a factor (see "Details" below for factor levels)
#' @section Details:
#'
#'   Spectrum can take HIV incidence from several different models as inputs:
#'   \enumerate{
#'   \item{Direct - manually entered incidence values by year}
#'   \item{EPP - the Estimation and Projection Package}
#'   \item{AEM - the AIDS Epidemic Model}
#'   \item{CSAVR - the Case Surveillance and Vital Registration tool}
#'   \item{Mortality - incidence calibrated to mortality data}
#'   \item{ECDC - The European Centre for Disease Prevention and Control's HIV Modelling Tool}
#'   }
#'
#'   \code{dp.inputs.incidence.model()} returns the name of the model used as a factor. Use
#'   \code{dp.inputs.incidence()} to get the incidence trend itself.
#'
#' @export
dp.inputs.incidence.model = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  opt = extract.dp.tag(dp.raw, "<IncidenceOptions MV>", fmt)[1,1]
  return(factor(opt, levels=0:5, labels=strata.labels$incidence.model))
}

#' Get the initial distribution of newly-infected adults by CD4 cell count category
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Request "wide" (default) or "long" format data.
#' @return the initial CD4 category distribution for newly-infected adults
#' @export
dp.inputs.adult.initial.cd4 = function(dp.raw, direction="wide") {
  n.sex = length(strata.labels$sex)
  n.age = length(strata.labels$age.cd4.adult)
  n.cd4 = length(strata.labels$cd4.adult)

  fmt = list(cast=as.numeric, offset=3, nrow=n.sex, ncol=n.age * n.cd4)
  raw = extract.dp.tag(dp.raw, "<AdultDistNewInfectionsCD4 MV>", fmt)
  dat = cbind(rep(strata.labels$age.cd4.adult, each=n.cd4),
              rep(strata.labels$cd4.adult, n.age),
              data.frame(t(raw)))
  colnames(dat) = c("Age", "CD4", "Male", "Female")
  dat.long = reshape2::melt(dat, id.vars=c("Age", "CD4"), variable.name="Sex", value.name="Value")
  dat.long$CD4 = factor(dat$CD4, levels=strata.labels$cd4.adult)
  if (direction == "wide") {
    dat = reshape2::dcast(dat.long, Sex+CD4~Age, value.var="Value")
  } else {
    dat = dat.long[,c("Sex", "Age", "CD4", "Value")]
  }
  return(dat)
}

#' Get new HIV diagnoses overall
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Request "wide" (default) or "long" format data. #' @param
#'   first.year First year of the projection. If \code{first.year=NULL}, it will
#'   be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame of numbers of new diagnoses
#' @export
dp.inputs.csavr.diagnoses = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## .DP stores two rows for this modvar, but only uses rows 1
  fmt = list(cast=as.numeric, devoffset=2, nrow=2, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<CSAVRInputNewDiagnoses MV>", fmt)
  raw[raw==dp_not_avail] = NA

  if (direction == "long") {
    dat = cbind(Year=first.year:final.year, Value=raw[1,])
  } else {
    dat = data.frame(t(raw[1,]))
    colnames(dat) = sprintf("%d", first.year:final.year)
  }

  return(dat)
}

#' Get new HIV diagnoses by sex
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Request "wide" (default) or "long" format data. #' @param
#'   first.year First year of the projection. If \code{first.year=NULL}, it will
#'   be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame of numbers of new diagnoses by sex
#' @export
dp.inputs.csavr.diagnoses.sex = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n.sex = length(strata.labels$sex)

  ## .DP stores four rows for this modvar, but only uses rows 1 (males) and 3
  ## (females)
  fmt = list(cast=as.numeric, offset=2, nrow=2 * n.sex, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<CSAVRInputNewDiagnosesBySex MV>", fmt)
  raw[raw==dp_not_avail] = NA
  dat = cbind(strata.labels$sex,
              data.frame(raw)[c(1,3),])
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars="Sex", variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get new HIV diagnoses by sex and age
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Request "wide" (default) or "long" format data. #' @param
#'   first.year First year of the projection. If \code{first.year=NULL}, it will
#'   be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame of numbers of new diagnoses by age and sex
#' @export
dp.inputs.csavr.diagnoses.sex.age = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n.sex = length(strata.labels$sex)
  n.age = length(strata.labels$age.csavr)

  fmt = list(cast=as.numeric, offset=2, nrow=n.sex * n.age, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<CSAVRInputNewDiagnosesBySexAge MV>", fmt)
  raw[raw==dp_not_avail] = NA
  dat = cbind(rep(strata.labels$sex, each=n.age),
              rep(strata.labels$age.csavr, n.sex),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the adult HIV disease progression rates
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Request "wide" (default) or "long" format data.
#' @return adult HIV disease progression rates
#' @export
dp.inputs.adult.cd4.progression = function(dp.raw, direction="wide") {
  n.sex = length(strata.labels$sex)
  n.age = length(strata.labels$age.cd4.adult)
  n.cd4 = length(strata.labels$cd4.adult)

  fmt = list(cast=as.numeric, offset=3, nrow=n.sex, ncol=n.age * n.cd4)
  raw = extract.dp.tag(dp.raw, "<AdultAnnRateProgressLowerCD4 MV>", fmt)
  dat = cbind(rep(strata.labels$age.cd4.adult, each=n.cd4),
              rep(strata.labels$cd4.adult, n.age),
              data.frame(t(raw)))
  colnames(dat) = c("Age", "CD4", "Male", "Female")
  dat.long = reshape2::melt(dat, id.vars=c("Age", "CD4"), variable.name="Sex", value.name="Value")
  dat.long$CD4 = factor(dat$CD4, levels=strata.labels$cd4.adult)
  if (direction == "wide") {
    dat = reshape2::dcast(dat.long, Sex+CD4~Age, value.var="Value")
  } else {
    dat = dat.long[,c("Sex", "Age", "CD4", "Value")]
  }
  return(subset(dat, CD4 != "CD4<50")) # Progression rates for CD4<50 are not used
}

#' Get HIV-related mortality rates among adults not on ART
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Request "wide" (default) or "long" format data.
#' @return adult HIV-related mortality rates off ART
#' @export
dp.inputs.adult.hiv.mortality.off.art = function(dp.raw, direction="wide") {
  n.sex = length(strata.labels$sex)
  n.age = length(strata.labels$age.cd4.adult)
  n.cd4 = length(strata.labels$cd4.adult)

  fmt = list(cast=as.numeric, offset=3, nrow=n.sex, ncol=n.age * n.cd4)
  raw = extract.dp.tag(dp.raw, "<AdultMortByCD4NoART MV>", fmt)
  dat = cbind(rep(strata.labels$age.cd4.adult, each=n.cd4),
              rep(strata.labels$cd4.adult, n.age),
              data.frame(t(raw)))
  colnames(dat) = c("Age", "CD4", "Male", "Female")
  dat.long = reshape2::melt(dat, id.vars=c("Age", "CD4"), variable.name="Sex", value.name="Value")
  dat.long$CD4 = factor(dat$CD4, levels=strata.labels$cd4.adult)
  if (direction == "wide") {
    dat = reshape2::dcast(dat.long, Sex+CD4~Age, value.var="Value")
  } else {
    dat = dat.long[,c("Sex", "Age", "CD4", "Value")]
  }
  return(dat)
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
#' @section Details:
#'
#'   Adult treatment data can be entered into Spectrum as numbers or
#'   percentages. These units can vary from year to year. When
#'   \code{direction="wide"}, the return value will include rows for numbers and
#'   for percentages, with percentages missing in years where numbers were
#'   entered or vice-versa. When \code{direction="long"}, the data frame will only
#'   include rows for whichever unit was entered into Spectrum in any year.
#'
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
  colnames(dat) = c("Sex", "Unit", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Unit"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
    dat = subset(dat, is.finite(Value)) # Remove "NA" values
  }

  ## Drop Male+Female values because these are calculated by Spectrum, and only
  ## for years when inputs are entered as numbers
  return(subset(dat, Sex != "Male+Female"))
}

#' Get Spectrum child HIV treatment inputs
#'
#' Get input numbers or percentages of children on antiretroviral therapy (ART)
#' or cotrimoxazole (CTX) prophylaxis entered into Spectrum by year and age in
#' long or wide format
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
#'   Child treatment data can be entered into Spectrum as numbers or percentages
#'   for ages 0-14. Further, ART numbers can be entered by five-year age group
#'   (0-4, 5-9, or 10-14) instead. These units and age resolutions can vary from
#'   year to year. When \code{direction="wide"}, the return value will include
#'   rows for numbers and for percentages, with percentages missing in years
#'   where numbers were entered or vice-versa. When \code{direction="long"},
#'   unused combinations of age group, treatment, and unit are removed before
#'   returning.
#'
#' @export
dp.inputs.child.art = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## Read treatment coverage values (numbers and/or percentages)
  data.fmt = list(cast=as.numeric, offset=2, nrow=5, ncol=final.year - first.year + 1)
  data.raw = extract.dp.tag(dp.raw, "<ChildTreatInputs MV3>", data.fmt)

  ## Read treatment coverage units (0: number, 1: percent)
  unit.fmt = list(cast=as.numeric, offset=2, nrow=5, ncol=final.year - first.year + 1)
  unit.raw = extract.dp.tag(dp.raw, "<ChildARTByAgeGroupPerNum MV2>", data.fmt)

  ## Decode and annotate the data
  data.raw[data.raw==dp_not_avail] = NA
  num.raw = data.raw
  num.raw[unit.raw==1] = NA
  prc.raw = data.raw
  prc.raw[unit.raw==0] = NA

  dat = cbind(rep(c("0-14", "0-14", "0-4", "5-9", "10-14"), 2),
              rep(c("CTX", "ART", "ART", "ART", "ART"), 2),
              rep(c("Number", "Percent"), each=5),
              rbind(data.frame(num.raw),
                    data.frame(prc.raw)))
  colnames(dat) = c("Age", "Treatment", "Unit", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Age", "Treatment", "Unit"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
    dat = subset(dat, is.finite(Value)) # Remove "NA" values
  }

  return(dat)
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

#' Get Spectrum input HIV incidence
#'
#' Get the input HIV incidence trend. This may pertain to either the 15-49 or
#' 15+ adult age group.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.inputs.incidence = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  opt.fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  opt.val = extract.dp.tag(dp.raw, "<IncidenceOptions MV>", opt.fmt)[1,1]

  age.fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  age.val = extract.dp.tag(dp.raw, "<EppAgeRange MV>", age.fmt)[1,1]

  inc.fmt = list(cast=as.numeric, offset=2, nrow=6, ncol=final.year - first.year + 1)
  inc.raw = extract.dp.tag(dp.raw, "<IncidenceByFit MV4>", inc.fmt)
  colnames(inc.raw) = sprintf("%d", first.year:final.year)
  inc.val = inc.raw[opt.val + 1,] # offset by +1 because options use 0-based while R uses 1-based indexing

  ages = ifelse(age.val==80, "15+", sprintf("15-%d", age.val))
  dat = cbind(Age=ages, as.data.frame(t(inc.val)))

  if (direction=="long") {
    dat = data.frame(Year=first.year:final.year, Age=ages, Value=inc.val)
  }

  return(dat)
}

#' Get Spectrum input HIV incidence rate ratios by sex
#'
#' Get the input ratio of female to male incidence.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.inputs.irr.sex = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=1, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<HIVSexRatio MV>", fmt)

  if (direction=="long") {
    dat = data.frame(Year=first.year:final.year, Value=raw[1,])
  } else {
    colnames(raw) = sprintf("%d", first.year:final.year)
    dat = data.frame(raw, check.names=FALSE)
  }

  return(dat)
}

#' Get Spectrum input HIV incidence rate ratios by age
#'
#' Get the input rate ratios of incidence by age relative to ages 25-29,
#' stratified by sex.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @export
dp.inputs.irr.age = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  nr = length(strata.labels$sex) * length(strata.labels$age.5yr)
  fmt = list(cast=as.numeric, offset=3, nrow=nr, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<DistOfHIV MV2>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age.5yr)),
              rep(strata.labels$age.5yr, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get numbers of adults on ART by month during 2020-2021
#'
#' Get the number of adults on ART during 2020-2021, stratified by sex.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @return A data frame.
#' @export
dp.inputs.adult.art.monthly = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=4, ncol=length(strata.labels$month)+1)
  raw = extract.dp.tag(dp.raw, "<AdultARTByMonth MV>", fmt)
  dat = cbind(rep(strata.labels$sex, each=2), rep(2020:2021, 2), data.frame(raw[,2:13]))
  colnames(dat) = c("Sex", "Year", strata.labels$month)
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Year"), variable.name="Month", value.name="Value")
  }
  return(dat)
}

#' Get adult ART loss to follow-up inputs by month during 2020-2021
#'
#' Get the input monthly percentages of adults who were lost to ART follow-up during 2020-2021.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @return A data frame.
#' @export
dp.inputs.adult.ltfu.monthly = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=2, ncol=length(strata.labels$month)+1)
  raw = extract.dp.tag(dp.raw, "<AdultPercentLTFUByMonth MV>", fmt)
  dat = cbind(2020:2021, data.frame(raw[,2:13]))
  colnames(dat) = c("Year", strata.labels$month)
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Year"), variable.name="Month", value.name="Value")
  }
  return(dat)
}
