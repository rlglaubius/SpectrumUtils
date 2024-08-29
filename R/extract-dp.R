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

#' Helper function for extracting a single row of data with one value per year
dp.extract.time.series = function(dp.raw, direction="wide", first.year, final.year, tag, offset) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}
  fmt = list(cast=as.numeric, offset=offset, nrow=1, ncol = final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, tag, fmt)
  if (direction=="long") {
    dat = data.frame(Year = first.year:final.year, Value=raw[1,])
  } else {
    dat = data.frame(raw)
    colnames(dat) = sprintf("%d", first.year:final.year)
  }
  return(dat)
}

#' Check whether the projection was valid when last saved
#'
#' @inheritParams dp.inputs.first.year
#' @return TRUE if the projection was valid, FALSE otherwise
#' @export
dp.status.projection.valid = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<ProjectionValid MV2>", fmt)[1,1] == 1)
}

#' Spectrum projection time span
#'
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Ignored; included for compatibility with similar functions.
#' @return the requested year
#' @describeIn dp.inputs.first.year First year of the projection
#' @export
dp.inputs.first.year = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<FirstYear MV2>", fmt)[1,1])
}

#' @describeIn dp.inputs.first.year Final year of the projection
#' @export
dp.inputs.final.year = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<FinalYear MV2>", fmt)[1,1])
}

#' Spectrum demographic inputs
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{dp.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{dp.inputs.final.year()}
#' @return A data frame.
#' @describeIn dp.inputs.tfr Total fertility rate by year.
#' @export
dp.inputs.tfr = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<TFR MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' @describeIn dp.inputs.tfr Proportionate age-specific fertility rates, specified as percentages (0 <= x <= 100).
#' @export
dp.inputs.pasfr = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=7, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<ASFR MV>", fmt)
  dat = cbind(Age=strata.labels$age.fert, data.frame(raw))
  colnames(dat) = c("Age", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.tfr Net numbers of migrants by sex and year.
#' @export
dp.inputs.migr.rate = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}
  fmt = list(cast=as.numeric, offset=3, nrow=4, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<MigrRate MV2>", fmt)[c(2,4),] # non-standard layout
  dat = cbind(strata.labels$sex, data.frame(raw))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.tfr Age distribution of net migrants by sex and year,
#'   normalized to sum to 100. Negative values indicate net flows of migrants
#'   that are in the opposite direction of overall net migration.
#' @export
dp.inputs.migr.dist = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}
  fmt = list(cast=as.numeric, offset=3, nrow=length(strata.labels$sex)*length(strata.labels$age.5yr), ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<MigrAgeDist MV2>", fmt)
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

#' @describeIn dp.inputs.tfr Sex ratio at birth, expressed as the number of male
#'   births per 100 female births.
#' @export
dp.inputs.srb = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<SexBirthRatio MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get the input life table selection
#'
#' Spectrum users may select a life table from a list of model life tables, or use a
#' country-specific life table based on the latest World Population Prospects
#' revision. This function indicates which selection the user made.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @return The life table name as a factor
#' @section Details:
#'
#'   \code{dp.inputs.life.table} just indicates which life table was selected.
#'   Life table indicators can be accessed separately: \code{dp.inputs.surv}
#'   returns survival ratios, and \code{dp.inputs.e0} returns life expectancy at
#'   birth. To keep projection file sizes small, most other common life table
#'   indicators are not saved.
#'
#'   Supported life tables include:
#'   \enumerate{
#'   \item{Coale-Demeny West}
#'   \item{Coale-Demeny North}
#'   \item{Coale-Demeny East}
#'   \item{Coale-Demeny South}
#'   \item{UN General}
#'   \item{UN Chile}
#'   \item{UN South Asia}
#'   \item{UN East Asia}
#'   \item{Country-specific (usually based on the latest WPP revision)}
#'   \item{Custom}
#'   }
#'
#' @export
dp.inputs.life.table = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=3, nrow=1, ncol=1)
  opt = extract.dp.tag(dp.raw, "<LifeTableNum MV2>", fmt)[1,1]
  return(factor(opt, levels=1:11, labels=strata.labels$life.table))
}

#' @describeIn dp.inputs.tfr Survival rates (Sx) by age, sex, and year. This is
#'   expressed as the proportion of people alive and age x at the start of the
#'   year who are still alive at the end of that year.
#' @export
dp.inputs.surv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}
  ages = c("Birth", strata.labels$age)
  fmt = list(cast=as.numeric, offset=3, nrow=164, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<SurvRate MV2>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(ages)),
              rep(ages, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.tfr Input life expectancy at birth (e0) by year and sex.
#' @export
dp.inputs.e0 = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}
  ages = c("Birth", strata.labels$age)
  n.sex = length(strata.labels$sex)
  fmt = list(cast=as.numeric, offset=3, nrow=n.sex, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<LE MV2>", fmt)
  dat = cbind(strata.labels$sex, data.frame(raw))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the default input life expectancy at birth
#'
#' Get the default input life expectancy at birth (e0) by year and sex. These
#' life expectancy trends typically come from the latest UN Population Division
#' World Population Prospects revision. These may differ from the user-editable
#' life expectancy used in Spectrum calculation, which can be accessed using
#' \code{dp.inputs.e0}.
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.e0.default = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}
  ages = c("Birth", strata.labels$age)
  n.sex = length(strata.labels$sex)
  fmt = list(cast=as.numeric, offset=4, nrow=n.sex, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<DefaultUPDLE MV>", fmt)
  dat = cbind(strata.labels$sex, data.frame(raw))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
    return(dat)
}

#' Get the external population inputs used to calculate population adjustments
#'
#' Spectrum users may enter externally-produced population targets by sex, age,
#' and year that can be used to align Spectrum population outputs with a
#' reference population projection. \code{dp.inputs.external.pop} returns those
#' population targets in long or wide format.
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @section Details:
#'
#'   Use of an external population file is optional and rare in Spectrum. Use
#'   \code{dp.inputs.use.external.pop} to check whether to check whether an
#'   external population was specified. Note that \code{dp.inputs.external.pop}
#'   will return population sizes of 0 for all years, sexes and ages if no
#'   external population was specified.
#'
#' @export
dp.inputs.external.pop = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ages = c(strata.labels$age, "All Ages")

  fmt = list(cast=as.numeric, offset=3, nrow=164, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<RegionalAdjustPopData MV2>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(ages)),
              rep(ages, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Check whether an external population was specified
#'
#' Spectrum users may enter externally-produced population targets by sex, age,
#' and year that can be used to align Spectrum population outputs with a
#' reference population projection. \code{dp.inputs.use.external.pop} indicates
#' whether this mechanism was used.
#' @inheritParams dp.inputs.first.year
#' @return A list with two elements. Element \code{use.external.pop} is TRUE if
#' an external population was used, FALSE otherwise. Element \code{final.year}
#' specifies the final year of adjustments.
#' @section Details:
#'
#'   Use of an external population file is optional and rare in Spectrum.
#'   Projections that use this mechanism may specify that adjustments to match
#'   external population targets are not done in every year, but only until a
#'   specified final year. Use \code{dp.inputs.external.pop} to access the
#'   external population sizes.
#'
#' @export
dp.inputs.use.external.pop = function(dp.raw, direction="wide") {
  rval = list()

  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  raw = extract.dp.tag(dp.raw, "<RegionalAdjustPopCBState MV>", fmt)
  rval$use.external.pop = (raw[1,1] == 1)

  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  raw = extract.dp.tag(dp.raw, "<CustomPopStopRescalingYear MV>", fmt)
  rval$final.year = raw[1,1]

  return(rval)
}

#' Get the national population entered in a subnational projection
#' @inheritParams dp.inputs.tfr
#' @return population sizes by year.
#' @section Details:
#'
#'   NOTE: THIS INPUT PERTAINS TO A FEATURE THAT HAS BEEN REMOVED FROM SPECTRUM.
#'   Countries preparing subnational projections could specify the subnational
#'   population by entering a national population size trend, and the share of
#'   the national population living in the subnational region.
#'
#'   Use \code{dp.inputs.pop.percent} to extract the share of the national
#'   population living in the subnational region.
#'
#' @export
dp.inputs.pop.country = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<CountryProjBigPop MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get the percentage of the national population living in a subnational region
#' @inheritParams dp.inputs.tfr
#' @return population sizes by year.
#' @section Details:
#'
#'   NOTE: THIS INPUT PERTAINS TO A FEATURE THAT HAS BEEN REMOVED FROM SPECTRUM.
#'   Countries preparing subnational projections could specify the subnational
#'   population by entering a national population size trend, and the share of
#'   the national population living in the subnational region.
#'
#'   Use \code{dp.inputs.pop.country} to extract the national
#'   population of the country the subnational projection resides in.
#'
#' @export
dp.inputs.pop.percent = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<PercentOfPop MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get data on HIV testing during antenatal care
#'
#' Extract Spectrum inputs on antenatal clinic (ANC) attendance and HIV testing
#' @inheritParams dp.inputs.tfr
#' @return a data frame.
#' @section Details:
#'
#' Spectrum users may enter validation data on HIV testing at ANC in their
#' Spectrum files. These consist of:
#'
#' \enumerate{
#' \item{Program reported births: Live births reported to the national program}
#' \item{First ANC visits: number of women with a first ANC visit during their current pregnancy}
#' \item{Tested: number of women who received at least one HIV test at ANC}
#' \item{Tested HIV+: number of women who tested HIV+ at the first test of their current pregnancy}
#' \item{Known HIV+: number of women whose HIV-positive status was known at their first ANC visit}
#' \item{Known HIV-: number of women whose HIV-negative status was known at their first ANC visit}
#' \item{ANC HIV\%: HIV prevalence at ANC. Calculated as (Tested HIV+ + Known HIV+) / (Tested + Known HIV+)}
#' \item{Retested: number of women who were tested for HIV at least once after their first HIV test during their current pregnancy}
#' \item{Retested HIV+: number of women who tested HIV+ during retesting}
#' }
#'
#' Some of these data were not collected in earlier versions of Spectrum and are
#' not available in projections last saved in those versions.
#'
#' @export
dp.inputs.anc.testing = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## Note: versions 1 and 3 currently are not supported by this package
  tag_v2 = "<ANCTestingValues MV2>"
  tag_v4 = "<ANCTestingValues MV4>"
  if (tag_v2 %in% dp.raw$Tag) {
    rnames = c("First ANC visits", "Tested", "Tested HIV+", "Known HIV+", "ANC HIV%", "Retested", "Retested HIV+")
    dat = dp.inputs.anc.testing.helper(dp.raw, tag_v2, rnames, first.year, final.year)
  } else {
    rnames = c("First ANC visits", "Tested", "Tested HIV+", "Known HIV+", "ANC HIV%", "Retested", "Retested HIV+", "Program births", "Known HIV-")
    dat = dp.inputs.anc.testing.helper(dp.raw, tag_v4, rnames, first.year, final.year)
  }

  colnames(dat) = c("Indicator", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars="Indicator", variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

dp.inputs.anc.testing.helper = function(dp.raw, tag, rnames, first.year, final.year) {
  fmt = list(cast=as.numeric, offset=2, nrow=length(rnames), ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, tag, fmt)
  raw[raw == dp_not_avail] = NA
  dat = cbind(rnames, data.frame(raw))
}

#' Extract HIV testing program data
#' @inheritParams dp.inputs.tfr
#' @return a data frame
#' @export
dp.inputs.hiv.testing = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ind_name = c("Total diagnostic tests", "Total positive tests",
               "Total HTS tests",        "Total positive HTS tests",
               "Total ANC tests",        "Total positive ANC tests",
               "Total self-tests",
               "Total index partner tests")
  years = sprintf("%d", first.year:final.year)
  fmt = list(cast=as.numeric, offset=2, nrow=8, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<HIVTesting MV>", fmt)
  raw[raw==dp_not_avail] = NA
  dat = cbind(Indicator=ind_name, data.frame(raw))
  colnames(dat) = c("Indicator", years)
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Indicator"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the source indicated for number who know their HIV+ status
#' @inheritParams dp.inputs.first.year
#' @return The knowledge of status (KoS) source as a factor (see "Details" below
#'   for factor levels)
#' @section Details:
#'
#'   Spectrum users can choose one of five sources for numbers of people living
#'   with HIV who know their HIV status:
#'   \enumerate{
#'   \item{Case reports}
#'   \item{Shiny90}
#'   \item{CSAVR - the Case Surveillance and Vital Registration tool}
#'   \item{ECDC - The European Centre for Disease Prevention and Control's HIV Modelling Tool}
#'   \item{Direct - manually entered values by year}
#'   }
#'
#' Note that users may change this selection after entering knowledge of status data into Spectrum.
#' \code{dp.inputs.kos.data()} returns the input numbers of people who know their status.
#'
#' @export
dp.inputs.kos.source = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  opt = extract.dp.tag(dp.raw, "<KnowledgeOfStatusInputType MV2>", fmt)[1,1]
  return(factor(opt, levels=0:4, labels=strata.labels$kos.source))
}

#' Get input numbers of people living with HIV who know their HIV+ status
#' @inheritParams dp.inputs.tfr
#' @return a data frame of numbers who know their status.
#' @export
dp.inputs.kos.data = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  tag_v3 = "<KnowledgeOfStatusInput MV3>"
  tag_v4 = "<KnowledgeOfStatusInput MV4>"
  if (length(grep(tag_v3, dp.raw$Tag)) > 0) {
    ## V3 tag only stored values from 2010 onward
    first.year = max(first.year, 2010) # input editor starts in 2010 if the project starts earlier
    fmt = list(cast=as.numeric, offset=2, nrow=3, ncol=final.year - first.year + 2)
    raw = extract.dp.tag(dp.raw, tag_v3, fmt)
    raw = raw[,2:ncol(raw)] # first column of KoS inputs is intentionally blank in .DP
  } else {
    ## V4 tag stores values for all years and does not have a blank first column
    fmt = list(cast=as.numeric, offset=2, nrow=3, ncol=final.year - first.year + 2)
    raw = extract.dp.tag(dp.raw, tag_v4, fmt)
  }

  raw[raw==dp_not_avail] = NA
  yrs = sprintf("%d", first.year:final.year)
  dat = cbind(c("Children 0-14", "Males 15+", "Females 15+"), data.frame(raw))
  colnames(dat) = c("Population", yrs)
  if (direction == "long") {
    dat = reshape2::melt(dat,
                         id.vars=c("Population"),
                         measure.vars=yrs,
                         variable.name="Year",
                         value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the model used to estimate incidence in a Spectrum projection
#' @inheritParams dp.inputs.first.year
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

#' Get the epidemic type specified in EPP
#' @inheritParams dp.inputs.first.year
#' @return "GENERALIZED" or "CONCENTRATED"
#' @export
dp.inputs.epp.epidemic.type = function(dp.raw, direction="wide") {
  fmt = list(cast=as.character, offset=2, nrow=1, ncol=1)
  val = extract.dp.tag(dp.raw, "<EpidemicTypeFromEPP MV>", fmt)[1,1]
  return(val)
}

#' Get the first year of HIV incidence estimated by EPP
#' @inheritParams dp.inputs.first.year
#' @return an integer-valued year
#' @export
dp.inputs.epp.epidemic.first.year = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  val = extract.dp.tag(dp.raw, "<FirstYearOfEpidemic MV>", fmt)[1,1]
  return(as.integer(val))
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

#' Get the model used to estimate incidence in CSAVR
#' @inheritParams dp.inputs.first.year
#' @return The incidence model name as a factor (see "Details" below for factor
#'   levels)
#' @section Details:
#'
#'   CSAVR users select one of four curve types to model incidence:
#'   \enumerate{
#'   \item{None - no curve selected}
#'   \item{Single logistic}
#'   \item{Double logistic}
#'   \item{Splines}
#'   \item{rLogistic}
#'   }
#'
#'   Use \code{dp.inputs.incidence.model()} to check if CSAVR was used to
#'   estimate incidence; use \code{dp.inputs.incidence()} to get the incidence
#'   estimate itself.
#'
#' @export
dp.inputs.csavr.model = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  opt = extract.dp.tag(dp.raw, "<FitIncidenceTypeOfFit MV2>", fmt)[1,1]
  return(factor(opt, levels=0:6, labels=strata.labels$csavr.model))
}

#' Check which data were selected for CSAVR incidence estimation
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp}
#' @param direction Request "wide" (default) or "long" format data.
#' @return The incidence model name as a factor (see "Details" below for factor
#'   levels)
#' @section Details:
#'
#'   The CSAVR fitting tool can estimate incidence using three different kinds
#'   of data
#'   \enumerate{
#'   \item{New HIV diagnoses}
#'   \item{HIV-related deaths}
#'   \item{CD4 cell counts at diagnosis (as of 2022)}
#'   }
#'
#'   Use \code{dp.inputs.incidence.model()} to check if CSAVR was used to
#'   estimate incidence; use \code{dp.inputs.incidence()} to get the incidence
#'   estimate itself.
#'
#'   Since CD4 counts at diagnoses were reintroduced in Spectrum for the 2022
#'   round of UNAIDS HIV estimates, this function may not work correctly with
#'   files produced with older versions of Spectrum.
#'
#' @export
dp.inputs.csavr.data.options = function(dp.raw, direction="wide") {
  ## CSAVRFitOptions has an unusual format. The "Data" column entry is blank,
  ## and the first non-blank entry is for DP_PLHIV, which is not a valid option for
  ## fitting.
  fmt = list(cast=function(x) {as.logical(as.integer(x))}, offset=2, nrow=1, ncol=5)
  raw = extract.dp.tag(dp.raw, "<CSAVRFitOptions MV3>", fmt)
  dat = data.frame(raw[,3:5,drop=FALSE])
  colnames(dat) = c("New HIV diagnoses", "AIDS deaths", "CD4 distribution")
  if (direction == "long") {
    dat = reshape2::melt(data.opt, measure.vars=colnames(data.opt), variable.name="Indicator", value.name="Value")
  }
  return(dat)
}

#' Check if incidence rate ratios (IRRs) by sex or age were estimated while fitting CSAVR
#' @inheritParams dp.inputs.first.year
#' @return A data frame of TRUE/FALSE variables indicating whether IRR fitting by sex or
#' age was enabled. These options are selected separately for each CSAVR incidence model.
#' @section Limitations:
#'
#'   Users may change IRR settings in Spectrum after they have fitted an incidence model. As
#'   a result, the IRR option indicators may not always correctly indicate whether IRRs were
#'   estimated during fitting.
#'
#' @export
dp.inputs.csavr.irr.options = function(dp.raw, direction="wide") {
  tag_found = grep("CSAVRAdjustIRRs", dp.raw$Tag, value=TRUE)
  tag_v3 = "<CSAVRAdjustIRRs MV3>"
  tag_v4 = "<CSAVRAdjustIRRs MV4>"

  if (tag_found == tag_v4) {
    tag = tag_v4
    n_models = 6
    csavr_models = strata.labels$csavr.model[2:7]
  } else if (tag_found == tag_v3) {
    tag = tag_v3
    n_models = 4
    csavr_models = strata.labels$csavr.model[2:5]
  }

  # two-step cast needed because as.logical("1") = NA, but as.logical(as.integer("1")) = TRUE
  fmt = list(cast=function(x) {as.logical(as.integer(x))}, offset=2, nrow=n_models, ncol=2)
  raw = extract.dp.tag(dp.raw, tag, fmt)
  dat = cbind(csavr_models, data.frame(raw))
  colnames(dat) = c("Model", "Sex", "Age")
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars="Model", variable.name="IRR", value.name="Value")
  }
  return(dat)
}

#' Get input numbers of overall new HIV diagnoses
#' @inheritParams dp.inputs.tfr
#' @return A data frame of numbers of new diagnoses
#' @export
dp.inputs.csavr.diagnoses = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## .DP stores two rows for this modvar, but only uses rows 1
  fmt = list(cast=as.numeric, offset=2, nrow=2, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<CSAVRInputNewDiagnoses MV>", fmt)
  raw[raw==dp_not_avail] = NA

  if (direction == "long") {
    dat = data.frame(Year=first.year:final.year, Value=raw[1,])
  } else {
    dat = data.frame(t(raw[1,]))
    colnames(dat) = sprintf("%d", first.year:final.year)
  }

  return(dat)
}

#' Get input numbers of new HIV diagnoses by sex
#' @inheritParams dp.inputs.tfr
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

#' Get input numbers of new HIV diagnoses by sex and age
#' @inheritParams dp.inputs.tfr
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

#' Get input numbers of new HIV diagnoses by CD4 category
#' @inheritParams dp.inputs.tfr
#' @return A data frame of numbers of new diagnoses by CD4 cell category
#' @export
dp.inputs.csavr.diagnoses.cd4 = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n.cd4 = length(strata.labels$cd4.csavr)

  fmt = list(cast=as.numeric, offset=2, nrow=n.cd4, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<CSAVRInputCD4DistAtDiag MV>", fmt)
  raw[raw==dp_not_avail] = NA
  dat = cbind(rev(strata.labels$cd4.csavr), data.frame(raw))
  colnames(dat) = c("CD4", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("CD4"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get input numbers of AIDS deaths used by CSAVR to estimate HIV incidence
#'
#' Spectrum's Case Surveillance and Vital Registration (CSAVR) tool uses case
#' surveillance data on new HIV diagnoses and vital registration data on AIDS
#' deaths in adults to estimate HIV incidence. AIDS deaths can be entered
#' overall, by sex, or by sex and age. Countries may use AIDS deaths data from
#' their vital registration system or AIDS deaths estimates produced by the
#' Institute of Health Metrics and Evaluation (IHME) in CSAVR. Current Spectrum
#' versions allow users to enter data from up to three different sources, then
#' select which source would be used for fitting. Older versions of Spectrum
#' only allowed users to enter one source of data.
#' @inheritParams dp.inputs.tfr
#' @return a data frame
#' @describeIn dp.inputs.csavr.deaths.source The data source selected for use in
#'   CSAVR.
#' @export
dp.inputs.csavr.deaths.source = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  raw = extract.dp.tag(dp.raw, "<CSAVRInputAIDSDeathsSource MV>", fmt)
  return(strata.labels$csavr.source[raw + 1])
}

#' @describeIn dp.inputs.csavr.deaths.source User-specified name for each source of AIDS deaths data entered into CSAVR.
#' @export
dp.inputs.csavr.deaths.source.names = function(dp.raw, direction="wide") {
  fmt = list(cast=as.character, offset=2, nrow=1, ncol=length(strata.labels$csavr.source))
  raw = extract.dp.tag(dp.raw, "<CSAVRInputAIDSDeathsSourceName MV>", fmt)
  return(data.frame(Source = strata.labels$csavr.source, Name = raw[1,]))
}

#' @describeIn dp.inputs.csavr.deaths.source Adult AIDS deaths by year and data source.
#' @export
dp.inputs.csavr.deaths = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  tag_v1 = "<CSAVRInputAIDSDeaths MV>"
  tag_v2 = "<CSAVRInputAIDSDeaths MV2>"

  n_src = length(strata.labels$csavr.source)

  ## CSAVR has two rows of deaths data, one for the number of deaths, one for
  ## reporting completeness. Spectrum does not display or use the completeness
  ## input, so we have not extracted it here.
  if (tag_v1 %in% dp.raw$Tag) {
    fmt = list(cast=as.numeric, offset=2, nrow=2, ncol=final.year - first.year + 1)
    raw = extract.dp.tag(dp.raw, tag_v1, fmt)[1,]
    raw = c(NA, raw)
    dat = data.frame(t(raw))
  } else {
    ## Version 2 was introduced to allow countries to enter up to three
    ## different streams of VR data
    fmt = list(cast=as.numeric, offset=2, nrow=2 * n_src, ncol=final.year - first.year + 1)
    raw = extract.dp.tag(dp.raw, tag_v2, fmt)[c(1,3,5),]
    dat = cbind(strata.labels$csavr.source, data.frame(raw))
    # dat = data.frame(raw)
  }
  dat[dat==dp_not_avail] = NA
  colnames(dat) = c("Source", first.year:final.year)

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars="Source", variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  if (tag_v1 %in% dp.raw$Tag) {
    dat$Source = NULL
  }

  return(dat)
}

#' @describeIn dp.inputs.csavr.deaths.source Adult AIDS deaths by year, sex, and data source.
#' @export
dp.inputs.csavr.deaths.sex = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  tag_v1 = "<CSAVRInputAIDSDeathsBySex MV>"
  tag_v2 = "<CSAVRInputAIDSDeathsBySex MV2>"

  n_sex = length(strata.labels$sex)
  n_src = length(strata.labels$csavr.source)

  ## Spectrum stores time series of deaths for each sex (MV1) or each sex and
  ## data source (MV2). Each time series includes two rows: the deaths data, and
  ## a % completeness. The latter is not displayed or used by Spectrum, so we
  ## do not extract it.
  if (tag_v1 %in% dp.raw$Tag) {
    fmt = list(cast=as.numeric, offset=2, nrow=2 * n_sex, ncol=final.year - first.year + 1)
    raw = extract.dp.tag(dp.raw, tag_v1, fmt)[seq.int(1,3,2),]
    dat = cbind(strata.labels$sex, NA, data.frame(raw))
  } else {
    fmt = list(cast=as.numeric, offset=2, nrow=2 * n_sex * n_src, ncol=final.year - first.year + 1)
    raw = extract.dp.tag(dp.raw, tag_v2, fmt)[seq.int(1,12,2),]
    dat = cbind(expand.grid(Sex=strata.labels$sex, Source=strata.labels$csavr.source), data.frame(raw))
  }
  dat[dat==dp_not_avail] = NA
  colnames(dat) = c("Sex", "Source", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Source"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  if (tag_v1 %in% dp.raw$Tag) {
    dat$Source = NULL
  }

  return(dat)
}

#' @describeIn dp.inputs.csavr.deaths.source Adult AIDS deaths by year, sex, age, and data source.
#' @export
dp.inputs.csavr.deaths.sex.age = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  tag_v1 = "<CSAVRInputAIDSDeathsBySexAge MV>"
  tag_v2 = "<CSAVRInputAIDSDeathsBySexAge MV2>"

  n_sex = length(strata.labels$sex)
  n_age = length(strata.labels$age.csavr)
  n_src = length(strata.labels$csavr.source)

  if (tag_v1 %in% dp.raw$Tag) {
    fmt = list(cast=as.numeric, offset=2, nrow=n_sex * n_age, ncol=final.year - first.year + 1)
    raw = extract.dp.tag(dp.raw, tag_v1, fmt)
    dat = cbind(expand.grid(Age=strata.labels$age.csavr, Sex=strata.labels$sex), NA, data.frame(raw))
  } else {
    fmt = list(cast=as.numeric, offset=2, nrow=n_sex * n_age * n_src, ncol=final.year - first.year + 1)
    raw = extract.dp.tag(dp.raw, tag_v2, fmt)
    dat = cbind(expand.grid(Age=strata.labels$age.csavr, Sex=strata.labels$sex, Source=strata.labels$csavr.source), data.frame(raw))
  }
  dat[dat==dp_not_avail] = NA
  colnames(dat) = c("Age", "Sex", "Source", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age", "Source"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  if (tag_v1 %in% dp.raw$Tag) {
    dat$Source = NULL
  }

  return(dat)
}

#' Get new diagnoses among in-migrants
#'
#' Get input numbers of new diagnoses among in-migrants entered into CSAVR by
#' sex, age, and year
#' @inheritParams dp.inputs.tfr
#' @return A data frame
#' @export
dp.inputs.csavr.migr.diagnoses = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n.sex = length(strata.labels$sex)
  n.age = length(strata.labels$age.5yr)

  fmt = list(cast=as.numeric, offset=3, nrow=n.sex * n.age, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<NetMigrationPLHIV MV>", fmt)
  dat = cbind(rep(strata.labels$sex, each=n.age),
              rep(strata.labels$age.5yr, n.sex),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get inputs numbers of in-migrants living with HIV
#'
#' Get input numbers of in-migrants living with HIV entered by sex, age, and
#' year
#' @inheritParams dp.inputs.tfr
#' @return A data frame
#' @export
dp.inputs.migr.plhiv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n.sex = length(strata.labels$sex)
  n.age = length(strata.labels$age.5yr)

  fmt = list(cast=as.numeric, offset=3, nrow=n.sex * n.age, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<HIVMigrantsByAgeSex MV>", fmt)
  dat = cbind(rep(strata.labels$sex, each=n.age),
              rep(strata.labels$age.5yr, n.sex),
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

#' HIV-related mortality rates among adults on ART
#'
#' HIV-related mortality rate inputs to Spectrum are stratified by age, sex, CD4
#' category, time on ART, and calendar year. Spectrum users may adjust a
#' separate on-ART mortality scale factor to calibrate deaths on ART to vital
#' registration data. Baseline rates by age, sex, CD4 and ART duration are
#' accessed separately from mortality rate ratios over time and scale factors.
#' Functions to access each of these are described below.
#' @inheritParams dp.inputs.tfr
#' @describeIn dp.inputs.adult.hiv.mortality.art Baseline HIV-related mortality
#'   rates on ART by sex, age, CD4 category, and time on ART.
#' @return \code{dp.inputs.adult.hiv.mortality.art} returns a data frame.
#' @export
dp.inputs.adult.hiv.mortality.art = function(dp.raw, direction="wide") {
  n.sex = length(strata.labels$sex)
  n.age = length(strata.labels$age.cd4.adult)
  n.cd4 = length(strata.labels$cd4.adult)
  n.art = length(strata.labels$art.dur)

  tags = c("<AdultMortByCD4WithART0to6 MV2>", "<AdultMortByCD4WithART7to12 MV2>", "<AdultMortByCD4WithARTGt12 MV2>")

  fmt = list(cast=as.numeric, offset=2, nrow=n.sex, ncol=n.age*n.cd4)
  raw_list = lapply(tags, function(tag) {extract.dp.tag(dp.raw, tag, fmt)})
  dat_list = lapply(raw_list, function(raw) {
    dat = cbind(rep(strata.labels$age.cd4.adult, each=n.cd4),
                rep(strata.labels$cd4.adult, n.age),
                data.frame(t(raw)))
    colnames(dat) = c("Age", "CD4", strata.labels$sex)
    dat = reshape2::melt(dat, id.vars=c("Age", "CD4"), variable.name="Sex", value.name="Value")
    dat$CD4 = factor(dat$CD4, levels=strata.labels$cd4.adult)
    return(dat)
  })
  names(dat_list) = strata.labels$art.dur
  dat_flat = dplyr::bind_rows(dat_list, .id="ART")

  if (direction == "wide") {
    dat = reshape2::dcast(dat_flat, Sex+CD4+ART~Age, value.var="Value")
  } else {
    dat = dat_flat
  }
  return(dat)
}

#' @describeIn dp.inputs.adult.hiv.mortality.art On-ART mortality rate ratios by year and ART duration.
#' @return \code{dp.inputs.adult.hiv.mortality.art.trend} returns a data frame.
#' @export
dp.inputs.adult.hiv.mortality.art.trend = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=2, nrow=2, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<MortalityRates MV2>", fmt)
  dat = cbind(strata.labels$art.dur.agg, data.frame(raw))
  colnames(dat) = c("ART", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("ART"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.adult.hiv.mortality.art Scale factor applied to on-ART mortality rates.
#' @return \code{dp.inputs.adult.hiv.mortality.art.scale} returns scale factor.
#' @export
dp.inputs.adult.hiv.mortality.art.scale = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<MortalityRatesMultiplier MV>", fmt)[1,1])
}

#' Get the effect of ART on HIV transmission
#'
#' Return the reduction in HIV transmission on ART. Spectrum does not use this
#' in its calculations, but passes it to EPP for use in incidence estimation.
#' @inheritParams dp.inputs.first.year
#' @return a number
#' @export
dp.inputs.art.transmission.reduction = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<AdultInfectReduc MV>", fmt)[1,1])
}

#' Get Spectrum's calculated number of births
#'
#' Get Spectrum's calculated number of births by year in long or wide format.
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.output.births = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<Births MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get Spectrum's calculated population
#'
#' Get Spectrum's calculated population by age, sex, and year in long or wide
#' format
#' @inheritParams dp.inputs.tfr
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


#' Get Spectrum's calculated infant mortality rate
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.output.imr = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=length(strata.labels$sex.aug), ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<IMR MV2>", fmt)
  dat = cbind(strata.labels$sex.aug[c(2,3,1)], data.frame(raw))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get Spectrum's calculated HIV-positive population
#'
#' Get Spectrum's calculated HIV-positive population by age, sex, and year in long or wide
#' format
#' @inheritParams dp.inputs.tfr
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
#' @inheritParams dp.inputs.tfr
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


#' Get Spectrum's calculated number in need of ART
#'
#' Get Spectrum's calculated number in need of ART by year, sex, and five-year age group
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.output.art.need = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  age.labels = c("All ages", strata.labels$age.5yr)
  sex.labels = strata.labels$sex.aug
  n.age = length(age.labels)
  n.sex = length(sex.labels)
  fmt = list(cast=as.numeric, offset=2, nrow=n.age*n.sex, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<NeedART MV>", fmt)
  dat = cbind(rep(sex.labels, length(age.labels)),
              rep(age.labels, each=length(sex.labels)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get Spectrum PMTCT outputs
#' @describeIn dp.output.pmtct Number of pregnant women receiving PMTCT
#' @inheritParams dp.inputs.tfr
#' @return A data frame
#' @export
dp.output.pmtct = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<ChildOnPMTCT MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' @describeIn dp.output.pmtct Number of pregnant women who need PMTCT
#' @export
dp.output.pmtct.need = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<ChildNeedPMTCT MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get Spectrum's calculated new HIV infections
#'
#' Get Spectrum's calculated new HIV infections by age, sex, and year in long or
#' wide format
#' @inheritParams dp.inputs.tfr
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
#' @inheritParams dp.inputs.tfr
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

#' Get Spectrum's calculated HIV-related deaths among PLHIV on ART
#'
#' Get Spectrum's calculated HIV-related deaths among PLHIV onART by age, sex,
#' and year in long or wide format
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.output.deaths.art = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n.sex = length(strata.labels$sex.aug)
  n.age = length(strata.labels$age) + 1 # includes "all" row
  fmt = list(cast=as.numeric, offset=3, nrow=n.sex * n.age, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<AIDSDeathsARTSingleAge MV>", fmt)
  dat = cbind(rep(strata.labels$sex.aug, each=n.age),
              rep(c("All", strata.labels$age), n.sex),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))

  dat = subset(dat, Age != "All") # "All" age rows are not populated by Spectrum

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
#' @inheritParams dp.inputs.tfr
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

#' Get inputs saved for EPP
#'
#' Get the percentage of adults on ART who are age 50 or older.
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.output.art.50plus = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=length(strata.labels$sex.aug), ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<PercART50Plus MV>", fmt)
  dat = cbind(strata.labels$sex.aug, data.frame(raw))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get Spectrum's estimate of adult PLHIV by CD4 category
#'
#' Get the estimated number of adults by CD4 category and ART status
#' @describeIn dp.output.cd4.15_up Adults age 15+
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.output.cd4.15_up = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## CD4 outputs are organized in two blocks, one off ART the other on ART.
  ## Blocks have rows by sex (including both) and CD4 (including HIV-negative).
  ## Blocks are separated by a blank line.
  ndat = length(strata.labels$sex.aug) * (length(strata.labels$cd4.adult) + 1)
  nrow = 2 * ndat + 1
  fmt = list(cast=as.numeric, offset=2, nrow=nrow, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<CD4Distribution MV2>", fmt)
  raw = rbind(raw[1:ndat,], raw[1:ndat + ndat + 1,])
  dat = cbind(expand.grid(CD4=c("HIV-", strata.labels$cd4.adult),
                          Sex=strata.labels$sex.aug,
                          ART=strata.labels$art.status),
              data.frame(raw))
  colnames(dat) = c("CD4", "Sex", "ART", sprintf("%d", first.year:final.year))
  dat = dat[dat$CD4 != "HIV-",] # drop rows for HIV-negative people

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "CD4", "ART"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' @describeIn dp.output.cd4.15_up Adults age 15-49
#' @export
dp.output.cd4.15_49 = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  ## CD4 outputs are organized in two blocks, one off ART the other on ART.
  ## Blocks have rows by sex (including both) and CD4 (including HIV-negative).
  ## Blocks are separated by a blank line.
  ndat = length(strata.labels$sex.aug) * (length(strata.labels$cd4.adult) + 1)
  nrow = 2 * ndat + 1
  fmt = list(cast=as.numeric, offset=2, nrow=nrow, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<CD4Distribution15_49 MV2>", fmt)
  raw = rbind(raw[1:ndat,], raw[1:ndat + ndat + 1,])
  dat = cbind(expand.grid(CD4=c("HIV-", strata.labels$cd4.adult),
                          Sex=strata.labels$sex.aug,
                          ART=strata.labels$art.status),
              data.frame(raw))
  colnames(dat) = c("CD4", "Sex", "ART", sprintf("%d", first.year:final.year))
  dat = dat[dat$CD4 != "HIV-",] # drop rows for HIV-negative people

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "CD4", "ART"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' Get numbers of 14-year-olds living with HIV
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @section Details:
#'
#'   Spectrum stores detailed information about 14-year-olds living with HIV.
#'   This is used by EPP to account for the contribution of mother-to-child HIV
#'   transmission to HIV prevalence in younger adjults as children living with
#'   HIV reach adulthood. Data are stratified by sex, CD4 category, ART status,
#'   and timing of HIV acquisition (perinatal, breastfeeding at 0-6, 6-12, or
#'   12+ months of life)
#' @export
dp.output.child.hiv.14 = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  rlab = expand.grid(
    HIV=strata.labels$hiv.ped,
    CD4=strata.labels$cd4.child.old,
    Sex=strata.labels$sex)
  fmt = list(cast=as.numeric, offset=2, nrow=nrow(rlab), ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<ChAged14ByCD4Cat MV>", fmt)
  dat = cbind(rlab, data.frame(raw))
  colnames(dat) = c("HIV", "CD4", "Sex", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "HIV", "CD4"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get Spectrum adult ART inputs
#'
#' Get input numbers or percentages of adults on ART entered into Spectrum by
#' sex and year in long or wide format
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @section Details:
#'
#'   Adult treatment data can be entered into Spectrum as numbers or
#'   percentages. These units can vary from year to year. When
#'   \code{direction="wide"}, the return value will include rows for numbers and
#'   for percentages, with percentages missing in years where numbers were
#'   entered or vice-versa. When \code{direction="long"}, the data frame will
#'   include data for numbers or percentages, but not both, for any year.
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

#' Extract inputs used to adjust programmatic numbers on ART for overcount or
#' undercount
#'
#' Routine ART program data entered into Spectrum may be overcounted or
#' undercounted for a variety of reasons. Countries may enter scale factors by
#' year to adjust their program data up or down proportionally. These scale
#' factors are entered seperately for adults and for children. Spectrum also has
#' a TRUE/FALSE flag associated with adult and child adjustments. Spectrum will
#' only adjust numbers on ART in a given year if this flag is TRUE, the number
#' on ART is entered as an absolute number (not a percentage), and the scale
#' factor value for that year is some value besides 1.
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @describeIn dp.inputs.adult.art.adjustment.value Adjustment values for adult
#'   ART by sex and year.
#' @export
dp.inputs.adult.art.adjustment.value = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}
  fmt = list(cast=as.numeric, offset=3, nrow=2, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<AdultARTAdjFactor>", fmt)
  dat = cbind(strata.labels$sex, data.frame(raw))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.adult.art.adjustment.value TRUE if adult ART adjustments are enabled, FALSE otherwise.
#' @export
dp.inputs.adult.art.adjustment.flag = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<AdultARTAdjFactorFlag>", fmt)[1,1] == 1)
}

#' @describeIn dp.inputs.adult.art.adjustment.value Adjustment values for child ART by year.
#' @export
dp.inputs.child.art.adjustment.value = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<ChildARTAdjFactor MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' @describeIn dp.inputs.adult.art.adjustment.value TRUE if child ART adjustments are enabled, FALSE otherwise.
#' @export
dp.inputs.child.art.adjustment.flag = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<ChildARTAdjFactorFlag>", fmt)[1,1] == 1)
}

#' Check parameters used to determine ART initiation timing
#'
#' @inheritParams dp.inputs.first.year
#' @return A list with named elements 'Method' and 'Weight'
#' @section Details:
#'
#'   Spectrum uses one of four methods to determine how new ART patients are
#'   drawn from the ART-eligible population by CD4 category:
#'   \enumerate{
#'   \item{Method=1: ART initiations are proportional to the expected number of HIV-related deaths in each CD4 category.}
#'   \item{Method=2: ART initiations are proportional to the number of people in each CD4 category.}
#'   \item{Method=3: ART initiations are a weighted combination of methods 1 and 2.}
#'   \item{Method=4: ART initiations are drawn from the lowest CD4 categories first.}
#'   }
#'   The return value element "Weight"=(w1, w2) is a two-element vector that
#'   describes the weight placed on method 1 (w1) and method 2 (w2) when using
#'   methods 1, 2, or 3. Spectrum ignores these weights when method 4 is used.
#' @export
dp.inputs.adult.art.allocation = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  fmt_method = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  fmt_weight = list(cast=as.numeric, offset=2, nrow=1, ncol=2)
  raw_method = extract.dp.tag(dp.raw, "<NewARTPatAllocationMethod MV2>", fmt_method)[1,1]
  raw_weight = extract.dp.tag(dp.raw, "<NewARTPatAlloc MV>", fmt_weight)
  return(list(Method=raw_method, Weight=as.vector(raw_weight)))
}

#' Get PMTCT inputs
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @describeIn dp.inputs.pmtct Number or percent of women receiving PMTCT by year and regimen
#' @export
dp.inputs.pmtct = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  inc = c(2:15, 18:21) # include these rows; exclude "none", "total", and monthly dropout rows
  fmt = list(cast=I, offset=2, nrow=26, ncol=final.year - first.year + 2)
  raw = extract.dp.tag(dp.raw, "<ARVRegimen MV3>", fmt)
  raw = raw[inc,2:ncol(raw)] # drop percent/number column
  raw = matrix(as.numeric(raw), nrow=length(inc))
  colnames(raw) = first.year:final.year

  preg_names = expand.grid(Timing  = strata.labels$pmtct_time[1],
                           Unit    = c("Number", "Percent"),
                           Regimen = strata.labels$pmtct_regimen)
  post_names = expand.grid(Timing  = strata.labels$pmtct_time[2],
                           Unit    = c("Number", "Percent"),
                           Regimen = strata.labels$pmtct_regimen[c(3,4)])
  dat = cbind(rbind(preg_names, post_names), data.frame(raw, check.names=FALSE))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Timing", "Unit", "Regimen"), value.name="Value", variable.name="Year")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.pmtct Percent retained on ART at delivery among HIV+ pregnant women
#' @export
dp.inputs.pmtct.retention.perinatal = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  rnames = c("ART started before current pregnancy", "ART started during current pregnancy")

  fmt = list(cast=as.numeric, offset=2, nrow=2, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<PercentARTDelivery MV>", fmt)
  dat = cbind(rnames, data.frame(raw))
  colnames(dat) = c("Timing", sprintf("%d", first.year:final.year))

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Timing"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.pmtct Percentage of HIV+ breastfeeding mothers who interrupt PMTCT each month
#' @export
dp.inputs.pmtct.retention.postnatal = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  inc = 23:26 # rows to include
  fmt = list(cast=I, offset=2, nrow=26, ncol=final.year - first.year + 2)
  raw = extract.dp.tag(dp.raw, "<ARVRegimen MV3>", fmt)
  raw = raw[inc,2:ncol(raw)] # drop percent/number column then convert to numbers
  raw = matrix(as.numeric(raw), nrow=length(inc))

  reg_names = c(strata.labels$pmtct_regimen[3],
                strata.labels$pmtct_regimen[4],
                "ART <12 months after delivery",
                "ART 12+ months after delivery")

  dat = cbind(reg_names, data.frame(raw, check.names=FALSE))
  dat$Unit = NULL # Unit column is not needed since all values are reported as percentages
  colnames(dat) = c("Regimen", first.year:final.year)

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Regimen"), value.name="Value", variable.name="Year")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the input percentage of adults on ART who are lost to follow-up annually
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.adult.art.ltfu = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<PercLostFollowup MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get the input percentage of children on ART who are lost to follow-up annually
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.child.art.ltfu = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<PercLostFollowupChild MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get Spectrum child HIV treatment inputs
#'
#' Get input numbers or percentages of children on antiretroviral therapy (ART)
#' or cotrimoxazole (CTX) prophylaxis entered into Spectrum by year and age in
#' long or wide format
#' @inheritParams dp.inputs.tfr
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

#' Get input numbers of adults initiating ART
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.adult.art.initiations = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n.sex = length(strata.labels$sex)
  fmt = list(cast=as.numeric, offset=2, nrow=n.sex, ncol = final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<NumNewlyInitART MV>", fmt)
  dat = cbind(strata.labels$sex, data.frame(raw))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get input numbers of adults who re-initiated ART
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.adult.art.reinitiations = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<NumberInitTreatmentReinits MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get input numbers of children initiating ART
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.child.art.initiations = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<NumNewlyInitARTChild MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get input numbers of children who re-initiated ART
#'
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.child.art.reinitiations = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<NumberInitTreatmentReinitsChild MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' ART uptake in children by age and year
#'
#' Get the annual probability of ART initiation by single age among children not on ART
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.child.art.uptake = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=2, nrow=15, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<ChildARTDist MV>", fmt)
  dat = cbind(Age=0:14, data.frame(raw))
  colnames(dat) = c("Age", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get Spectrum ART by age inputs
#'
#' Get input numbers of people on ART entered into Spectrum by sex, age and year
#' in long or wide format. These data may be entered by five-year age group, or
#' in age groups specified for UNAIDS Global AIDS Monitoring. The latter
#' stratifies ages 25+ into two age groups, 25-49 and 50+.
#' @inheritParams dp.inputs.tfr
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
#' @inheritParams dp.inputs.tfr
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

#' Nosocomial infections among children
#'
#' AIM takes as input numbers of nosocomial infections in children by five-year
#' age group and calendar year.
#' @inheritParams dp.inputs.first.year
#' @return A data frame.
#' @export
dp.inputs.nosocomial.infections = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=2, nrow=3, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, "<NosocomialInfectionsByAge MV>", fmt)
  dat = cbind(Age=strata.labels$age.5yr[1:3], data.frame(raw))
  colnames(dat) = c("Age", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the maximum amount that AIM can adjust incidence estimates from EPP
#'
#' EPP hands AIM estimated HIV incidence and prevalence trends. EPP and AIM
#' have different structures, so EPP's incidence may produce a somewhat different prevalence trend
#' than EPP provides. EPP estimates its trends using HIV prevalence data, so
#' AIM may adjust the incidence it gets from EPP to better match EPP's prevalence
#' estimate, subject to a user-specified maximum allowed adjustment. This
#' function returns that maximum. The minimum adjustment is equal to the reciprocal
#' of the maximum.
#' @inheritParams dp.inputs.first.year
#' @return the maximum allowed adjustment.
#' @export
dp.inputs.epp.adjustment.cap = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<EPPMaxAdjFactor MV>", fmt)[1,1])
}

#' Check if AIM is allowed to adjust HIV incidence from EPP
#' @inheritParams dp.inputs.first.year
#' @return TRUE if adjustments is allowed, FALSE otherwise
#' @section Details:
#'
#'   The cap on adjustments allowed can be accessed using
#'   \code{dp.inputs.epp.adjustment.cap}. See the documentation of that function
#'   for more details on the rationale for adjustment.
#'
#' @export
dp.inputs.epp.adjustment.enabled = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<EPPPrevAdj MV>", fmt)[1,1] == 1)
}

#' Get Spectrum inputs describing incidence by age and sex
#'
#' @describeIn dp.inputs.irr.age Incidence rate ratios by age relative to ages 25-29 for each sex
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @section Details:
#'
#'   Spectrum supports several ways of specifying epidemic patterns for
#'   determining incidence patterns by sex and age. The choice of pattern
#'   can be extracted using \code{dp.inputs.irr.pattern}:
#'   \enumerate{
#'   \item{Generalized - default pattern for generalized epidemics}
#'   \item{Concentrated non-IDU - default pattern for concentrated epidemics driven by transmission modes other than injection drug use}
#'   \item{Concentrated IDU - default pattern for concentrated epidemics driven by injection drug use}
#'   \item{Fitted: fixed over time - deprecated, replaced by "Fitted to HIV prevalence or ART"}
#'   \item{Fitted: time-varying - deprecated, replaced by "Fitted to HIV prevalence or ART"}
#'   \item{Fitted to HIV prevalence or ART - pattern estimated from survey data}
#'   \item{CSAVR - pattern estimated while fitting CSAVR}
#'   }
#'
#' Spectrum can estimate incidence by age and sex from HIV prevalence by age and
#' sex from household surveys, or from a country's reported numbers of people on ART
#' by age and sex. When fitting to HIV prevalence, countries could choose to use patterns
#' that were fixed or varied over time. In older versions of Spectrum, these choices could be
#' accessed via \code{dp.inputs.irr.pattern}. In current Spectrum versions, \code{dp.inputs.irr.pattern}
#' just indicates whether a pattern was fitted to HIV prevalence or ART, then \code{dp.inputs.irr.fitted}
#' is used to indicate the data source and model type:
#' \enumerate{
#' \item{HIV prevalence, Fixed incidence ratios over time}
#' \item{HIV prevalence, Time dependent incidence ratios}
#' \item{ART by age}
#' }
#'
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

#' @describeIn dp.inputs.irr.age Female-to-male incidence rate ratio
#' @export
dp.inputs.irr.sex = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<HIVSexRatio MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=3))
}

#' @describeIn dp.inputs.irr.age Return the epidemic pattern as a factor value.
#' @export
dp.inputs.irr.pattern = function(dp.raw, direction="wide") {
  n.epi = length(strata.labels$epi.patterns)
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  dat = extract.dp.tag(dp.raw, "<IncEpidemicRGIdx MV>", fmt)[1,1]
  dat = factor(dat, levels=0:(n.epi - 1), labels=strata.labels$epi.patterns)
  return(dat)
}

#' @describeIn dp.inputs.irr.age Returns TRUE if incidence patterns have been entered manually, FALSE otherwise
#' @export
dp.inputs.irr.custom = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<IncEpidemicCustomFlagIdx MV>", fmt)[1,1]==1)
}

#' @describeIn dp.inputs.irr.age Returns TRUE if incidence rate ratios by sex were imported from EPP, FALSE otherwise
#' @export
dp.inputs.irr.sex.from.epp = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<SexRatioFromEPP MV>", fmt)[1,1] == 1)
}

#' @describeIn dp.inputs.irr.age Check what options were used to estimate incidence rate ratios.
#' @export
dp.inputs.irr.fitted = function(dp.raw, direction="wide") {
  fmt_dat = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  fmt_irr = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  opt_dat = extract.dp.tag(dp.raw, "<SAPFitType MV>", fmt_dat)[1,1]
  opt_irr = extract.dp.tag(dp.raw, "<HIVPrevModel MV>", fmt_irr)[1,1]
  if (opt_dat == 0) {
    rval = ifelse(opt_irr == 0, "HIV prevalence, Fixed incidence ratios over time", "HIV prevalence, Time dependent incidence ratios")
  } else if (opt_dat == 1) {
    rval = "ART by age"
  } else {
    rval = "Unrecognized option"
  }
  return(rval)
}

#' Get the HIV-related fertility local adjustment factor
#'
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Ignored; included for compatibility with similar functions.
#' @param first.year Ignored; included for compatibility with similar functions.
#' @param final.year Ignored; included for compatibility with similar functions.
#' @return the HIV-related fertility local adjustment factor
#' @export
dp.inputs.hiv.frr.location = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<FRRbyLocation MV>", fmt)[1,1])
}

#' Get HIV-related fertility adjustments by age
#'
#' Get HIV-related fertility adjustments by age and year for women with
#' untreated HIV
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.hiv.frr.age = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  nr = length(strata.labels$age.fert)
  fmt = list(cast=as.numeric, offset=2, nrow=nr, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<HIVTFR MV4>", fmt)
  dat = cbind(strata.labels$age.fert, data.frame(raw))
  colnames(dat) = c("Age", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get HIV-related fertility adjustments by CD4 cell count
#'
#' Get HIV-related fertility adjustments by CD4 cell count category for women
#' with untreated HIV
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year Ignored; included for compatibility with similar functions.
#' @param final.year Ignored; included for compatibility with similar functions.
#' @return A data frame.
#' @export
dp.inputs.hiv.frr.cd4 = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=length(strata.labels$cd4.adult)+1)
  raw = extract.dp.tag(dp.raw, "<FertCD4Discount MV>", fmt)
  dat = raw[,2:ncol(raw),drop=FALSE] # first cell is empty
  if (direction == "long") {
    dat = data.frame(CD4=strata.labels$cd4.adult, Value=dat[1,])
  } else {
    dat = data.frame(dat)
    colnames(dat) = strata.labels$cd4.adult
  }
  return(dat)
}

#' Get HIV-related fertility adjustments on ART
#'
#' Get HIV-related fertility adjustments by age for women on ART
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year Ignored; included for compatibility with similar functions.
#' @param final.year Ignored; included for compatibility with similar functions.
#' @return A data frame.
#' @export
dp.inputs.hiv.frr.art = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=length(strata.labels$age.fert))
  raw = extract.dp.tag(dp.raw, "<RatioWomenOnART MV2>", fmt)
  if (direction == "long") {
    dat = data.frame(Age=strata.labels$age.fert, Value=raw[1,])
  } else {
    dat = data.frame(raw)
    colnames(dat) = strata.labels$age.fert
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

#' Get viral suppression inputs
#'
#' @describeIn dp.inputs.viral.suppression Numbers of people who had a viral load measured and numbers who were virally suppressed
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @details Some versions of Spectrum only allowed viral suppression data to be
#'   entered during 2010-2025. Data are not available in earlier or later years
#'   in files saved with these versions. Spectrum 6.29 and later can save data
#'   for every year in a projection, though countries may not input anything in
#'   most years.
#' @export
dp.inputs.viral.suppression = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  tag_v3 = "<ViralSuppressionInput MV3>"
  tag_v4 = "<ViralSuppressionInput MV4>"

  if (tag_v3 %in% dp.raw$Tag) {
    fmt = list(cast=as.numeric, offset=2, nrow=9, ncol=2025-2010+2)
    raw = extract.dp.tag(dp.raw, tag_v3, fmt)
    raw = raw[,2:ncol(raw)] # modvar uses a non-standard first column
    cnames = c("Indicator", "Population", 2010:2025)
  } else {
    fmt = list(cast=as.numeric, offset=2, nrow=9, ncol=final.year - first.year + 1)
    raw = extract.dp.tag(dp.raw, tag_v4, fmt)
    cnames = c("Indicator", "Population", first.year:final.year)
  }
  raw[raw==dp_not_avail] = NA
  pop = c("Children 0-14", "Males 15+", "Females 15+")
  ind = c("On ART", "Number tested", "Number virally suppressed")
  dat = cbind(expand.grid(ind, pop), data.frame(raw))
  colnames(dat) = cnames
  dat = subset(dat, Indicator != "On ART") # not filled in
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Indicator", "Population"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn dp.inputs.viral.suppression The threshold (in virus copies/mL) used to classify people as virally suppressed
#' @export
dp.inputs.viral.suppression.threshold = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<ViralSuppressionThreshold MV4>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get HIV prevalence data entered from household surveys
#'
#' Get HIV prevalence data from household surveys that have been entered into
#' Spectrum for validation of its HIV prevalence estimates.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @return A data frame.
#' @section Details:
#'
#'   Spectrum can store data from up to five household surveys. Data
#'   consist of HIV prevalence estimates by sex (male, female, both)
#'   and five-year age group (0-4, 5-9, ..., 75-79, 80+). Five statistics
#'   are stored for each group:
#'
#'   \enumerate{
#'   \item{Number - not used}
#'   \item{Prevalence - HIV prevalence point estimate}
#'   \item{Lower - 95\% confidence interval lower bound}
#'   \item{Upper - 95\% confidence interval upper bound}
#'   \item{N - sample size. Countries may enter non-integral survey-weighted sample sizes}
#'   }
#'
#' @export
dp.inputs.survey.hiv.prevalence = function(dp.raw, direction="wide") {
  tag_found = grep("PrevSurveyData", dp.raw$Tag, value=TRUE)
  tags_v4 = list(data="<PrevSurveyData MV4>", used="<PrevSurveyUsed MV2>", name="<PrevSurveyName MV2>", year="<PrevSurveyYear MV2>")
  tags_v5 = list(data="<PrevSurveyData MV5>", used="<PrevSurveyUsed MV3>", name="<PrevSurveyName MV3>", year="<PrevSurveyYear MV3>")

  if (tag_found == tags_v5$data) {
    max_surveys = 8
    tags = tags_v5
  } else if (tag_found == tags_v4$data) {
    max_surveys = 5
    tags = tags_v4
  }

  ## nrow allows for a blank row between consecutive surveys
  nrow = (length(strata.labels$sex.aug) * length(strata.labels$age.5yr) + 1) * max_surveys - 1
  cnames = c("Number", "Prevalence", "Lower", "Upper", "N")
  ncol = length(cnames)

  fmt_data = list(cast=as.numeric,   offset=4, nrow=nrow, ncol=ncol)
  fmt_used = list(cast=as.numeric,   offset=2, nrow=1, ncol=max_surveys)
  fmt_name = list(cast=as.character, offset=2, nrow=1, ncol=max_surveys)
  fmt_year = list(cast=as.numeric,   offset=2, nrow=1, ncol=max_surveys)

  raw_data = extract.dp.tag(dp.raw, tags$data, fmt_data)
  raw_used = extract.dp.tag(dp.raw, tags$used, fmt_used)
  raw_name = extract.dp.tag(dp.raw, tags$name, fmt_name)
  raw_year = extract.dp.tag(dp.raw, tags$year, fmt_year)

  raw_data = raw_data[!is.na(raw_data[,1]),] # drop blank rows separating surveys
  raw_data = as.data.frame(raw_data)
  colnames(raw_data) = cnames

  dat = cbind(expand.grid(Age  = strata.labels$age.5yr,
                          Sex  = strata.labels$sex.aug,
                          Name = raw_name),
              raw_data)

  ## Merge in survey metadata
  meta = data.frame(Year=as.vector(raw_year),
                    Name=as.vector(raw_name),
                    Used=as.vector(raw_used))
  dat = dplyr::left_join(dat, meta, by=c("Name"))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Name", "Sex", "Age", "Year", "Used"), variable.name="Statistic", value.name="Value")
  } else {
    dat = dat[,c("Name", "Sex", "Age", "Year", "Used", "Prevalence", "Lower", "Upper", "N")]
  }

  return(dat)
}

#' Get ART coverage data entered from household surveys
#'
#' Get ART coverage data from household surveys that have been entered into
#' Spectrum for validation of its ART coverage estimates.
#' @param dp.raw DemProj module data in raw format, as returned by
#'   \code{read.raw.dp()}
#' @param direction Request "wide" (default) or "long" format data.
#' @return A data frame.
#' @section Details:
#'
#'   Spectrum can store data from up to five household surveys. Data
#'   consist of ART coverage estimates by sex (male, female, both)
#'   and five-year age group (0-4, 5-9, ..., 75-79, 80+). Five statistics
#'   are stored for each group:
#'
#'   \enumerate{
#'   \item{Number - not used}
#'   \item{Coverage - ART coverage point estimate}
#'   \item{Lower - 95\% confidence interval lower bound}
#'   \item{Upper - 95\% confidence interval upper bound}
#'   \item{N - sample size. Countries may enter non-integral survey-weighted sample sizes}
#'   }
#'
#' @export
dp.inputs.survey.art.coverage = function(dp.raw, direction="wide") {
  tag_found = grep("ARTCovSurveyData", dp.raw$Tag, value=TRUE)
  tags_v1 = list(data="<ARTCovSurveyData MV>",  used="<ARTCovSurveyUsed MV>",  name="<ARTCovSurveyName MV>",  year="<ARTCovSurveyYear MV>")
  tags_v2 = list(data="<ARTCovSurveyData MV2>", used="<ARTCovSurveyUsed MV2>", name="<ARTCovSurveyName MV2>", year="<ARTCovSurveyYear MV2>")

  if (tag_found == tags_v2$data) {
    max_surveys = 8
    tags = tags_v2
  } else if (tag_found == tags_v1$data) {
    max_surveys = 5
    tags = tags_v1
  }

  lab.age = c(strata.labels$age.5yr, "0-14", "15-49", "50+")
  num.age = length(lab.age)

  ## nrow allows for a blank row between consecutive surveys
  nrow = (length(strata.labels$sex.aug) * num.age + 1) * max_surveys - 1

  fmt_data = list(cast=as.numeric,   offset=4, nrow=nrow, ncol=5)
  fmt_used = list(cast=as.numeric,   offset=2, nrow=1, ncol=max_surveys)
  fmt_name = list(cast=as.character, offset=2, nrow=1, ncol=max_surveys)
  fmt_year = list(cast=as.numeric,   offset=2, nrow=1, ncol=max_surveys)

  raw_data = extract.dp.tag(dp.raw, tags$data, fmt_data)
  raw_used = extract.dp.tag(dp.raw, tags$used, fmt_used)
  raw_name = extract.dp.tag(dp.raw, tags$name, fmt_name)
  raw_year = extract.dp.tag(dp.raw, tags$year, fmt_year)

  raw_data = raw_data[!is.na(raw_data[,1]),] # drop blank rows separating surveys
  raw_data[raw_data==dp_not_avail] = NA      # replace missing values with NA
  raw_data = as.data.frame(raw_data)
  colnames(raw_data) = c("Number", "Coverage", "Lower", "Upper", "N")

  dat = cbind(expand.grid(Age  = lab.age,
                          Sex  = strata.labels$sex.aug,
                          Name = raw_name),
              raw_data)

  ## Merge in survey metadata
  meta = data.frame(Year=as.vector(raw_year),
                    Name=as.vector(raw_name),
                    Used=as.vector(raw_used))
  dat = dplyr::left_join(dat, meta, by=c("Name"))

  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Name", "Sex", "Age", "Year", "Used"), variable.name="Statistic", value.name="Value")
  } else {
    dat = dat[,c("Name", "Sex", "Age", "Year", "Used", "Coverage", "Lower", "Upper", "N")]
  }

  return(dat)
}

#' Get all-cause deaths among adult PLHIV on ART
#'
#' @inheritParams dp.inputs.tfr
#' @export
dp.inputs.deaths.art.allcause = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  tag = "<AllCauseDeathsARTValidation MV>"
  return(dp.extract.time.series(dp.raw, direction, first.year, final.year, tag=tag, offset=2))
}

#' Get COVID-19 deaths inputs
#'
#' Get COVID-19 deaths inputs to DemProj. This includes values for every year of
#' the projection, not just 2019 and later.
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.covid19.deaths = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  fmt = list(cast=as.numeric, offset=4, nrow=3, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<COVID19DeathRate_MV>", fmt)
  dat = cbind(strata.labels$sex, data.frame(raw[c(1,3),]))
  colnames(dat) = c("Sex", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Get the input age distribution of COVID-19 deaths
#'
#' Get the input age distribution of COVID-19 deaths. This includes values for
#' every year of the projection, not just 2019 and later.
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @export
dp.inputs.covid19.pattern = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  nr = length(strata.labels$sex) * length(strata.labels$age.5yr)
  fmt = list(cast=as.numeric, offset=3, nrow=nr, ncol=final.year - first.year + 1)
  raw = extract.dp.tag(dp.raw, "<COVID19DeathAgeDist MV>", fmt)
  dat = cbind(rep(strata.labels$sex, each=length(strata.labels$age.5yr)),
              rep(strata.labels$age.5yr, length(strata.labels$sex)),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name=output.labels$value)
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' Check if COVID-19 inputs are enabled
#'
#' Countries can toggle entry of COVID-19 deaths as a configuration option. This
#' function checks whether that toggle is turned on.
#' @inheritParams dp.inputs.first.year
#' @return TRUE if the toggle is enabled, FALSE otherwise.
#' @export
dp.inputs.covid19.enabled = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=3, nrow=1, ncol=1)
  return(extract.dp.tag(dp.raw, "<EnterCOVID19Deaths_MV>", fmt)[1,1] == 1)
}

#' AIM advanced options regional configuration
#'
#' Get the region selected for advanced options parameter values in Spectrum
#' @inheritParams dp.inputs.first.year
#' @return The region name as a factor (see "Details" below for factor levels)
#' @describeIn dp.inputs.adult.hiv.mortality.region Mortality rates among HIV+ adults not on ART
#' @section Details:
#'
#' Spectrum options are specified for one of several regions:
#' \enumerate{
#' \item{Asia}
#' \item{Central Africa}
#' \item{Developed Countries}
#' \item{East Africa}
#' \item{Eastern Europe}
#' \item{Latin America and Caribbean}
#' \item{North Africa Middle East}
#' \item{Southern Africa}
#' \item{West Africa}
#' }
#'
#' @export
dp.inputs.adult.hiv.mortality.region = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  dat = extract.dp.tag(dp.raw, "<AdultHIVMortNoARTRegion MV2>", fmt)[1,1]
  return(factor(dat, levels=1:length(strata.labels$opt.region), labels=strata.labels$opt.region))
}

#' @describeIn dp.inputs.adult.hiv.mortality.region Mortality rates among HIV+ adults on ART
#' @export
dp.inputs.adult.hiv.mortality.art.region = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  dat = extract.dp.tag(dp.raw, "<HIV Mortality with ART Country or Region MV>", fmt)[1,1]
  return(factor(dat, levels=1:length(strata.labels$opt.region), labels=strata.labels$opt.region))
}


#' AIM advanced option configuration
#'
#' Check if advanced options in AIM are unlocked for customization.
#' @inheritParams dp.inputs.first.year
#' @return TRUE if the parameter is unlocked for configuration, FALSE otherwise.
#' @describeIn dp.inputs.adult.hiv.mortality.custom Mortality rates among HIV+ adults not on ART
#' @export
dp.inputs.adult.hiv.mortality.custom = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  dat = extract.dp.tag(dp.raw, "<AdultHIVMortNoARTCustomFlag MV>", fmt)[1,1]
  return(dat==1)
}

#' @describeIn dp.inputs.adult.hiv.mortality.custom Mortality rates among HIV+ adults on ART
#' @export
dp.inputs.adult.hiv.mortality.art.custom = function(dp.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  dat = extract.dp.tag(dp.raw, "<AdultHIVMortARTCustomFlag MV>", fmt)[1,1]
  return(dat==1)
}

#' Data on abortions among HIV-positive women
#'
#' Get input data on the number of pregnancies among HIV-positive women
#' terminated by abortion
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @section Details:
#'
#'   Abortion data can be entered in Spectrum as numbers or percentages
#'   of HIV-positive women. These units can vary from year to year. When
#'   \code{direction="wide"}, the return value includes separate rows
#'   numbers and percentages. When \code{direction="long"}, the data frame
#'   will include one row per year with the unit indicated.
#'
#' @export
dp.inputs.hiv.abortion = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  vals = dp.extract.time.series(dp.raw, direction="wide", first.year, final.year, tag="<PregTermAbortion MV3>",       offset=2)
  unit = dp.extract.time.series(dp.raw, direction="wide", first.year, final.year, tag="<PregTermAbortionPerNum MV2>", offset=2)
  num.raw = vals
  pct.raw = vals
  num.raw[unit==1] = NA
  pct.raw[unit==0] = NA
  dat = cbind(c("Number", "Percent"), rbind(data.frame(num.raw), data.frame(pct.raw)))
  colnames(dat) = c("Unit", sprintf("%d", first.year:final.year))
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Unit"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
    dat = dat[is.finite(dat$Value),] # Remove "NA" values
  }
  return(dat)
}

# Helper function for extracting CSAVR age-aggregated outputs organized by model, sex, and statistic
extract.csavr.output = function(tag, dp.raw, direction, first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = dp.inputs.first.year(dp.raw)}
  if (is.null(final.year)) {final.year = dp.inputs.final.year(dp.raw)}

  n_mod = length(strata.labels$csavr.model) - 1
  n_sex = length(strata.labels$sex.aug)
  fmt = list(cast=as.numeric, offset=2, nrow=n_mod * n_sex * 4, ncol=final.year-first.year+1)
  raw = extract.dp.tag(dp.raw, tag, fmt)
  dat = cbind(expand.grid(Statistic=c("Value", "Unused", "Lower", "Upper"),
                          Sex=strata.labels$sex.aug,
                          Model=strata.labels$csavr.model[1+1:n_mod]), raw)
  dat = dat[dat$Statistic!="Unused",]
  colnames(dat) = c("Statistic", "Sex", "Model", sprintf("%d", first.year:final.year))
  dat$Model = factor(dat$Model)
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Model", "Sex", "Statistic"), variable.name="Year", value.name="Value")
    dat = reshape2::dcast(dat, Model+Sex+Year~Statistic, value.var="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' CSAVR estimates
#'
#' Get estimates produced by Spectrum's Case Surveillance and Vital Registration (CSAVR) tool
#' @inheritParams dp.inputs.tfr
#' @return A data frame.
#' @describeIn dp.output.csavr.deaths.hiv HIV-related deaths in adults
#' @export
dp.output.csavr.deaths.hiv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  return(extract.csavr.output("<CSAVRAIDSDeaths MV3>", dp.raw, direction, first.year, final.year))
}

#' @describeIn dp.output.csavr.deaths.hiv New HIV infections in adults
#' @export
dp.output.csavr.incident.hiv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  return(extract.csavr.output("<CSAVRNumNewInfections MV3>", dp.raw, direction, first.year, final.year))
}

#' @describeIn dp.output.csavr.deaths.hiv Adults living with HIV
#' @export
dp.output.csavr.plhiv = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  return(extract.csavr.output("<CSAVRNumPLHIV MV3>", dp.raw, direction, first.year, final.year))
}

#' @describeIn dp.output.csavr.deaths.hiv New HIV diagnoses in adults
#' @export
dp.output.csavr.diagnoses = function(dp.raw, direction="wide", first.year=NULL, final.year=NULL) {
  return(extract.csavr.output("<CSAVRNumDiagnosed MV3>", dp.raw, direction, first.year, final.year))
}

#' Extract uncertainty analysis data
#' @param pjnz.file The Spectrum file to extract data from
#' @param direction Request "wide" (default) or "long" format data.
#' @return A named list. See \code{Details} for a description of list contents.
#' @section Details:
#' \code{dp.output.ua.data} returns data used to calculate uncertainty bounds
#' on AIM estimates along with metadata captured when Spectrum's Uncertainty
#' Analysis (UA) tool was run. The data and metadata items consist of:
#' \describe{
#' \item{Version}{The version number of the file that stores UA data.}
#' \item{Date}{The date and time the UA tool was run, according to the
#' user's system clock. Note that SpectrumUtils interprets this in UTC timezone,
#' but Spectrum does not save the timezone so the date is only accurate to
#' within 24 hours.}
#' \item{AIDSDeaths}{The number of HIV-related deaths in the final year
#' of the projection across all ages and both sexes. Spectrum considers UA
#' results valid only if this number of deaths agrees with the point estimate
#' of HIV-related deaths. If these disagree, the user probably changed some
#' model inputs since the last time UA was run.}
#' \item{Data}{Uncertainty analysis bounds by sex and year for several
#' key indicators. Bounds are expressed as a ratio relative to the point
#' estimate. For example, a high bound of 1.1 means that the upper bound
#' is 10\% higher than the point estimate. Point estimates for indicators
#' must be extracted separately using appropriate \code{dp.extract}
#' functions.}
#' }
#' @export
dp.output.ua.data = function(pjnz.file, direction="wide", first.year=NULL, final.year=NULL) {
  raw = read.module.data(pjnz.file, extension="DPUAD")

  version = as.numeric(raw[1,2])
  if (version == 10) {
    date   = lubridate::parse_date_time(raw[2,2], "mdy IMS Op", quiet=TRUE)
    deaths = as.numeric(raw[3,2])
    bgnrow = grep("Master ID", raw[,1])
    endrow = grep("<end>", raw[,1])
    dat    = raw[bgnrow:endrow,]

    dat_name = dat[1,]
    dat_vals = dat[3:nrow(dat),]
    colnames(dat_vals) = dat_name
    colnames(dat_vals)[2] = "Indicator"
    colnames(dat_vals)[3] = "Sex"
    colnames(dat_vals)[5] = "Bound"

    indicator = NA
    for (k in 1:nrow(dat_vals)) {
      if (dat_vals$Indicator[k] != "") {
        indicator = dat_vals$Indicator[k]
      } else {
        dat_vals$Indicator[k] = indicator
      }
    }
    dat_vals = dat_vals[,c(2, 3, 5, 6:ncol(dat_vals))]

    if (direction == "long") {
      dat_vals = reshape2::melt(dat_vals, id.vars=c("Indicator", "Sex", "Bound"), value.name="Value", variable.name="Year")
      dat_vals$Year = as.numeric(as.character(dat_vals$Year))
    }

  } else {
    warning("Uncertainty analysis data file version %d not supported", version)
    date = NULL
    deaths = NULL
    dat_vals = NULL
  }

  return(list(Version    = version,
              Date       = date,
              AIDSDeaths = deaths,
              Data       = dat_vals))
}


