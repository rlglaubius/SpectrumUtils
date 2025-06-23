#' Load Spectrum Goal ASM module data
#'
#' Read Spectrum Goals ASM module data as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @return an unformatted table of module data
#' @examples
#' ha.data = read.data.ha("Antarctica.PJNZ")
#' @export
read.raw.ha = function(pjnz.file) {
  raw = read.module.data(pjnz.file, extension="HA")

  ## Goals ASM tags are stored in HAModData. Many of these include a leading
  ## whitespace. This would cause extract.raw.tag() to fail unless the
  ## whitespace is passed to extract.raw.tag(tag=tag_name) as part of tag_name.
  ## Rather than do that, and cause SpectrumUtils to break if the whitespace is
  ## ever stripped from the tag, we instead just strip leading and trailing
  ## whitespace from all tags here to preempt any such change.
  raw$Tag = trimws(raw$Tag)
  return(raw)
}

#' @noRd
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

#' Extract inputs listed in the Goals ASM "Configure" form
#'
#' Extract secondary school gross enrollment rates, secondary pupil-teacher
#' ratios, frequency of teacher re-training, numbers of sex acts per partner,
#' and condom wastage parameters.
#' @inheritParams ha.inputs.first.year
#' @return a data frame
#' @export
ha.inputs.configuration = function(ha.raw, direction="wide") {
  fmt_1 = list(cast=as.numeric, offset=1, nrow=5, ncol=1)
  fmt_2 = list(cast=as.numeric, offset=1, nrow=1, ncol=1)
  raw_1 = extract.ha.tag(ha.raw, "<Config Costing>", fmt_1)
  raw_2 = extract.ha.tag(ha.raw, "<Condom Wastage>", fmt_2)

  names = c("AGYW: secondary school gross enrollment rate",
            "ABYM: secondary school gross enrollment rate",
            "Secondary pupil-teacher ratio",
            "Frequency of teacher re-training (years)",
            "Number of sex acts per partner",
            "Condom wastage during storage and distribution")
  rval = data.frame(Parameter = names,
                    Value = c(raw_1[c(1,2,4,5,3)], raw_2)) # reorder to match Spectrum form
  return(rval)
}

#' Extract inputs listed in the Goals ASM "Epidemiology" form
#'
#' Extract transmission probabilities by HIV-positive partner's sex
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{ha.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{ha.inputs.final.year()}
#' @return a data frame
#' @export
ha.inputs.transmission = function(ha.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=3)
  raw = extract.ha.tag(ha.raw, " <TranmRel>", fmt) # This tag has a leading space in .HA file
  val = raw[1,2:3] # There is a blank cell before the model inputs
  if (direction=="wide") {
    dat = data.frame(matrix(val, nrow=1))
    colnames(dat) = strata.labels$sex
  } else {
    dat = data.frame(Sex=strata.labels$sex, Value=val)
  }
  return(dat)
}

#' Extract Goals ASM behavioral inputs
#' @describeIn ha.inputs.lifetime.partners Time trend in numbers of lifetime partners
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{ha.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{ha.inputs.final.year()}
#' @return a data frame
#' @section Details:
#'
#' \itemize{
#' \item{\code{ha.inputs.partner.age.par} Returns the peak and median in relative rates of
#' partner change by age after sexual debut. Relative rates of partner change at age a are
#' calculated as dlnorm(a+0.5-debut, logmu, logsd) where logmu = median-debut and logsd=sqrt(logmu - ln(peak - debut))}
#' \item{\code{ha.inputs.mixing.age}} Returns mixing preferences of females by male age. In wide format, rows correspond to male age, columns to female age.
#' }
#'
#' @export
ha.inputs.lifetime.partners = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  fmt = list(cast=as.numeric, offset=3, nrow=1, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, " <Partner Trend>", fmt) # This tag has a leading space in .HA file
  if (direction=="wide") {
    dat = data.frame(matrix(raw, nrow=1))
    colnames(dat) = sprintf("%d", first.year:final.year)
  } else {
    dat = data.frame(Year = first.year:final.year, Value=matrix(raw, ncol=1))
  }
  return(dat)
}

#' @describeIn ha.inputs.lifetime.partners Partner change rates by age in the last year of projection
#' @export
ha.inputs.partner.age = function(ha.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=81, ncol=1)
  raw = extract.ha.tag(ha.raw, " <Partner Age Trend>", fmt) # This tag has a leading space in .HA file
  dat = data.frame(Age=0:80, Value=raw)
  return(dat)
}

#' @describeIn ha.inputs.lifetime.partners Parameters describing relative rates of partner change by age
#' @export
ha.inputs.partner.age.par = function(ha.raw, direction="wide") {
  fmt1 = list(cast=as.numeric, offset=1, nrow=1, ncol=1)
  fmt2 = list(cast=as.numeric, offset=2, nrow=1, ncol=3)
  raw_peak = extract.ha.tag(ha.raw, "<Age Peak Activity>",   fmt1)
  raw_half = extract.ha.tag(ha.raw, "<Age Median Activity>", fmt1)
  raw_debut = extract.ha.tag(ha.raw, " <Sex Debut>", fmt2)
  dat = data.frame(Parameter=c("Age at sexual debut, male", "Age at sexual debut, female", "Age of peak sexual activity", "Age of median sexual activity"),
                   Value = c(raw_debut[1,2:3], raw_peak, raw_half))
  return(dat)
}

#' @describeIn ha.inputs.lifetime.partners Mixing preferences by age
#' @export
ha.inputs.mixing.age = function(ha.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=1, nrow=81, ncol=81)
  raw = extract.ha.tag(ha.raw, " <Partner Choice>", fmt) # This tag has a leading space in .HA file
  dat = data.frame(Age=0:80, Value=raw)
  colnames(dat) = c("Male age", 0:80)
  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars="Male age", variable.name="Female age", value.name="Value")
    dat$`Female age` = as.numeric(as.character(dat$`Female age`))
  }
  return(dat)
}

#' HIV program coverage inputs
#'
#' @param ha.raw Goals ASM module data in raw format, as returned by
#'   \code{read.raw.ha()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection. If \code{first.year=NULL}, it
#'   will be filled in using \code{ha.inputs.first.year()}
#' @param final.year Final year of the projection. If \code{final.year=NULL}, it
#'   will be filled in using \code{ha.inputs.final.year()}
#' @describeIn ha.inputs.condom.use Condom use inputs by year and age group
#' @return a data frame
#' @export
ha.inputs.condom.use = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  fmt = list(cast=as.character, offset=1, nrow=19, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, " <Condom By Age>", fmt) # This tag has a leading space in .HA file
  val = matrix(as.numeric(raw[c(4,9,14,19),]), nrow=length(strata.labels$age.cd4.adult)) # Extracts just time trends, not the logistic curve parameters
  dat = cbind(strata.labels$age.cd4.adult, data.frame(val))
  colnames(dat) = c("Age", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn ha.inputs.condom.use Input levels of viral suppression on ART by year, sex, and age group
#' @export
ha.inputs.viral.suppression = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  fmt = list(cast=as.numeric, offset=1, nrow=8, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, "<ARTViralSuppression>", fmt)
  dat = cbind(Sex = rep(strata.labels$sex, each=4),
              Age = rep(strata.labels$age.cd4.adult, 2),
              data.frame(raw))
  colnames(dat) = c("Sex", "Age", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
}

#' @describeIn ha.inputs.condom.use Input male circumcision prevalence by year and age group
#' @export
ha.inputs.male.circumcision = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  ## The block contains a mix of strings and numbers, so we extract as-is using
  ## cast=I to suppressed cast-to-NA warnings. Then we extract the numeric data
  ## and cast-to-numeric manually.
  ind_age = seq(1,31,5)
  fmt = list(cast=I, offset=1, nrow=34, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, "<Male Circumcision By Age>", fmt)
  raw = raw[ind_age + 3,] # Extract the rows with annual data.
  val = matrix(as.numeric(raw), nrow=nrow(raw))
  dat = cbind(Age = strata.labels$age.5yr[4:10], data.frame(val))
  colnames(dat) = c("Age", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Age"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }
  return(dat)
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

#' Data for model calibration
#' @describeIn ha.inputs.fitdata.trend HIV incidence or prevalence data points
#' @inheritParams ha.inputs.first.year
#' @return a data frame.
#' @export
ha.inputs.fitdata.trend = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  ## <Fit Data> has dynamic size, so we do an initial read to get the number of
  ## rows of data. <Fit Data> also violates some rules about how data should be
  ## written to the PJNZ, so we need custom code to parse this data.
  tag = "<Fit Data>"
  ind_tag = dplyr::first(which(ha.raw$Tag == tag))
  nrow = as.numeric(ha.raw[ind_tag+1,3])

  row_bgn = ind_tag + 2
  row_end = row_bgn + nrow - 1
  col_bgn = 2
  col_end = 10

  raw = ha.raw[row_bgn:row_end, col_bgn:col_end]
  dat = as.data.frame(matrix(as.numeric(unlist(raw)), nrow=row_end - row_bgn + 1, ncol=col_end - col_bgn + 1))
  colnames(dat) = c("Population", "Sex", "Year", "Value", "Lower", "Upper", "N", "UseInFit", "Type")
  dat = dplyr::mutate(dat,
                      Population = factor(Population, levels=-1:1, labels=c("None", "Adults (15-49)", "Young people (15-24)")),
                      Sex        = factor(Sex,        levels=0:2,  labels=strata.labels$sex.aug),
                      Type       = factor(Type,       levels=-1:1, labels=c("None", "Incidence", "Prevalence")))

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

#' Get estimates of financial resource needs for HIV programming
#' @describeIn ha.output.population.reached Get the number of people reached by HIV interventions
#' @inheritParams ha.output.transmitted
#' @return a data frame
#' @export
ha.output.population.reached = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  lab_sex  = strata.labels$sex.aug
  lab_age  = c("15+", strata.labels$age.cd4.adult)
  lab_prog = strata.labels$ha.programs

  num_sex  = length(lab_sex)
  num_age  = length(lab_age)
  num_prog = length(lab_prog)

  fmt = list(cast=as.numeric, offset=4, nrow=num_sex * num_age * num_prog, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, "<Population Reached>", fmt)
  dat = cbind(expand.grid(Program=lab_prog, Age=lab_age, Sex=lab_sex), data.frame(raw))
  colnames(dat) = c("Program", "Age", "Sex", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age", "Program"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}

#' @describeIn ha.output.population.reached Get the financial resource needs for each HIV intervention
#' @inheritParams ha.output.transmitted
#' @return a data frame
#' @export
ha.output.resources.required = function(ha.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = ha.inputs.first.year(ha.raw)}
  if (is.null(final.year)) {final.year = ha.inputs.final.year(ha.raw)}

  lab_sex  = strata.labels$sex.aug
  lab_age  = c("15+", strata.labels$age.cd4.adult)
  lab_prog = strata.labels$ha.programs

  num_sex  = length(lab_sex)
  num_age  = length(lab_age)
  num_prog = length(lab_prog)

  fmt = list(cast=as.numeric, offset=4, nrow=num_sex * num_age * num_prog, ncol=final.year-first.year+1)
  raw = extract.ha.tag(ha.raw, "<Result Costs>", fmt)
  dat = cbind(expand.grid(Program=lab_prog, Age=lab_age, Sex=lab_sex), data.frame(raw))
  colnames(dat) = c("Program", "Age", "Sex", sprintf("%d", first.year:final.year))
  if (direction == "long") {
    dat = reshape2::melt(dat, id.vars=c("Sex", "Age", "Program"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}


