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

#' Resource needs module (RNM) PrEP cover input
#' @param rn.raw RNM module data in raw format, as returned by
#'   \code{read.raw.rn()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection.
#' @param final.year Final year of the projection.
#' @return A data frame.
#' @export
rn.inputs.prep.coverage= function(rn.raw, direction="wide", first.year, final.year) {
  fmt = list(cast=as.numeric, offset=3, nrow=13, ncol=final.year-first.year+1)
  raw = data.frame(extract.rn.tag(rn.raw, "<PrEPCoverage MV>", fmt))
  colnames(raw) = c(first.year:final.year)

  dat <- raw
  dat$sex <- c(rep("Male",9), rep("Female", 4))
  dat$risk_group <- c(2:10, 2:5)
  dat$risk_group <- strata.labels$risk.groups[as.character(dat$risk_group)]
  dat <- dat |> dplyr::select(sex, risk_group, dplyr::everything())

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("sex", "risk_group"),
                         variable.name = "year",
                         value.name = "value")
    dat$year = as.numeric(as.character(dat$year))
  }

  return(dat)
}


#' Resource needs module (RNM) PrEP method mix input
#' @param rn.raw RNM module data in raw format, as returned by
#'   \code{read.raw.rn()}
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection.
#' @param final.year Final year of the projection.
#' @return A data frame.
#' @export
rn.inputs.prep.method.mix = function(rn.raw, direction="wide", first.year, final.year) {
  fmt = list(cast=as.numeric, offset=3, nrow=121, ncol=final.year-first.year+4)
  raw = data.frame(extract.rn.tag(rn.raw, "<MethodMix MV5>", fmt))
  colnames(raw) = c("risk_group", "sex", "prep_method",first.year:final.year)

  dat <- raw
  dat$sex <- c("Male", "Female")[match(dat$sex, c(1,2))]
  dat$risk_group <- strata.labels$risk.groups[as.character(dat$risk_group)]
  dat$prep_method <- strata.labels$prep.methods[as.character(dat$prep_method)]
  attr(dat, "label") <- NULL


  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("sex", "risk_group", "prep_method"),
                         variable.name = "year",
                         value.name = "value")
    dat$year = as.numeric(as.character(dat$year))
  }

  return(dat)
}

