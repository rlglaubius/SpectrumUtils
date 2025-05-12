#' Load Spectrum Resource Needs module data
#'
#' Read Spectrum Resource Needs module data as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @return an unformatted table of module data
#' @examples
#' rn.data = read.data.rn("Antarctica.PJNZ")
#' @export
read.raw.rn = function(pjnz.file) {
  return(SpectrumUtils:::read.module.data(pjnz.file, extension="RN"))
}

#' @noRd
extract.rn.tag = function(rn.raw, tag, fmt) {
  fmt$is.modvar = TRUE
  val = SpectrumUtils:::extract.raw.tag(rn.raw, tag, fmt)
  if (is.null(val)) {
    val = matrix(NA, nrow=fmt$nrow, ncol=fmt$ncol)
  }
  return(val)
}

strata.labels = list(
  rnm.programs = c( # TODO: Add to extract-const.R in SpectrumUtils
    "General population: Community mobilization",
    "General population: Mass media",
    "General population: Voluntary counseling and testing",
    "General population: Condom provision",
    "General population: Primary students with teachers trained in AIDS",
    "General population: Secondary students with teachers trained in AIDS",
    "General population: Out-of-school youth reached",
    "General population: Young women and girls (15-24) receiving cash transfers",
    "General population: Workforce receiving STI treatment", # NOTE: May not be active in UI, but is present in .RN
    "Key populations: Female sex workers reached by intervention",
    "Key populations: Male sex workers reached by intervention",
    "Key populations: MSMs reached by intervention per year",
    "Key populations: MSMs receiving lubricants",
    "Key populations: PWID receiving harm reduction intervention",
    "Key populations: PWID receiving counseling and testing",
    "Key populations: PWID receiving community outreach and peer education",
    "Key populations: PWID receiving needle and syringe exchange",
    "Key populations: PWID receiving drug substitution",
    "Male circumcision: Males 15-49 circumcised",
    "Male circumcision: Infant males circumcised", # NOTE: Not active in UI, but is present in .RN
    "Medical services: Males with STI receiving treatment",
    "Medical services: Females with STI receiving treatment",
    "Medical services: Units of blood for transfusion tested",
    "Medical services: Post-exposure prophylaxis need that is met",
    "Medical services: Unsafe injections replaced with AD syringes", # NOTE: AD=auto-destruct.
    "Medical services: Reduction in number of other injections",
    "Medical services: Hospital beds covered by universal precautions")
)

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
  dat = cbind(Program=strata.labels$rnm.programs, raw)
  dat$ID = NULL

  if (direction=="long") {
    dat = reshape2::melt(dat, id.vars=c("Program"), variable.name="Year", value.name="Value")
    dat$Year = as.numeric(as.character(dat$Year))
  }

  return(dat)
}



## NOTE: I hope this works, but am not sure if path.expand('~') will do the right thing on Parallels
pjnz_path = sprintf("%s/../Avenir Health Dropbox/Avenir Shared Drive/Projects/Models/Goals/2025/HIV Control Modelling/Goals files", path.expand("~"))
pjnz_name = "Kenya 2024.PJNZ"
pjnz_full = sprintf("%s/%s", pjnz_path, pjnz_name)

rn = read.raw.rn(pjnz_full)
dp = read.raw.dp(pjnz_full)

year_first = dp.inputs.first.year(dp)
year_final = dp.inputs.final.year(dp)
coverage = rn.inputs.coverage(rn, direction="long", first.year=year_first, final.year=year_final)
