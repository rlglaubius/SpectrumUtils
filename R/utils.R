#' Load Spectrum module data
#'
#' Read Spectrum module data as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @param extension Module data file name extension
#' @return an unformatted table of module data
#' @examples
#' dp.data = read.module.data("Antarctica.PJNZ", extension="DP")
#' @export
read.module.data = function(pjnz.file, extension="DP") {
  grep.str = sprintf("\\.%s$", extension)
  mod.file = grep(grep.str, unzip(pjnz.file, list=TRUE)$Name, value=TRUE)
  if (length(mod.file) == 0) {
    warning(sprintf("No data for module '%s' in %s", extension, pjnz.file))
    mod.data = NULL
  } else if (length(mod.file) > 1) {
    warning(sprintf("Reading data for module '%s' failed, found %d matching files in %s", extension, length(mod.file), pjnz.file))
    mod.data = NULL
  } else {
    mod.data = read.csv(unz(pjnz.file, mod.file), stringsAsFactors=FALSE)
  }
  colnames(mod.data)[1] = "Tag" # Strip off UTF BOM
  return(mod.data)
}

#' Extract specific Spectrum data
#'
#' Read raw Spectrum data from a given module
#' @param mod.raw Raw module data returned by read.module.data or
#'   module-specific functions
#' @param tag A module variable name
#' @param fmt Format parameters. See "Details"
#' @return an the variable data as a matrix.
#'
#' @section Details:
#'
#'   Spectrum module data uses either "modvar" or non-modvar formats.
#'
#'   modvar formats group data into blocks delimited by a starting tag (e.g.
#'   <FirstYear MV2>) and matching "<End>" tag. Starting tags have an embedded
#'   version number (e.g. "MV2"; "MV" for version 1) to accommodate the
#'   evolution of Spectrum file formats over time.
#'
#'   non-modvar formats also include a starting tag (e.g. "<Projection Name>")
#'   but no embedded version number, and no matching "<End>" tag.
#'
#'   The \code{fmt} parameter is a named list that specifies the module data
#'   format and variable layout that should contain the following:
#'
#'   \code{fmt$is.modvar} is \code{TRUE} for modvar format modules and
#'   \code{FALSE} otherwise.
#'
#'   \code{fmt$offset}: the number of rows between the starting tag and the
#'   first row of data.
#'
#'   \code{fmt$nrow}: Number of rows of data, relative to the starting tag row
#'   number + \code{fmt$offset}
#'
#'   \code{fmt$ncol}: Number of columns of data, relative to mod.raw$Data.
#'
#'   \code{fmt$cast} is a function used to cast the data to a desired type. For
#'   example, set \code{fmt$type=as.numeric} to return numeric data.
#' @export
extract.raw.tag = function(mod.raw, tag, fmt) {
  ind.tag = dplyr::first(which(mod.raw$Tag == tag))
  row.bgn = ind.tag + fmt$offset
  row.end = row.bgn + fmt$nrow - 1
  col.bgn = which(colnames(mod.raw) == "Data")
  col.end = col.bgn + fmt$ncol - 1

  raw.data = unlist(mod.raw[row.bgn:row.end, col.bgn:col.end])
  return(matrix(fmt$cast(raw.data), nrow=fmt$nrow, ncol=fmt$ncol))
}

#' Read Spectrum projection parameters
#'
#' Read Spectrum projection parameters as an unformatted table
#' @param pjnz.file The Spectrum file to extract data from
#' @return an unformatted table of projection data
#' @examples
#' dp.data = read.raw.pjn("Antarctica.PJNZ")
#' @export
read.raw.pjn = function(pjnz.file) {
  return(read.module.data(pjnz.file, "PJN"))
}

extract.pjn.tag = function(pjn.raw, tag, fmt) {
  fmt$is.modvar = FALSE
  return(extract.raw.tag(pjn.raw, tag, fmt))
}

#' Read Spectrum geographic metadata
#'
#' Extract the country ISO-3166 numeric code and subnational unit name and id
#' number from Spectrum projection data
#' @param pjn.raw Raw projection parameter data, as returned by
#'   \code{read.raw.pjn()}
#' @return A data frame with three elements:
#'
#'   \code{iso.code} ISO-3166 numeric country code
#'
#'   \code{snu.name} Subnational unit name. This is empty for national
#'   projections
#'
#'   \code{snu.code} Subnational numeric code. This is 0 for national
#'   projections
#'
#' @section Details:
#'
#'   Numeric subnational codes were assigned by Avenir Health for internal use
#'   in Spectrum. Unlike national ISO-3166 codes, these subnational codes are
#'   not governed by any international organization and may not match codes used
#'   for similar purposes by other organizations.
#'
#' @export
extract.geo.info = function(pjn.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  iso.raw = extract.pjn.tag(pjn.raw, "<Projection Parameters>", fmt)[1,1]

  fmt = list(cast=as.character, offset=2, nrow=2, ncol=1)
  snu.raw = extract.pjn.tag(pjn.raw, "<Projection Parameters - Subnational Region Name2>", fmt)

  dat = data.frame(iso.code = iso.raw, snu.name = snu.raw[1,1], snu.code = as.numeric(snu.raw[2,1]))
  return(dat)
}



