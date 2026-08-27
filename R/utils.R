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
  mod.file = grep(grep.str, utils::unzip(pjnz.file, list=TRUE)$Name, value=TRUE)
  if (length(mod.file) == 0) {
    warning(sprintf("No data for module '%s' in %s", extension, pjnz.file))
    mod.data = NULL
  } else if (length(mod.file) > 1) {
    warning(sprintf("Reading data for module '%s' failed, found %d matching files in %s", extension, length(mod.file), pjnz.file))
    mod.data = NULL
  } else {
    mod.data = read.csv(unz(pjnz.file, mod.file), quote = "",
                        fill = TRUE, stringsAsFactors=FALSE)
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
#' @return A matrix storing the module variable values.
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
#'   \code{fmt$offset_col}: (optional) the first row with data. If this is
#'   omitted, the "Data" column is used. Since almost all tag data starts in
#'   the "Data" column, \code{offset_col} usually is not needed.
#'
#'   \code{fmt$ncol}: Number of columns of data, relative to mod.raw$Data.
#'
#'   \code{fmt$cast} is a function used to cast the data from a string to a
#'   desired type. For example, set \code{fmt$type=as.numeric} to return numeric
#'   data.
#'
#' @usage extract.raw.tag(mod.raw, tag, fmt)
#' @export extract.raw.tag
extract.raw.tag = function(mod.raw, tag, fmt) {
  ind.tag = dplyr::first(which(mod.raw$Tag == tag))
  if (is.na(ind.tag)) {
    return(NULL)
  }

  row.bgn = ind.tag + fmt$offset
  row.end = row.bgn + fmt$nrow - 1

  if (is.null(fmt$offset_col)) {
    col.bgn = which(colnames(mod.raw) == "Data")
  } else {
    col.bgn = fmt$offset_col
  }
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

#' @noRd
extract.pjn.tag = function(pjn.raw, tag, fmt) {
  fmt$is.modvar = FALSE
  return(extract.raw.tag(pjn.raw, tag, fmt))
}

#' Read the projection name
#' @param pjn.raw Raw projection parameter data, as returned by
#'   \code{read.raw.pjn()}
#' @return The projection name. This will usually match the PJNZ file name. This
#'   may not be true if the PJNZ is renamed manually, such as by renaming it in
#'   Windows Explorer.
#'
#' @usage extract.proj.name(pjn.raw)
#' @export extract.proj.name
extract.proj.name = function(pjn.raw) {
  fmt = list(cast=as.character, offset=2, nrow=1, ncol=1)
  dat = extract.pjn.tag(pjn.raw, "<Projection Name>", fmt)[1,1]
  return(dat)
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
#' @usage extract.geo.info(pjn.raw)
#' @export extract.geo.info
extract.geo.info = function(pjn.raw) {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  iso.raw = extract.pjn.tag(pjn.raw, "<Projection Parameters>", fmt)[1,1]

  fmt = list(cast=as.character, offset=2, nrow=2, ncol=1)
  snu.raw = extract.pjn.tag(pjn.raw, "<Projection Parameters - Subnational Region Name2>", fmt)

  dat = data.frame(iso.code = iso.raw, snu.name = snu.raw[1,1], snu.code = as.numeric(snu.raw[2,1]))
  return(dat)
}
#' Write Spectrum module data back in original format
#' @param data The data frame to write
#' @param file.path The file path to write to
#' @export
write.module.data.tab = function(data, file.path) {
  utils::write.csv(data, file.path,
                   quote = FALSE,
                   na = "",
                   row.names = FALSE,
                   fileEncoding = "UTF-8")
}

## Shared ggplot2 theme for comparison/analysis plots. Text is sized so that
## panel titles, facet strips, and axis labels stay readable when many of
## these plots are saved into a single multi-page PDF. base_size can be
## lowered (e.g. for multi-panel composites) to keep titles from being cut
## off.
#' @noRd
spectrumutils.plot.theme = function(base_size=14) {
  ggplot2::theme_minimal(base_size=base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size=base_size + 4, face="bold"),
      strip.text = ggplot2::element_text(size=base_size - 1, face="bold"),
      axis.title = ggplot2::element_text(size=base_size),
      axis.text = ggplot2::element_text(size=base_size - 3),
      legend.text = ggplot2::element_text(size=base_size - 2),
      legend.title = ggplot2::element_text(size=base_size - 1))
}

## Puts the legend at the bottom of the plot, freeing up horizontal space so
## multi-panel plots and long x-axis year labels stay legible.
#' @noRd
spectrumutils.legend.bottom = function() {
  ggplot2::theme(legend.position="bottom")
}

## Coverage/share values are stored as percentages (0-100, never negative);
## floor the y-axis at 0 and label it with a "%" suffix without rescaling.
#' @noRd
spectrumutils.pct.scale = function() {
  ggplot2::scale_y_continuous(labels=function(x) paste0(x, "%"), limits=c(0, NA))
}

## As spectrumutils.pct.scale(), but the y-axis always covers at least the
## full 0-100% range in 10% increments. Unlike a hard limits=c(0,100), this
## does not clip values above 100% (e.g. a coverage input exceeding 100% due
## to a data issue) -- the axis simply extends further so the anomaly stays
## visible instead of being silently dropped.
#' @noRd
spectrumutils.pct.scale.100 = function() {
  list(
    ggplot2::scale_y_continuous(
      labels=function(x) paste0(x, "%"),
      breaks=function(limits) seq(0, max(100, ceiling(limits[2] / 10) * 10), by=10)),
    ggplot2::expand_limits(y=c(0, 100)))
}

## Forces whole-year x-axis breaks/labels (no decimal years), regardless of
## how narrow the plotted year range is.
#' @noRd
spectrumutils.year.scale = function() {
  ggplot2::scale_x_continuous(
    breaks=function(limits) unique(round(pretty(limits))),
    labels=function(x) formatC(x, format="d"))
}

## The extract.raw.tag() family always reads a data block starting at a
## module's actual first stored year, regardless of what first.year/final.year
## a caller supplies -- those arguments only set how many columns are read
## and how the result is labelled afterward. If a caller's requested range
## doesn't exactly match the file's true range, the result is silently
## misaligned/mislabelled data rather than an error, or a genuine subset of
## the plotted years. This checks a requested range against a file's actual
## first.year/final.year (as reported by dp.inputs.first.year()/
## dp.inputs.final.year() or hv.inputs.first.year()/hv.inputs.final.year())
## and stops with a clear explanation if they don't match exactly.
#' @noRd
spectrumutils.validate.year.range = function(requested.first.year, requested.final.year,
                                              true.first.year, true.final.year, file.label=NULL) {
  if (is.null(requested.first.year) || is.null(requested.final.year)) {return(invisible(NULL))}
  if (requested.first.year == true.first.year && requested.final.year == true.final.year) {return(invisible(NULL))}

  stop(sprintf(paste(
    "first.year/final.year (%d-%d) must exactly match%s the file's own",
    "projection year range (%d-%d). These extraction functions always read",
    "a fixed-width data block starting at the file's actual first year --",
    "a mismatched range silently reads and mislabels the wrong years rather",
    "than filtering the plotted window. Use dp.inputs.first.year(dp.raw)/",
    "dp.inputs.final.year(dp.raw) (or the hv.inputs.* equivalents) to get",
    "the correct values."),
    requested.first.year, requested.final.year,
    if (is.null(file.label)) "" else paste0(" ", file.label, "'s"),
    true.first.year, true.final.year))
}

## Arranges a list of ggplot objects onto a single landscape A4 page and
## saves it as a one-page PDF, for functions' combine.plots=TRUE option.
#' @noRd
spectrumutils.save.combined.pdf = function(plots, file.path) {
  combined = patchwork::wrap_plots(plots)
  ggplot2::ggsave(filename=file.path, plot=combined, width=297, height=210, units="mm")
  return(invisible(file.path))
}
