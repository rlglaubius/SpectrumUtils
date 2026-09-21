#' Set the reduction in HIV transmission on ART in Goals RSM (.HV file, tag: <InfectMultiplierOnART MV>)
#' @param pjnz.file Spectrum file (.pjnz)
#' @param first.year First year in the HV time series
#' @param final.year Final year in the HV time series
#' @param first.year.effect Year to begin the ART effect change
#' @param final.year.effect Year to end the ART effect change
#' @param first.effect ART infectiousness multiplier at `first.year.effect`
#' @param final.effect ART infectiousness multiplier at `final.year.effect`
#' @param overwrite If TRUE, overwrite input pjnz.file; else write to `name`
#' @param name Output filename when overwrite = FALSE
#' @export
set.hv.inputs.art.effect = function(pjnz.file,
                                     first.year, final.year,
                                     first.year.effect, final.year.effect,
                                     first.effect, final.effect,
                                     overwrite = FALSE,
                                     name) {

  filename = basename(pjnz.file)
  cat(paste0("\033[31m", "Modifying ", filename, "\033[0m\n"))

  # Checks
  stopifnot(final.year > first.year)
  stopifnot(final.year.effect >= first.year.effect)
  stopifnot(first.year.effect >= first.year, final.year.effect <= final.year)
  stopifnot(first.effect >= 0, final.effect >= 0)

  # Interpolate the ART effect between `first.effect` and `final.effect`
  effect.range = final.year.effect - first.year.effect + 1
  effect.range.vals = seq(first.effect, final.effect, length.out=effect.range)
  # If `final.year.effect` is not equal to `final.year`, carry `final.effect`
  # through to end of timeseries
  years.to.end = final.year - final.year.effect
  if (years.to.end != 0) {
    effect.range.vals = c(effect.range.vals, rep(final.effect, years.to.end))
  }

  # Read in .HV file
  hv = read.raw.hv(pjnz.file)

  # Set ART effect
  # Get row and col indexes
  offset = 4
  ncol = final.year - first.year + 1
  ind.tag = which(hv[[1]] == "<InfectMultiplierOnART MV>")
  row = ind.tag + offset
  col.bgn = which(colnames(hv) == "Data")
  effect.cols = (col.bgn + (first.year.effect - first.year)):(col.bgn + ncol - 1)

  hv[row, effect.cols] = effect.range.vals
  print(paste0("ART effect updated to ", first.effect, "-", final.effect,
               " for ", first.year.effect, "-", final.year.effect))

  # Unzip all existing files in PJNZ to tmpdir
  tmp_dir = tempfile()
  dir.create(tmp_dir, recursive=TRUE, showWarnings=FALSE)
  utils::unzip(pjnz.file, exdir=tmp_dir)

  # Overwrite .HV with updated HV
  files = list.files(tmp_dir)
  hv_path = files[grepl("\\.HV$", files, ignore.case=TRUE)]
  out_hv = file.path(tmp_dir, hv_path)
  dir.create(dirname(out_hv), recursive=TRUE, showWarnings=FALSE)
  utils::write.table(hv, out_hv, quote=FALSE, na="", row.names=FALSE,
                      sep=",", fileEncoding="UTF-8")

  # Create a tmp pjnz
  tmp_pjnz = tempfile(fileext=".PJNZ")
  zip::zip(zipfile=tmp_pjnz, files=files, root=tmp_dir)

  if (overwrite) {
    if (file.copy(tmp_pjnz, pjnz.file, overwrite=TRUE)) {
      cat(paste0("\033[31m", pjnz.file, " updated.\033[0m\n"))
    } else {
      cat(paste0("\033[31m Failed to overwrite: ", pjnz.file, "\033[0m\n"))
    }
  } else {
    if (file.copy(tmp_pjnz, name, overwrite=TRUE)) {
      cat(paste0("\033[31m", "New file saved at: ", name, "\033[0m\n"))
    } else {
      cat(paste0("\033[31m Failed to write new file to: ", name, "\033[0m\n"))
    }
  }
}
