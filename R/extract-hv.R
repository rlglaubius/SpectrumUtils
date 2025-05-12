read.module.data = function(pjnz.file, extension="HV") {
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

extract.raw.tag = function(mod.raw, tag, fmt) {
  ind.tag = dplyr::first(which(mod.raw$Tag == tag))
  if (is.na(ind.tag)) {
    return(NULL)
  }

  row.bgn = ind.tag + fmt$offset
  row.end = row.bgn + fmt$nrow - 1
  col.bgn = which(colnames(mod.raw) == "Data")
  col.end = col.bgn + fmt$ncol - 1

  raw.data = unlist(mod.raw[row.bgn:row.end, col.bgn:col.end])
  return(matrix(fmt$cast(raw.data), nrow=fmt$nrow, ncol=fmt$ncol))
}

read.raw.hv = function(pjnz.file) {
  return(read.module.data(pjnz.file, extension="HV"))
}

#' @noRd
extract.hv.tag = function(hv.raw, tag, fmt) {
  fmt$is.modvar = TRUE
  val = extract.raw.tag(hv.raw, tag, fmt)
  if (is.null(val)) {
    val = matrix(NA, nrow=fmt$nrow, ncol=fmt$ncol)
  }
  return(val)
}

hv.inputs.first.year = function(hv.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<FirstYear MV>", fmt)[1,1])
}

hv.inputs.final.year = function(hv.raw, direction="wide") {
  fmt = list(cast=as.numeric, offset=2, nrow=1, ncol=1)
  return(extract.hv.tag(hv.raw, "<FinalYear MV>", fmt)[1,1])
}

hv.inputs.art.effect = function(hv.raw, direction="wide", first.year=NULL, final.year=NULL) {
  if (is.null(first.year)) {first.year = hv.inputs.first.year(hv.raw)}
  if (is.null(final.year)) {final.year = hv.inputs.final.year(hv.raw)}

  fmt = list(cast=as.numeric, offset=4, nrow=1, ncol=final.year-first.year+1)
  raw = extract.hv.tag(hv.raw, "<InfectMultiplierOnART MV>", fmt)
  if (direction=="long") {
    dat = data.frame(Year = first.year:final.year, Value=raw[1,])
  } else {
    dat = data.frame(raw)
    colnames(dat) = sprintf("%d", first.year:final.year)
  }
  return(dat)
}
