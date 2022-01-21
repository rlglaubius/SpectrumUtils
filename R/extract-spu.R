#' Read HIV uncertainty data
#'
#' Uncertainty around HIV incidence and prevalence for AIM is sampled using
#' tools like EPP and CSAVR. These tools produce store the uncertainty data in
#' an SPU file within a projection. An SPU file is a CSV file that stores
#' coupled estimates of HIV incidence and prevalence that correspond to
#' underlying sampled model parameter values. \code{read.raw.spu} loads the
#' contents of this file with minimal processing. Other functions, like
#' \code{format.spu} can be used to reformat these data for easier use.
#' @param pjnz.file The Spectrum file to extract data from
#' @return a dataframe with one row per row in the SPU file
#' @examples
#' dp.data = read.data.spu("Antarctica.PJNZ")
#' @export
read.raw.spu = function(pjnz.file) {
  grep.str = "\\.SPU$"
  spu.file = grep(grep.str, unzip(pjnz.file, list=TRUE)$Name, value=TRUE)
  if (length(spu.file) == 0) {
    warning(sprintf("No SPU file in %s", extension, pjnz.file))
    spu.data = NULL
  } else if (length(spu.file) > 1) {
    warning(sprintf("Reading SPU data failed, found %d matching files in %s", extension, length(spu.file), pjnz.file))
    spu.data = NULL
  } else {
    spu.data = read.csv(unz(pjnz.file, spu.file), header=FALSE, stringsAsFactors=FALSE)
  }
  colnames(spu.data)[1] = "Tag" # Strip off UTF BOM
  return(spu.data)
}

#' Reformat SPU data as a data frame
#'
#' Return a data frame of HIV incidence and prevalence estimates. Uncertainty
#' data in SPU files are sampled from some distribution. Sampling may be done
#' with replacement. Formatted SPU data include a sample id column ("Sample"),
#' the sampling frequency ("Frequency"), and Year. If "wide" output is
#' requested, the dataframe will have columns for HIV prevalence and incidence;
#' if "long" output is requested, the dataframe will have an Indicator and Value
#' columns instead. The SPU data includes the best-fitting incidence and
#' prevalence point estimate. This has sample ID 0 and missing (NA) sampling
#' frequency.
#' @param raw.spu SPU data returned by \code{read.raw.spu}
#' @return a dataframe in long or wide format
#' @examples
#' spu = read.raw.spu("Antarctica.PJNZ")
#' dat = spu.indicator.data(spu, direction="long")
#' @export
spu.indicator.data = function(raw.spu, direction="wide") {
  ## Data for each sampled (incidence, prevalence) pair is preceded by a row
  ## "COUNT <n>". We use these separator rows to assigned unique id numbers to
  ## each sample.
  dat = dplyr::mutate(raw.spu, Sample=cumsum(grepl("COUNT", raw.spu[,1])))

  ## The "COUNT" rows tell us how frequently the sampled (incidence, prevalence)
  ## pair should appear in the posterior sample. Pull out this frequency.
  frq = dat[grep("COUNT", dat[,1]),]
  frq$Frequency = as.numeric(gsub("COUNT ", "", frq[,1]))

  ## Restrict dat to rows of raw.spu with incidence and prevalence data
  dat = dat[is.finite(dat[,3]),]
  colnames(dat) = c("Year", "Prevalence", "Incidence", "Sample")

  ## Merge in sampling frequencies
  dat = dplyr::left_join(dat, frq[,c("Sample", "Frequency")], by=c("Sample"))

  if (direction == "wide") {
    return(dat[,c("Sample", "Frequency", "Year", "Prevalence", "Incidence")])
  } else {
    return(reshape2::melt(dat,
                          id.vars=c("Sample", "Frequency", "Year"),
                          value.name="Value", variable.name="Indicator"))
  }
}
