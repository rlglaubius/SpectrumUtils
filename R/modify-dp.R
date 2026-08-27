#' Set ART coverage in the .DP file (tags: <HAARTBySex MV>, <ChildTreatInputs MV3>)
#' @param pjnz.file Spectrum file (.pjnz)
#' @param first.year First year in the DP time series (e.g., 1970)
#' @param final.year Final year in the DP time series (e.g., 2046)
#' @param first.year.art Year to begin ART coverage change
#' @param final.year.art Year to end ART coverage change
#' @param population Population(s) to set ART coverage for: "Male", "Female", and/or "Child"
#' @param first.art.cov ART coverage (%) at `first.year.art`, for each `population`
#' @param final.art.cov ART coverage (%) at `final.year.art`, for each `population`
#' @param overwrite If TRUE, overwrite input pjnz.file; else write to `name`
#' @param name Output filename when overwrite = FALSE
#' @export
set.dp.inputs.art = function(pjnz.file,
                              first.year, final.year,
                              first.year.art, final.year.art,
                              population,
                              first.art.cov, final.art.cov,
                              overwrite = FALSE,
                              name) {

  filename = basename(pjnz.file)
  cat(paste0("\033[31m", "Modifying ", filename, "\033[0m\n"))

  # Checks
  stopifnot(final.year > first.year)
  stopifnot(final.year.art >= first.year.art)
  stopifnot(first.year.art >= first.year, final.year.art <= final.year)

  # Coverage values within [0, 100]
  stopifnot(all(first.art.cov >= 0 & first.art.cov <= 100))
  stopifnot(all(final.art.cov >= 0 & final.art.cov <= 100))

  art.map = data.frame(pop = population, first.art.cov, final.art.cov)
  # Range to interpolate  between first and final year of art coverage
  art.map$art.range  = final.year.art - first.year.art + 1
  # Interpolate art coverage between `first.art.cov` and `final.art.cov`
  art.map$art.range.cov = mapply(
    function(a, b, n) seq(a, b, length.out = n),
    art.map$first.art.cov, art.map$final.art.cov, art.map$art.range, SIMPLIFY = FALSE)
  # If `final.year.art` is not equal to `final.year`, carry `final.art.cov`
  # through to end of timeseries
  art.map$years.to.end = final.year - final.year.art
  art.map$art.range.cov = mapply(
    function(a, b, n) if(n !=0){a = c(a, rep(b, times = n))}else{a},
    art.map$art.range.cov, art.map$final.art.cov, art.map$years.to.end, SIMPLIFY = FALSE)

  # Read in .DP file
  dp = read.raw.dp(pjnz.file)

  # Set ART coverage for adults
  # Get row and col indexes
  offset = 4
  nrow = 2  # expected rows: Male+Female, Males, Females
  ncol = final.year - first.year + 1

  ind.tag = which(dp[[1]] == "<HAARTBySex MV>")
  row.bgn = ind.tag + offset
  row.end = row.bgn + nrow - 1
  rows = seq.int(row.bgn, row.end)
  # Column index from `first.year` to `final.year`
  cols = 4:(ncol + 3)
  # Column index from `first.year.art` to `final.year`
  art.cols = (4 + (first.year.art - first.year)):(ncol + 3)

  # Set ART for adults
  if("Male" %in% art.map$pop){
    male.cov = art.map[art.map$pop == "Male",]$art.range.cov[[1]]
    range = art.map[art.map$pop == "Male",]$art.range
    dp[rows[1], art.cols] = male.cov
    print(paste0("Male ART cov updated to ", male.cov[1],
                 "%-", male.cov[range], "% for ",
                 first.year.art, "-", final.year.art))
  }

  if("Female" %in% art.map$pop){
    female.cov = art.map[art.map$pop == "Female",]$art.range.cov[[1]]
    range = art.map[art.map$pop == "Female",]$art.range
    dp[rows[2], art.cols] = female.cov
    print(paste0("Female ART cov updated to ", female.cov[1], "%-",
                 female.cov[range], "% for ",
                 first.year.art, "-", final.year.art))
  }

  if("Child" %in% art.map$pop){

    # Set ART coverage for Child
    # Get row and col indexes
    offset = 3
    ncol = final.year - first.year + 1

    ind.tag = which(dp[[1]] == "<ChildTreatInputs MV3>")
    row.bgn = ind.tag + offset

    # Set ART for Child
    child.cov = art.map[art.map$pop == "Child",]$art.range.cov[[1]]
    range = art.map[art.map$pop == "Child",]$art.range
    dp[row.bgn, art.cols] = child.cov
    print(paste0("Child ART cov updated to ", child.cov[1], "%-",
                 child.cov[range], "% for ",
                 first.year.art, "-", final.year.art))

  }

  # Unzip all existing files in PJNZ to tmpdir
  tmp_dir = tempfile()
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(pjnz.file, exdir = tmp_dir)

  # Overwrite .DP with updated DP
  files = list.files(tmp_dir)
  dp_path = files[grepl("\\.DP$", files, ignore.case = TRUE)]
  out_dp = file.path(tmp_dir, dp_path)
  dir.create(dirname(out_dp), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(dp, out_dp, quote = FALSE, na = "", row.names = FALSE,
                     sep = ",", fileEncoding = "UTF-8")

  # Create a tmp pjnz
  tmp_pjnz = tempfile(fileext = ".PJNZ")
  zip::zip(zipfile = tmp_pjnz, files = files, root = tmp_dir)

  if(overwrite){
    if(file.copy(tmp_pjnz, pjnz.file, overwrite = TRUE)){
      cat(paste0("\033[31m", pjnz.file, " updated.\033[0m\n"))
    } else{
      cat(paste0("\033[31m Failed to overwrite: ", pjnz.file, "\033[0m\n"))
    }
  } else{
    if(file.copy(tmp_pjnz, name, overwrite = TRUE)){
      cat(paste0("\033[31m", "New file saved at: ", name, "\033[0m\n"))
    } else{
      cat(paste0("\033[31m Failed to write new file to: ", name, "\033[0m\n"))
    }
  }





}

