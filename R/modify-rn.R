#' Set PrEP coverage in Resource needs module (RNM)
#' @param pjnz.file The Spectrum file to extract data from
#' @param direction Request "wide" (default) or "long" format data.
#' @param first.year First year of the projection.
#' @param final.year Final year of the projection.
#' @param first.year.prep Year to begin PrEP coverage
#' @param final.year.prep Year to end PrEP coverage
#' @param first.prep.cov PreP coverage at `first.prep.year`
#' @param final.prep.cov PrEP coverage at `final.prep.year`
#' @param risk.group Risk group(s) to set PrEP coverage for, from `strata.labels$risk.groups`
#' @param zero.prep If TRUE, zero PrEP coverage for all years before applying the coverage map
#' @return A data frame.
#' @export


set.rn.inputs.prep = function(pjnz.file,
                              first.year, final.year,
                              first.year.prep, final.year.prep,
                              first.prep.cov, final.prep.cov,
                              risk.group, sex,
                              prep.method = NULL, prep.method.mix = NULL,
                              zero.prep = TRUE,
                              overwrite = FALSE,
                              name){
  filename = basename(pjnz.file)
  cat(paste0("\033[31m", "Modifying ", filename, "\033[0m\n"))

  # Check options
  stopifnot(final.year > first.year)
  stopifnot(final.year.prep >= first.year.prep)
  stopifnot(first.year.prep >= first.year,
            final.year.prep <= final.year)
  stopifnot(all(sex %in% c("Male", "Female")))
  stopifnot(all(risk.group %in% strata.labels$risk.groups))

  # Coverage values within [0, 100]
  stopifnot(all(first.prep.cov >= 0 & first.prep.cov <= 100))
  stopifnot(all(final.prep.cov >= 0 & final.prep.cov <= 100))

  # Make map to set PrEP coverage by risk group
  make_key = function(x, y) paste(trimws(x), trimws(y), sep = ";")
  risk.group = make_key(sex, risk.group)
  prep.map = data.frame(key = risk.group, first.prep.cov, final.prep.cov)
  # Range to interpolate  between first and final year of PrEP coverage
  prep.map$prep.range  = final.year.prep - first.year.prep + 1
  # Interpolate PrEP coverage between `first.prep.cov` and `final.prep.cov`
  prep.map$prep.range.cov = mapply(
    function(a, b, n) seq(a, b, length.out = n),
    prep.map$first.prep.cov, prep.map$final.prep.cov, prep.map$prep.range, SIMPLIFY = FALSE)
  # If `final.year.prep` is not equal to `final.year`, carry `final.prep.cov`
  # through to end of timeseries
  prep.map$years.to.end = final.year - final.year.prep
  prep.map$prep.range.cov = mapply(
    function(a, b, n) if(n !=0){a = c(a, rep(b, times = n))}else{a},
    prep.map$prep.range.cov, prep.map$final.prep.cov, prep.map$years.to.end, SIMPLIFY = FALSE)

  # Read in .RN file
  rn = read.raw.rn(pjnz.file)


  # Set PrEP coverage
  #  Get row and col indexes
  offset = 3
  nrow=13
  ncol=final.year-first.year+1
  ind.tag = which(rn[[1]] == "<PrEPCoverage MV>")
  row.bgn = ind.tag + offset
  row.end = row.bgn + nrow - 1
  rows = seq.int(row.bgn, row.end)
  # Column index from `first.year` to `final.year`
  cols = 4:(ncol+3)
  # Column index from `first.year.prep` to `final.year`
  prep.cols = (4 +(first.year.prep - first.year)):(ncol+3)

  # If zero.prep = TRUE; Zero PrEP for all years
  if (zero.prep) {
    print("PrEP coverage set to 0% for all years")
    rn[rows, cols] = 0
  }

  # Set coverage levels
  # Add sex category for raw.data
  rows = setNames(rows, c(rep("Male", 9), rep("Female", 4)))

  for (i in seq_along(rows)) {
    row_idx = rows[i]
    sex_label = names(rows)[i]
    risk_label = trimws(rn[row_idx, 3])
    key = make_key(sex_label, risk_label)
    # If row key is specified is sex/age_group -> set PrEP to levels in `map`
    if(key %in% prep.map$key){
      msg = paste(prep.map[prep.map$key == key,]$first.prep.cov, "-",
                   prep.map[prep.map$key == key,]$final.prep.cov, "%",
                   "for", first.year.prep,"-",final.year.prep)
      cov = prep.map[prep.map$key == key,]$prep.range.cov[[1]]
      rn[row_idx, prep.cols] = cov
      print(paste0("PrEP coverage updated - ",key,": ", msg))
    }
  }

  # Set PrEP method mix
  if(!is.null(prep.method)){
    if(!is.list(prep.method)){prep.method = list(prep.method)}
    if(!is.list(prep.method.mix)){prep.method.mix = list(prep.method.mix)}
    print("Updating PreP Method Mix")
    # Check PrEP method mix sums to 100
    stopifnot(all(vapply(prep.method.mix, sum, numeric(1)) == 100))
    stopifnot(all(unlist(prep.method) %in% strata.labels$prep.methods))
    names(prep.method) = risk.group
    names(prep.method.mix) = risk.group
    # PrEP method mix map
    prep.method.map = do.call(rbind, lapply(names(prep.method), function(k) {
      data.frame(key = rep(k, length(prep.method[[k]])),
                 method = prep.method[[k]],
                 mix = prep.method.mix[[k]],
                 stringsAsFactors = FALSE)
    }))
    prep.method.map$key = paste(prep.method.map$key, prep.method.map$method, sep = ";")
    # Get row and col indexes
    ind.tag = which(rn[[1]] == "<MethodMix MV5>")
    offset=3
    nrow=121
    ncol=final.year-first.year +1
    row.bgn = ind.tag + offset
    row.end = row.bgn + nrow - 1
    rows = seq.int(row.bgn, row.end)
    # Column index from first.year to final.year
    cols = 7:(ncol + 6)
    # Set coverage levels
    # Add sex category for raw.data
    rows = setNames(rows, c(rep("Male", 81), rep("Female", 40)))
    for (i in seq_along(rows)) {
      row_idx = rows[i]
      sex_label = names(rows)[i]
      method_label = trimws(gsub(";\\s+", ";", rn[row_idx, 3]))
      key = make_key(sex_label, method_label)
      # If row key is specified is sex/age_group -> set PrEP to levels in map
      if(key %in% prep.method.map$key){
        mix = prep.method.map[prep.method.map$key == key,]$mix
        rn[row_idx, cols] = mix
        print(paste0(key," mix set to ", mix))
      }
    }
  }


  # PreP
  # Unzip all existing files in PJNZ to tmpdir
  tmp_dir = tempfile()
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(pjnz.file, exdir = tmp_dir)

  # Overwrite .RN with updated RN
  files = list.files(tmp_dir)
  rn_path = files[grepl("\\.RN$", files, ignore.case = TRUE)]
  out_rn = file.path(tmp_dir, rn_path)
  dir.create(dirname(out_rn), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(rn, out_rn, quote = FALSE, na = "", row.names = FALSE,
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

  #' Set outreach coverage in Resource needs module (RNM)
  #' @param pjnz.file The Spectrum file to extract data from
  #' @param first.year First year of the projection.
  #' @param final.year Final year of the projection.
  #' @param first.year.outreach Year to begin outreach
  #' @param final.year.outreach Year to end outreach
  #' @param outreach.program Outreach programme(s) to set coverage for, from `strata.labels$rn.programs`
  #' @param zero.kp.outreach If TRUE, zero all KP outreach programmes for all years before applying the coverage map
  #' @return A data frame.
  #' @export

  set.rn.inputs.outreach = function(pjnz.file,
                                    first.year, final.year,
                                    first.year.outreach, final.year.outreach,
                                    first.outreach.cov, final.outreach.cov,
                                    outreach.program, zero.kp.outreach = FALSE,
                                    overwrite = FALSE,
                                    name){
    filename = basename(pjnz.file)
    cat(paste0("\033[31m", "Modifying ", filename, "\033[0m\n"))

    # Check options
    stopifnot(final.year > first.year)
    stopifnot(final.year.outreach >= first.year.outreach)
    stopifnot(first.year.outreach >= first.year,
              final.year.outreach <= final.year)
    stopifnot(all(outreach.program %in% strata.labels$rn.programs))

    # Coverage values within [0, 100]
    stopifnot(all(first.outreach.cov >= 0 & first.outreach.cov <= 100))
    stopifnot(all(final.outreach.cov >= 0 & final.outreach.cov <= 100))

    # Make map to set outreach coverage by risk group
    outreach.map = data.frame(program = outreach.program, first.outreach.cov, final.outreach.cov)
    # Range to interpolate  between first and final year of outreach coverage
    outreach.map$outreach.range  = final.year.outreach - first.year.outreach + 1
    # Interpolate outreach coverage between `first.outreach.cov` and `final.outreach.cov`
    outreach.map$outreach.range.cov = mapply(
      function(a, b, n) seq(a, b, length.out = n),
      outreach.map$first.outreach.cov, outreach.map$final.outreach.cov, outreach.map$outreach.range, SIMPLIFY = FALSE)
    # If `final.year.outreach` is not equal to `final.year`, carry `final.outreach.cov`
    # through to end of timeseries
    outreach.map$years.to.end = final.year - final.year.outreach
    outreach.map$outreach.range.cov = mapply(
      function(a, b, n) if(n !=0){a = c(a, rep(b, times = n))}else{a},
      outreach.map$outreach.range.cov, outreach.map$final.outreach.cov, outreach.map$years.to.end, SIMPLIFY = FALSE)

    # Read in .RN file
    rn = read.raw.rn(pjnz.file)

    # Set outreach coverage
    #  Get row and col indexes
    offset = 3
    nrow=27
    ncol=final.year-first.year+1
    ind.tag = which(rn[[1]] == "<Coverage MV>")
    row.bgn = ind.tag + offset
    row.end = row.bgn + nrow - 1
    rows = seq.int(row.bgn, row.end)
    # Column index from `Master ID` to `final.year`
    cols = 5:(ncol+4)
    # Column index from `first.year.outreach` to `final.year`
    outreach.cols = (5 +(first.year.outreach - first.year)):(ncol+4)

    # Set coverage levels
    for (i in seq_along(rows)) {
      row_idx = rows[i]
      key = rn[row_idx, 4]

      # If `zero.kp.outreach` = TRUE, zero all KP outreach programs (see `strata.labels$rn.programs`)
      if(zero.kp.outreach){
        if(key %in% c(1:8)){
          rn[row_idx, cols] = 0
          print(paste0(strata.labels$rn.programs[[as.character(key)]], " set to zero"))
        }
      }

      # If row key is specified is sex/age_group -> set outreach to levels in `map`
      key = strata.labels$rn.programs[as.character(key)]
      if(key %in% outreach.map$program){
        msg = paste(outreach.map[outreach.map$program == key,]$first.outreach.cov, "-",
                     outreach.map[outreach.map$program == key,]$final.outreach.cov, "%",
                     "for", first.year.outreach,"-",final.year.outreach)
        cov = outreach.map[outreach.map$program == key,]$outreach.range.cov[[1]]
        rn[row_idx, outreach.cols] = cov
        print(paste0(key, " set to: ", msg))
      }
    }

    # Unzip all existing files in PJNZ to tmpdir
    tmp_dir = tempfile()
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(pjnz.file, exdir = tmp_dir)

    # Overwrite .RN with updated RN
    files = list.files(tmp_dir)
    rn_path = files[grepl("\\.RN$", files, ignore.case = TRUE)]
    out_rn = file.path(tmp_dir, rn_path)
    dir.create(dirname(out_rn), recursive = TRUE, showWarnings = FALSE)
    utils::write.table(rn, out_rn, quote = FALSE, na = "", row.names = FALSE,
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


  #' Set unit costs in Resource needs module (RNM)
  #' @param pjnz.file The Spectrum file to extract data from
  #' @param first.year First year of the projection.
  #' @param final.year Final year of the projection.
  #' @param first.year.cost Year to begin applying the new unit cost
  #' @param unit Unit cost label(s) to set, from `strata.labels$rn.unit.costs.general`,
  #'   `strata.labels$rn.unit.costs.treatment`, or `strata.labels$rn.unit.costs.pmtct`
  #' @param cost Unit cost value(s) corresponding to `unit`
  #' @return A data frame.
  #' @export

  set.rn.inputs.unit.costs = function(pjnz.file,
                                    first.year, final.year,
                                    first.year.cost,
                                    unit, cost,
                                    overwrite = FALSE,
                                    name){
    filename = basename(pjnz.file)
    cat(paste0("\033[31m", "Modifying ", filename, "\033[0m\n"))

    # Check options
    stopifnot(all(unit %in% c(strata.labels$rn.unit.costs.general,
                              strata.labels$rn.unit.costs.treatment,
                              strata.labels$rn.unit.costs.pmtct)))
    stopifnot(final.year > first.year)
    stopifnot(is.numeric(cost))

    # Map of unit costs to modify
    unit.cost.map = data.frame(unit = unit, cost = cost)

    # Read in .RN file
    rn = read.raw.rn(pjnz.file)

    art_pmtct_tags = c(strata.labels$rn.unit.costs.treatment,
                        strata.labels$rn.unit.costs.pmtct)
    art_pmtct = intersect(unit, art_pmtct_tags)


    if(length(art_pmtct)> 0){
      for(x in art_pmtct){
        # Set unit costs for Treatment units
        idx = which(art_pmtct == x)
        tag = names(art_pmtct_tags)[art_pmtct_tags == x]

        print(x)
        print(tag)
        ind.tag = which(rn[[1]] == tag)
        row = ind.tag + 2
        col.bgn = first.year.cost - first.year + 4
        col.end = final.year - first.year+ 4
        cols = seq.int(col.bgn, col.end)
        cost = unit.cost.map[unit.cost.map$unit == x,]$cost
        rn[row, cols] = cost
        print(paste0(x, " set to: $", cost, " from ", first.year.cost, " to ", final.year))
      }
    }


    # Set unit costs for General Population units
    offset = 3
    nrow=41
    ind.tag = which(rn[[1]] == "<UnitCosts MV>")

    row.bgn = ind.tag + offset
    row.end = row.bgn + nrow - 1
    rows = seq.int(row.bgn, row.end)

    col.bgn = first.year.cost - first.year + 5
    col.end = final.year - first.year+ 5
    cols = seq.int(col.bgn, col.end)

    # Set unit costs
    for (i in seq_along(rows)) {
      row_idx = rows[i]
      key = rn[row_idx, 4]

      # If row key is specified is sex/age_group -> set outreach to levels in `map`
      key = strata.labels$rn.unit.costs.general[as.character(key)]
      if(key %in% unit.cost.map$unit){

        cost = unit.cost.map[unit.cost.map$unit == key,]$cost
        rn[row_idx, cols] = cost
        print(paste0(key, " set to: $", cost, " from ", first.year.cost, " to ", final.year))
      }
    }


    # Unzip all existing files in PJNZ to tmpdir
    tmp_dir = tempfile()
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    utils::unzip(pjnz.file, exdir = tmp_dir)

    # Overwrite .RN with updated RN
    files = list.files(tmp_dir)
    rn_path = files[grepl("\\.RN$", files, ignore.case = TRUE)]
    out_rn = file.path(tmp_dir, rn_path)
    dir.create(dirname(out_rn), recursive = TRUE, showWarnings = FALSE)
    utils::write.table(rn, out_rn, quote = FALSE, na = "", row.names = FALSE,
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
