## Row-major layout of KP programme facets for compare.kp.programme.coverage's
## grid: 4 rows x 3 columns, with NA marking an intentionally blank cell
## (FSW/MSW and MSM each only have 2 programmes; PWID has the full row).
#' @noRd
kp.programme.grid = c(
  "KP: FSW reached", "KP: MSW reached", NA,
  "KP: MSM lubricants", "KP: MSM reached", NA,
  "KP: PWID drug substitution", "KP: PWID needle exchange", "KP: PWID harm reduction",
  "KP: PWID outreach", "KP: PWID counseling/testing", NA)

#' Compare programme coverage inputs between two PJNZ files
#'
#' @details Reads Resource Needs Module (RNM) inputs from each file and
#'   compares general population condom provision coverage and
#'   key-population-specific programme coverage (FSW, MSW, MSM, PWID) between
#'   the two files over a common year range. The KP programmes are arranged
#'   into a fixed grid, one row per key population, so related programmes
#'   line up for comparison. See \code{compare.prep.coverage} for PrEP
#'   coverage and method mix.
#' @param pjnz.file1 Path to the first PJNZ file
#' @param pjnz.file2 Path to the second PJNZ file
#' @param first.year First year to compare
#' @param final.year Final year to compare
#' @param label1 Legend label for \code{pjnz.file1}. Defaults to its file name.
#' @param label2 Legend label for \code{pjnz.file2}. Defaults to its file name.
#' @param combine.plots If \code{FALSE} (default), returns a named list of
#'   ggplot objects. If \code{TRUE}, arranges all plots onto a single
#'   landscape A4 page and saves it as a PDF at \code{file.path} instead.
#' @param file.path Output PDF path used when \code{combine.plots=TRUE}.
#'   Defaults to \code{compare_kp_programme_coverage.pdf} in the current
#'   directory.
#' @return A named list of ggplot objects: \code{condom}, \code{kp} (or, if
#'   \code{combine.plots=TRUE}, the saved PDF path, invisibly).
#' @export
compare.kp.programme.coverage = function(pjnz.file1, pjnz.file2, first.year, final.year, label1=NULL, label2=NULL,
                                          combine.plots=FALSE,
                                          file.path=base::file.path(getwd(), "compare_kp_programme_coverage.pdf")) {
  if (is.null(label1)) {label1 = basename(pjnz.file1)}
  if (is.null(label2)) {label2 = basename(pjnz.file2)}
  if (label1 == label2) {stop("label1 and label2 must be distinct")}

  rn1 = read.raw.rn(pjnz.file1)
  rn2 = read.raw.rn(pjnz.file2)

  cov1 = rn.inputs.coverage(rn1, direction="long", first.year=first.year, final.year=final.year)
  cov2 = rn.inputs.coverage(rn2, direction="long", first.year=first.year, final.year=final.year)
  cov1$file = label1
  cov2$file = label2
  cov = rbind(cov1, cov2)

  condom = cov[cov$Program == "General population: Condom provision",]
  kp = cov[grepl("^Key populations:", cov$Program),]
  kp$Program = unname(kp.programme.labels[kp$Program])

  p_condom = ggplot2::ggplot(condom, ggplot2::aes(x=Year, y=Value, color=file)) +
    ggplot2::geom_line() +
    ggplot2::labs(title="Condom provision coverage", x="Year", y="Coverage", color=NULL) +
    spectrumutils.pct.scale.100() +
    spectrumutils.year.scale() +
    spectrumutils.plot.theme() +
    spectrumutils.legend.bottom()

  ## Build one small-multiple panel per KP programme (rather than a single
  ## facet_wrap) so blank cells can be placed at specific grid positions
  ## (kp.programme.grid), which facet_wrap cannot do mid-grid.
  panels = lapply(kp.programme.grid, function(program) {
    if (is.na(program)) {return(patchwork::plot_spacer())}
    ggplot2::ggplot(kp[kp$Program == program,], ggplot2::aes(x=Year, y=Value, color=file)) +
      ggplot2::geom_line() +
      ggplot2::labs(title=program, x="Year", y=NULL, color=NULL) +
      spectrumutils.pct.scale.100() +
      spectrumutils.year.scale() +
      spectrumutils.plot.theme(base_size=10)
  })
  p_kp = patchwork::wrap_plots(panels, ncol=3, guides="collect") &
    ggplot2::theme(legend.position="bottom")

  plots = list(condom=p_condom, kp=p_kp)
  if (combine.plots) {
    return(spectrumutils.save.combined.pdf(plots, file.path))
  }
  return(plots)
}

#' Compare PrEP coverage and method mix between two PJNZ files
#'
#' @details Reads Resource Needs Module (RNM) PrEP inputs from each file and
#'   compares PrEP coverage and PrEP method mix (share of PrEP users on each
#'   delivery method), by sex and risk group, between the two files over a
#'   common year range. Both coverage and method mix are split into separate
#'   male and female plots. In the method mix plots, colour distinguishes the
#'   two files being compared and line type distinguishes PrEP method.
#' @inheritParams compare.kp.programme.coverage
#' @param file.path Output PDF path used when \code{combine.plots=TRUE}.
#'   Defaults to \code{compare_prep_coverage.pdf} in the current directory.
#' @return A named list of ggplot objects: \code{coverage_male},
#'   \code{coverage_female}, \code{method_mix_male}, \code{method_mix_female}
#'   (or, if \code{combine.plots=TRUE}, the saved PDF path, invisibly).
#' @export
compare.prep.coverage = function(pjnz.file1, pjnz.file2, first.year, final.year, label1=NULL, label2=NULL,
                                  combine.plots=FALSE,
                                  file.path=base::file.path(getwd(), "compare_prep_coverage.pdf")) {
  if (is.null(label1)) {label1 = basename(pjnz.file1)}
  if (is.null(label2)) {label2 = basename(pjnz.file2)}
  if (label1 == label2) {stop("label1 and label2 must be distinct")}

  rn1 = read.raw.rn(pjnz.file1)
  rn2 = read.raw.rn(pjnz.file2)

  cov1 = rn.inputs.prep.coverage(rn1, direction="long", first.year=first.year, final.year=final.year)
  cov2 = rn.inputs.prep.coverage(rn2, direction="long", first.year=first.year, final.year=final.year)
  cov1$file = label1
  cov2$file = label2
  cov = rbind(cov1, cov2)

  mix1 = rn.inputs.prep.method.mix(rn1, direction="long", first.year=first.year, final.year=final.year)
  mix2 = rn.inputs.prep.method.mix(rn2, direction="long", first.year=first.year, final.year=final.year)
  mix1$file = label1
  mix2$file = label2
  mix = rbind(mix1, mix2)

  ## Drop PrEP methods that are never selected (0% share throughout) in either
  ## file, so the legend only lists methods actually in use for this comparison.
  used_methods = unique(mix$prep_method[mix$value > 0])
  mix = mix[mix$prep_method %in% used_methods,]

  build.coverage.plot = function(sex_value) {
    ggplot2::ggplot(cov[cov$sex == sex_value,], ggplot2::aes(x=year, y=value, color=file)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~risk_group) +
      ggplot2::labs(title=paste0("PrEP coverage: ", sex_value), x="Year", y="Coverage", color=NULL) +
      spectrumutils.pct.scale.100() +
      spectrumutils.year.scale() +
      spectrumutils.plot.theme(base_size=11) +
      spectrumutils.legend.bottom()
  }
  p_cov_male = build.coverage.plot("Male")
  p_cov_female = build.coverage.plot("Female")

  build.mix.plot = function(sex_value) {
    ggplot2::ggplot(mix[mix$sex == sex_value,], ggplot2::aes(x=year, y=value, color=file, linetype=prep_method)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~risk_group, labeller=ggplot2::label_wrap_gen(25)) +
      ggplot2::labs(title=paste0("PrEP method mix: ", sex_value), x="Year", y="Share", color=NULL, linetype="Method") +
      spectrumutils.pct.scale.100() +
      spectrumutils.year.scale() +
      spectrumutils.plot.theme(base_size=11) +
      spectrumutils.legend.bottom()
  }
  p_mix_male = build.mix.plot("Male")
  p_mix_female = build.mix.plot("Female")

  plots = list(coverage_male=p_cov_male, coverage_female=p_cov_female,
               method_mix_male=p_mix_male, method_mix_female=p_mix_female)
  if (combine.plots) {
    return(spectrumutils.save.combined.pdf(plots, file.path))
  }
  return(plots)
}
