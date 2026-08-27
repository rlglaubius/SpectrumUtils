## Facet labels shared by compare.art.coverage and compare.art.effect, so the
## two functions' panels line up.
#' @noRd
art.population.labels = c("Adult male"="Males (15+)", "Adult female"="Females (15+)", "Children"="Children (0-14)")

#' Compare ART coverage and adjustment factor between two PJNZ files
#'
#' @details Compares ART coverage (percent) and the ART adjustment factor
#'   (used to scale programmatic ART numbers for over/undercount) for adult
#'   males, adult females, and children (ages 0-14), over a common year
#'   range. Returns two plots: ART coverage and the ART adjustment factor,
#'   each faceted by population.
#' @param pjnz.file1 Path to the first PJNZ file
#' @param pjnz.file2 Path to the second PJNZ file
#' @param first.year First year to compare. NULL uses each file's own first year.
#' @param final.year Final year to compare. NULL uses each file's own final year.
#' @param label1 Legend label for \code{pjnz.file1}. Defaults to its file name.
#' @param label2 Legend label for \code{pjnz.file2}. Defaults to its file name.
#' @param combine.plots If \code{FALSE} (default), returns a named list of
#'   ggplot objects. If \code{TRUE}, arranges all plots onto a single
#'   landscape A4 page and saves it as a PDF at \code{file.path} instead.
#' @param file.path Output PDF path used when \code{combine.plots=TRUE}.
#'   Defaults to \code{compare_art_coverage.pdf} in the current directory.
#' @return A named list of ggplot objects: \code{coverage}, \code{adjustment}
#'   (or, if \code{combine.plots=TRUE}, the saved PDF path, invisibly).
#' @export
compare.art.coverage = function(pjnz.file1, pjnz.file2, first.year=NULL, final.year=NULL, label1=NULL, label2=NULL,
                                 combine.plots=FALSE,
                                 file.path=base::file.path(getwd(), "compare_art_coverage.pdf")) {
  if (is.null(label1)) {label1 = basename(pjnz.file1)}
  if (is.null(label2)) {label2 = basename(pjnz.file2)}
  if (label1 == label2) {stop("label1 and label2 must be distinct")}

  dp1 = read.raw.dp(pjnz.file1)
  dp2 = read.raw.dp(pjnz.file2)
  spectrumutils.validate.year.range(first.year, final.year, dp.inputs.first.year(dp1), dp.inputs.final.year(dp1), label1)
  spectrumutils.validate.year.range(first.year, final.year, dp.inputs.first.year(dp2), dp.inputs.final.year(dp2), label2)

  build = function(dp.raw, file_label) {
    art = dp.inputs.adult.art(dp.raw, direction="long", first.year=first.year, final.year=final.year)
    art = art[art$Unit == "Percent",]
    art_m = art[art$Sex == "Male",]
    art_f = art[art$Sex == "Female",]

    child = dp.inputs.child.art(dp.raw, direction="long", first.year=first.year, final.year=final.year)
    child = child[child$Age == "0-14" & child$Treatment == "ART" & child$Unit == "Percent",]

    cov = rbind(
      data.frame(Population="Adult male", Year=art_m$Year, Value=art_m$Value),
      data.frame(Population="Adult female", Year=art_f$Year, Value=art_f$Value),
      data.frame(Population="Children", Year=child$Year, Value=child$Value))
    cov$Metric = "ART coverage (%)"

    adj_a = dp.inputs.adult.art.adjustment.value(dp.raw, direction="long", first.year=first.year, final.year=final.year)
    adj_am = adj_a[adj_a$Sex == "Male",]
    adj_af = adj_a[adj_a$Sex == "Female",]
    adj_c = dp.inputs.child.art.adjustment.value(dp.raw, direction="long", first.year=first.year, final.year=final.year)

    adj = rbind(
      data.frame(Population="Adult male", Year=adj_am$Year, Value=adj_am$Value),
      data.frame(Population="Adult female", Year=adj_af$Year, Value=adj_af$Value),
      data.frame(Population="Children", Year=adj_c$Year, Value=adj_c$Value))
    adj$Metric = "ART adjustment factor"

    dat = rbind(cov, adj)
    dat$file = file_label
    dat$Population = factor(unname(art.population.labels[dat$Population]), levels=art.population.labels)
    return(dat)
  }

  dat = rbind(build(dp1, label1), build(dp2, label2))

  p_cov = ggplot2::ggplot(dat[dat$Metric == "ART coverage (%)",], ggplot2::aes(x=Year, y=Value, color=file)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~Population, nrow=1) +
    ggplot2::labs(title="Programme statistics: Number on ART", x="Year", y=NULL, color=NULL) +
    spectrumutils.pct.scale.100() +
    spectrumutils.year.scale() +
    spectrumutils.plot.theme() +
    spectrumutils.legend.bottom()

  p_adj = ggplot2::ggplot(dat[dat$Metric == "ART adjustment factor",], ggplot2::aes(x=Year, y=Value, color=file)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~Population, nrow=1) +
    ggplot2::labs(title="Programme statistics: ART adjustment factor", x="Year", y=NULL, color=NULL) +
    ggplot2::scale_y_continuous(limits=c(0, 2), breaks=seq(0, 2, 0.5)) +
    spectrumutils.year.scale() +
    spectrumutils.plot.theme() +
    spectrumutils.legend.bottom()

  plots = list(coverage=p_cov, adjustment=p_adj)
  if (combine.plots) {
    return(spectrumutils.save.combined.pdf(plots, file.path))
  }
  return(plots)
}

#' Compare the effect of ART on HIV transmission between two PJNZ files
#'
#' @details \code{hv.inputs.art.effect()} returns a single value per year (not
#'   broken down by sex or age) for the reduction in HIV transmission on ART.
#'   The same series is plotted in all three panels (adult male / adult
#'   female / children) so this function's layout lines up with
#'   \code{compare.art.coverage()}; the underlying value does not vary by
#'   population.
#' @inheritParams compare.art.coverage
#' @return A ggplot object faceted by population.
#' @export
compare.art.effect = function(pjnz.file1, pjnz.file2, first.year=NULL, final.year=NULL, label1=NULL, label2=NULL) {
  if (is.null(label1)) {label1 = basename(pjnz.file1)}
  if (is.null(label2)) {label2 = basename(pjnz.file2)}
  if (label1 == label2) {stop("label1 and label2 must be distinct")}

  hv1 = read.raw.hv(pjnz.file1)
  hv2 = read.raw.hv(pjnz.file2)
  spectrumutils.validate.year.range(first.year, final.year, hv.inputs.first.year(hv1), hv.inputs.final.year(hv1), label1)
  spectrumutils.validate.year.range(first.year, final.year, hv.inputs.first.year(hv2), hv.inputs.final.year(hv2), label2)

  build = function(hv.raw, file_label) {
    eff = hv.inputs.art.effect(hv.raw, direction="long", first.year=first.year, final.year=final.year)
    dat = do.call(rbind, lapply(c("Adult male", "Adult female", "Children"), function(pop) {
      data.frame(Population=pop, Year=eff$Year, Value=eff$Value)
    }))
    dat$file = file_label
    dat$Population = factor(unname(art.population.labels[dat$Population]), levels=art.population.labels)
    return(dat)
  }

  dat = rbind(build(hv1, label1), build(hv2, label2))

  p = ggplot2::ggplot(dat, ggplot2::aes(x=Year, y=Value, color=file)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~Population, nrow=1) +
    ggplot2::labs(title="Reduction in HIV transmission on ART", x="Year", y="Effect", color=NULL) +
    spectrumutils.year.scale() +
    spectrumutils.plot.theme() +
    spectrumutils.legend.bottom()

  return(p)
}
