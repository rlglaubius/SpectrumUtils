#' @noRd
goals_prevalence_by_sex_population = function(hv.raw, first.year=NULL, final.year=NULL) {
  adults = hv.output.adults(hv.raw, direction="long", first.year=first.year, final.year=final.year)

  ## Sums Total (all HIV statuses) and PLHIV (HIV-positive statuses) within
  ## each combination of the grouping columns in `by`.
  summarise.prevalence = function(data, by) {
    totals = stats::aggregate(Value ~ ., data=data[, c(by, "Value")], FUN=sum)
    plhiv  = stats::aggregate(Value ~ ., data=data[data$HIV != "Negative", c(by, "Value")], FUN=sum)
    colnames(totals)[colnames(totals) == "Value"] = "Total"
    colnames(plhiv)[colnames(plhiv) == "Value"] = "PLHIV"
    dat = merge(totals, plhiv, by=by, all.x=TRUE)
    dat$PLHIV[is.na(dat$PLHIV)] = 0
    return(dat)
  }

  ## Calibration data (hv.inputs.calibration.data) can report a prevalence
  ## estimate for any population at Sex="Male", "Female", or "Male+Female",
  ## and for "Adults" (all risk groups combined) as well as individual risk
  ## groups. Build model lines for every one of those combinations so any
  ## calibration point has a matching model series to plot against.
  by_sex_pop = summarise.prevalence(adults, c("Sex", "Population", "Year"))

  by_pop_mf = summarise.prevalence(adults, c("Population", "Year"))
  by_pop_mf$Sex = "Male+Female"

  adults_all = adults
  adults_all$Population = "Adults"
  by_sex_adults = summarise.prevalence(adults_all, c("Sex", "Population", "Year"))

  by_adults_mf = summarise.prevalence(adults_all, c("Population", "Year"))
  by_adults_mf$Sex = "Male+Female"

  dat = rbind(by_sex_pop,
             by_pop_mf[, colnames(by_sex_pop)],
             by_sex_adults,
             by_adults_mf[, colnames(by_sex_pop)])
  ## hv.inputs.calibration.data() reports Estimate/Lower/Upper as percent
  ## (e.g. 4.0 meaning 4%), so express modelled prevalence on the same scale.
  dat$Prevalence = ifelse(dat$Total > 0, 100 * dat$PLHIV / dat$Total, NA)
  return(dat)
}

## Fixed set of population/sex combinations shown by goals.calibration.plot,
## and the display title for each. "High risk heterosexual" is reported by
## sex only in the underlying data; Female is labelled as FSW here since
## that's how this risk group is understood in practice.
#' @noRd
calibration.plot.groups = list(
  list(population="People who inject drugs",   sex="Male+Female", title="PWID (Male + Female)"),
  list(population="Men who have sex with men",  sex="Male",        title="MSM (Male)"),
  list(population="High risk heterosexual",     sex="Female",      title="FSW: High risk heterosexual (Female)"),
  list(population="High risk heterosexual",     sex="Male",        title="High risk heterosexual (Male)"),
  list(population="Adults",                     sex="Female",      title="Adults (Female)"),
  list(population="Adults",                     sex="Male",        title="Adults (Male)"))

## For each Year in the (UseInFit==TRUE) calibration rows passed in, pools
## the individual survey/study estimates into a single N-weighted crude
## prevalence estimate with a 95% CI (normal approximation on the pooled
## proportion), for overlaying on top of the individual calibration points.
#' @noRd
pooled.crude.prevalence = function(calib_fit) {
  cols = c("Year", "Estimate", "Lower", "Upper")
  if (nrow(calib_fit) == 0) {return(calib_fit[, cols])}

  do.call(rbind, lapply(split(calib_fit, calib_fit$Year), function(d) {
    p = d$Estimate / 100
    n = d$N
    p.pooled = sum(n * p) / sum(n)
    se = sqrt(p.pooled * (1 - p.pooled) / sum(n))
    data.frame(
      Year = d$Year[1],
      Estimate = 100 * p.pooled,
      Lower = 100 * max(0, p.pooled - 1.96 * se),
      Upper = 100 * min(1, p.pooled + 1.96 * se))
  }))
}

#' Plot modelled HIV prevalence against Goals RSM calibration data
#'
#' @details For a fixed set of population/sex combinations (PWID, MSM, FSW,
#'   high risk heterosexual men, and adult males/females), plots the modelled
#'   HIV prevalence trend (derived from \code{hv.output.adults()}, black line)
#'   against the survey/study calibration points and their 95% confidence
#'   intervals, to visually check fit quality. Individual calibration points
#'   actually used in the Goals RSM fit (\code{UseInFit=TRUE}) are shown as
#'   filled red circles; points carried in the file for reference but
#'   excluded from fitting are shown as open grey circles. A blue diamond (
#'   with its own 95% CI) marks the N-weighted pooled crude prevalence
#'   estimate for each year, computed from only the \code{UseInFit=TRUE}
#'   points for that year.
#' @param pjnz.file Path to a PJNZ file
#' @param combine.plots If \code{FALSE} (default), returns a named list of
#'   ggplot objects. If \code{TRUE}, arranges all plots onto a single
#'   landscape A4 page and saves it as a PDF at \code{file.path} instead.
#' @param file.path Output PDF path used when \code{combine.plots=TRUE}.
#'   Defaults to \code{goals_calibration_plot.pdf} in the current directory.
#' @inheritParams hv.inputs.art.effect
#' @return A named list of ggplot objects, one per population/sex combination
#'   (or, if \code{combine.plots=TRUE}, the saved PDF path, invisibly).
#' @export
goals.calibration.plot = function(pjnz.file, first.year=NULL, final.year=NULL,
                                   combine.plots=FALSE,
                                   file.path=base::file.path(getwd(), "goals_calibration_plot.pdf")) {
  hv.raw = read.raw.hv(pjnz.file)
  model = goals_prevalence_by_sex_population(hv.raw, first.year=first.year, final.year=final.year)

  calib = hv.inputs.calibration.data(hv.raw)
  calib$Population = as.character(calib$Population)
  calib$Sex = as.character(calib$Sex)

  plots = list()
  for (group in calibration.plot.groups) {
    calib_grp = calib[calib$Population == group$population & calib$Sex == group$sex,]
    model_grp = model[model$Population == group$population & model$Sex == group$sex,]
    if (nrow(calib_grp) == 0 && nrow(model_grp) == 0) {next}

    pooled_grp = pooled.crude.prevalence(calib_grp[calib_grp$UseInFit,])

    p = ggplot2::ggplot() +
      ggplot2::geom_line(data=model_grp, ggplot2::aes(x=Year, y=Prevalence), color="black") +
      ggplot2::geom_errorbar(data=calib_grp, ggplot2::aes(x=Year, ymin=Lower, ymax=Upper, color=UseInFit), width=0.4) +
      ggplot2::geom_point(data=calib_grp, ggplot2::aes(x=Year, y=Estimate, shape=UseInFit, color=UseInFit), size=2) +
      ggplot2::geom_errorbar(data=pooled_grp, ggplot2::aes(x=Year, ymin=Lower, ymax=Upper), color="blue", width=0.4) +
      ggplot2::geom_point(data=pooled_grp, ggplot2::aes(x=Year, y=Estimate), shape=23, fill="blue", color="blue", size=2.5) +
      ggplot2::scale_shape_manual(name="Used in fit", values=c(`TRUE`=16, `FALSE`=1)) +
      ggplot2::scale_color_manual(name="Used in fit", values=c(`TRUE`="red", `FALSE`="grey50")) +
      ggplot2::labs(title=group$title, x="Year", y="HIV prevalence") +
      spectrumutils.pct.scale() +
      spectrumutils.year.scale() +
      spectrumutils.plot.theme()

    plots[[group$title]] = p
  }

  if (combine.plots) {
    return(spectrumutils.save.combined.pdf(plots, file.path))
  }
  return(plots)
}
