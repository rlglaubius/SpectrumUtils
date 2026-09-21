rsm_pjnz = "/Users/rachel/Avenir Health Dropbox/Rachel Esra/Goals/CIV_base_circ.PJNZ"

## Display titles used by goals.calibration.plot's fixed set of
## population/sex groups (see calibration.plot.groups in R/review-goals.R).
calibration_plot_titles = c(
  "PWID (Male + Female)",
  "MSM (Male)",
  "FSW: High risk heterosexual (Female)",
  "High risk heterosexual (Male)",
  "Adults (Female)",
  "Adults (Male)")

test_that("goals.calibration.plot returns one ggplot per calibration population", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")

  plots = goals.calibration.plot(rsm_pjnz)

  expect_true(is.list(plots))
  expect_true(length(plots) > 0)
  expect_true(all(vapply(plots, function(p) inherits(p, "ggplot"), logical(1))))
  expect_true(all(names(plots) %in% calibration_plot_titles))
})
