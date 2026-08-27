# Goals RSM (.HV) test fixture. Not yet committed to the repo pending
# confirmation of what can be shared publicly (see NEWS.md); tests below are
# skipped if the file isn't present at this local path.
rsm_pjnz = "/Users/rachel/Avenir Health Dropbox/Rachel Esra/Goals/CIV_base_circ.PJNZ"

test_that("Read Goals RSM scalar inputs", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  hv.raw = read.raw.hv(rsm_pjnz)

  expect_true(is.numeric(hv.inputs.trans.hiv.f(hv.raw)))
  expect_true(is.numeric(hv.inputs.trans.mult.m(hv.raw)))
  expect_true(is.numeric(hv.inputs.trans.mult.sti(hv.raw)))
  expect_true(is.numeric(hv.inputs.trans.mult.msm(hv.raw)))
  expect_true(is.numeric(hv.inputs.condom.eff(hv.raw)))
  expect_true(is.numeric(hv.inputs.epidemic.start.year(hv.raw)))
  expect_true(is.numeric(hv.inputs.initial.pulse(hv.raw)))
  expect_true(is.numeric(hv.inputs.months.primary.stage(hv.raw)))
})

test_that("Read Goals RSM behavioural time series", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  hv.raw = read.raw.hv(rsm_pjnz)

  expect_equal(nrow(hv.inputs.age.first.sex(hv.raw, direction="long")) > 0, TRUE)
  expect_equal(nrow(hv.inputs.force.inf(hv.raw, direction="long")) > 0, TRUE)
  expect_equal(nrow(hv.inputs.idu.sharing(hv.raw, direction="long")) > 0, TRUE)
  expect_equal(nrow(hv.inputs.num.partners(hv.raw, direction="long")) > 0, TRUE)
  expect_equal(nrow(hv.inputs.sex.acts(hv.raw, direction="long")) > 0, TRUE)
  expect_equal(nrow(hv.inputs.sti.prev(hv.raw, direction="long")) > 0, TRUE)
  expect_equal(nrow(hv.inputs.condom.percent(hv.raw, direction="long")) > 0, TRUE)

  ## condom groups exclude PWID (7 groups: 3 heterosexual risk + MSM + 3 MSM subgroups)
  cp = hv.inputs.condom.percent(hv.raw)
  expect_equal(nrow(cp), 7)
  expect_false("People who inject drugs" %in% cp$RiskGroup)
})

test_that("Read Goals RSM static risk-group and fitting inputs", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  hv.raw = read.raw.hv(rsm_pjnz)

  expect_equal(nrow(hv.inputs.perc.married(hv.raw)) > 0, TRUE)
  expect_equal(nrow(hv.inputs.infectiousness(hv.raw)), 3)
  expect_equal(nrow(hv.inputs.behavior(hv.raw)) > 0, TRUE)
  expect_equal(nrow(hv.inputs.recruitment(hv.raw)) > 0, TRUE)
  expect_equal(nrow(hv.inputs.impact.matrix(hv.raw)), 13)
  expect_equal(nrow(hv.inputs.fit.params(hv.raw)), 17)

  fc = hv.inputs.fit.control(hv.raw)
  expect_true(all(c("max_iterations", "error_tolerance", "weight") %in% names(fc)))
})

test_that("extract_all_hv returns tidy const and time_varying data frames", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  hv.raw = read.raw.hv(rsm_pjnz)
  res = extract_all_hv(hv.raw, file_name=basename(rsm_pjnz), iso3="CIV")

  expect_true(is.list(res))
  expect_true(all(c("const", "time_varying") %in% names(res)))
  expect_equal(nrow(res$const) > 0, TRUE)
  expect_equal(nrow(res$time_varying) > 0, TRUE)
  expect_true(all(res$const$iso3 == "CIV"))
  expect_true(all(c("sti_prev", "condom_percent") %in% res$time_varying$variable))
})

test_that("extract_all_hv_folder combines results across a folder", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  res = extract_all_hv_folder(dirname(rsm_pjnz), pattern="^CIV_base_circ\\.PJNZ$")

  expect_true(all(c("const", "time_varying") %in% names(res)))
  expect_equal(nrow(res$const) > 0, TRUE)
  expect_true(all(res$const$iso3 == "CIV"))
})
