rsm_pjnz = "/Users/rachel/Avenir Health Dropbox/Rachel Esra/Goals/CIV_base_circ.PJNZ"

test_that("compare.kp.programme.coverage returns condom/kp plots", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  plots = compare.kp.programme.coverage(rsm_pjnz, rsm_pjnz, first.year=fy, final.year=ly, label1="a", label2="b")

  expect_true(all(c("condom", "kp") %in% names(plots)))
  expect_true(all(vapply(plots, function(p) inherits(p, "ggplot"), logical(1))))
})

test_that("compare.kp.programme.coverage requires distinct labels", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  expect_error(compare.kp.programme.coverage(rsm_pjnz, rsm_pjnz, first.year=fy, final.year=ly, label1="x", label2="x"))
})

test_that("compare.prep.coverage returns coverage_male/coverage_female/method_mix_male/method_mix_female plots", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  plots = compare.prep.coverage(rsm_pjnz, rsm_pjnz, first.year=fy, final.year=ly, label1="a", label2="b")

  expect_true(all(c("coverage_male", "coverage_female", "method_mix_male", "method_mix_female") %in% names(plots)))
  expect_true(all(vapply(plots, function(p) inherits(p, "ggplot"), logical(1))))
})
