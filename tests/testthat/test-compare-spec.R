rsm_pjnz = "/Users/rachel/Avenir Health Dropbox/Rachel Esra/Goals/CIV_base_circ.PJNZ"

test_that("compare.art.coverage returns coverage/adjustment plots", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  plots = compare.art.coverage(rsm_pjnz, rsm_pjnz, first.year=fy, final.year=ly, label1="a", label2="b")

  expect_true(all(c("coverage", "adjustment") %in% names(plots)))
  expect_true(all(vapply(plots, function(p) inherits(p, "ggplot"), logical(1))))
})

test_that("compare.art.coverage requires distinct labels", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  expect_error(compare.art.coverage(rsm_pjnz, rsm_pjnz, first.year=fy, final.year=ly, label1="x", label2="x"))
})

test_that("compare.art.effect returns a single ggplot faceted by population", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  hv.raw = read.raw.hv(rsm_pjnz)
  fy = hv.inputs.first.year(hv.raw)
  ly = hv.inputs.final.year(hv.raw)

  p = compare.art.effect(rsm_pjnz, rsm_pjnz, first.year=fy, final.year=ly, label1="a", label2="b")

  expect_true(inherits(p, "ggplot"))
})

test_that("compare.art.effect requires distinct labels", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  hv.raw = read.raw.hv(rsm_pjnz)
  fy = hv.inputs.first.year(hv.raw)
  ly = hv.inputs.final.year(hv.raw)

  expect_error(compare.art.effect(rsm_pjnz, rsm_pjnz, first.year=fy, final.year=ly, label1="x", label2="x"))
})
