test_that("Read Goals ASM data from Spectrum 6.36", {
  moddat = read.raw.ha("../uga-asm-2023-v000.pjnz")
  expect_equal(nrow(ha.inputs.fitdata.trend(moddat, direction="wide")) > 0, TRUE)
  expect_equal(nrow(ha.output.transmitted(moddat, direction="wide")) > 0, TRUE)
})
