rsm_pjnz = "/Users/rachel/Avenir Health Dropbox/Rachel Esra/Goals/CIV_base_circ.PJNZ"

test_that("set.hv.inputs.art.effect interpolates and carries the ART effect forward", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  hv.raw = read.raw.hv(rsm_pjnz)
  fy = hv.inputs.first.year(hv.raw)
  ly = hv.inputs.final.year(hv.raw)

  out = tempfile(fileext=".PJNZ")
  set.hv.inputs.art.effect(rsm_pjnz, first.year=fy, final.year=ly,
                            first.year.effect=2000, final.year.effect=2010,
                            first.effect=0.25, final.effect=0.1,
                            overwrite=FALSE, name=out)

  hv2 = read.raw.hv(out)
  eff = hv.inputs.art.effect(hv2, direction="wide")

  expect_equal(eff[1, "1999"], 0.25) # unchanged before first.year.effect
  expect_equal(eff[1, "2000"], 0.25)
  expect_equal(eff[1, "2005"], 0.175) # interpolated midpoint
  expect_equal(eff[1, "2010"], 0.1)
  expect_equal(eff[1, as.character(ly)], 0.1) # carried forward to final.year
})
