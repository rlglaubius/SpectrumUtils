rsm_pjnz = "/Users/rachel/Avenir Health Dropbox/Rachel Esra/Goals/CIV_base_circ.PJNZ"

test_that("set.rn.inputs.prep zeroes coverage then sets it for specified risk groups", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  out = tempfile(fileext=".PJNZ")
  set.rn.inputs.prep(rsm_pjnz, first.year=fy, final.year=ly,
                      first.year.prep=2020, final.year.prep=2025,
                      first.prep.cov=10, final.prep.cov=50,
                      risk.group=c("High risk heterosexual", "Low risk heterosexual"),
                      sex=c("Male", "Female"),
                      zero.prep=TRUE, overwrite=FALSE, name=out)

  rn2 = read.raw.rn(out)
  cov = rn.inputs.prep.coverage(rn2, direction="wide", first.year=fy, final.year=ly)

  male = cov[cov$sex == "Male" & cov$risk_group == "High risk heterosexual",]
  female = cov[cov$sex == "Female" & cov$risk_group == "Low risk heterosexual",]
  untouched = cov[cov$sex == "Male" & cov$risk_group == "Medium risk heterosexual",]

  expect_equal(male[[as.character(fy)]], 0) # zeroed before first.year.prep
  expect_equal(male[["2020"]], 10)
  expect_equal(male[["2025"]], 50)
  expect_equal(female[["2020"]], 10)
  expect_equal(female[["2025"]], 50)
  expect_equal(untouched[["2020"]], 0) # zeroed, not in risk.group/sex map
})

test_that("set.rn.inputs.outreach zeroes KP programmes then sets the requested one", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  out = tempfile(fileext=".PJNZ")
  set.rn.inputs.outreach(rsm_pjnz, first.year=fy, final.year=ly,
                          first.year.outreach=2020, final.year.outreach=2025,
                          first.outreach.cov=5, final.outreach.cov=60,
                          outreach.program="Key populations: Female sex workers reached by intervention",
                          zero.kp.outreach=TRUE, overwrite=FALSE, name=out)

  rn2 = read.raw.rn(out)
  cov = rn.inputs.coverage(rn2, direction="wide", first.year=fy, final.year=ly)

  fsw = cov[cov$Program == "Key populations: Female sex workers reached by intervention",]
  msw = cov[cov$Program == "Key populations: Male sex workers reached by intervention",]

  expect_equal(fsw[[as.character(fy)]], 0) # zeroed before first.year.outreach
  expect_equal(fsw[["2020"]], 5)
  expect_equal(fsw[["2025"]], 60)
  expect_equal(msw[["2020"]], 0) # other KP programme zeroed by zero.kp.outreach, not set
})

test_that("set.rn.inputs.unit.costs updates the requested unit cost from first.year.cost", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)
  unit = "Condom provision: Cost per male condom distributed by the public sector"

  out = tempfile(fileext=".PJNZ")
  set.rn.inputs.unit.costs(rsm_pjnz, first.year=fy, final.year=ly,
                            first.year.cost=2020,
                            unit=unit, cost=0.05,
                            overwrite=FALSE, name=out)

  rn1 = read.raw.rn(rsm_pjnz)
  rn2 = read.raw.rn(out)
  ind.tag = which(rn2[[1]] == "<UnitCosts MV>")
  row = ind.tag + 3 + which(strata.labels$rn.unit.costs.general == unit) - 1
  col.2020 = 2020 - fy + 5
  col.2019 = col.2020 - 1

  expect_equal(as.numeric(rn2[row, col.2020]), 0.05)
  expect_equal(rn2[row, col.2019], rn1[row, col.2019]) # unchanged before first.year.cost
})
