rsm_pjnz = "/Users/rachel/Avenir Health Dropbox/Rachel Esra/Goals/CIV_base_circ.PJNZ"

test_that("set.dp.inputs.art interpolates ART coverage for Male, Female, and Child", {
  skip_if_not(file.exists(rsm_pjnz), "Goals RSM test fixture not available locally")
  dp.raw = read.raw.dp(rsm_pjnz)
  fy = dp.inputs.first.year(dp.raw)
  ly = dp.inputs.final.year(dp.raw)

  out = tempfile(fileext=".PJNZ")
  set.dp.inputs.art(rsm_pjnz, first.year=fy, final.year=ly,
                     first.year.art=2020, final.year.art=2025,
                     population=c("Male", "Female", "Child"),
                     first.art.cov=c(20, 25, 30), final.art.cov=c(70, 75, 80),
                     overwrite=FALSE, name=out)

  dp2 = read.raw.dp(out)

  adult = dp.inputs.adult.art(dp2, direction="long", first.year=fy, final.year=ly)
  adult = adult[adult$Unit == "Percent",]
  male = adult[adult$Sex == "Male" & adult$Year %in% c(2020, 2025),]
  female = adult[adult$Sex == "Female" & adult$Year %in% c(2020, 2025),]

  child = dp.inputs.child.art(dp2, direction="long", first.year=fy, final.year=ly)
  child = child[child$Age == "0-14" & child$Treatment == "ART" & child$Unit == "Percent" & child$Year %in% c(2020, 2025),]

  expect_equal(male$Value[male$Year == 2020], 20)
  expect_equal(male$Value[male$Year == 2025], 70)
  expect_equal(female$Value[female$Year == 2020], 25)
  expect_equal(female$Value[female$Year == 2025], 75)
  expect_equal(child$Value[child$Year == 2020], 30)
  expect_equal(child$Value[child$Year == 2025], 80)
})
