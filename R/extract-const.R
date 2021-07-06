
strata.labels = list(
  sex     = c("Male", "Female"),
  sex.aug = c("Male+Female", "Male", "Female"),
  age     = c(sprintf("%d", 0:79), "80+"),
  age.5yr = c(sprintf("%d-%d", seq(0, 75, 5), seq(4, 79, 5)), "80+"),
  age.gam = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-49", "50+"))

dp_not_avail = -9999 # Value used to indicate missing data in DemProj
