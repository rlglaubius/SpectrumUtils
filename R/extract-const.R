
strata.labels = list(
  sex     = c("Male", "Female"),
  sex.aug = c("Male+Female", "Male", "Female"),
  age     = c(sprintf("%d", 0:79), "80+"),
  age.5yr = c(sprintf("%d-%d", seq(0, 75, 5), seq(4, 79, 5)), "80+"),
  age.gam = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-49", "50+"),
  age.cd4.adult = c("15-24", "25-34", "35-44", "45+"),
  age.csavr = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"),
  cd4.adult = c("CD4>500", "CD4 350-500", "CD4 250-349", "CD4 200-249", "CD4 100-199", "CD4 50-99", "CD4<50"),
  incidence.model = c("Direct", "EPP", "AEM", "CSAVR", "Mortality", "ECDC"),
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

dp_not_avail = -9999 # Value used to indicate missing data in DemProj
