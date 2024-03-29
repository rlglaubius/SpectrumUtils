
strata.labels = list(
  sex      = c("Male", "Female"),
  sex.aug  = c("Male+Female", "Male", "Female"),
  age      = c(sprintf("%d", 0:79), "80+"),
  age.fert = c(sprintf("%d-%d", seq(15,45,5), seq(19, 49, 5))),
  age.5yr = c(sprintf("%d-%d", seq(0, 75, 5), seq(4, 79, 5)), "80+"),
  age.gam = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-49", "50+"),
  age.cd4.adult = c("15-24", "25-34", "35-44", "45+"),
  age.csavr = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+"),
  cd4.adult = c("CD4>500", "CD4 350-500", "CD4 250-349", "CD4 200-249", "CD4 100-199", "CD4 50-99", "CD4<50"),
  cd4.child.old = c("CD4>1000", "CD4 750-1000", "CD4 500-750", "CD4 350-500", "CD4 200-350", "CD4<200"),
  cd4.csavr = c("CD4>500", "CD4 350-500", "CD4 200-350", "CD4<200"),
  art.status = c("no ART", "ART"),
  hiv.ped = c("Off ART, perinatal HIV",
              "Off ART, HIV during breastfeeding at 0-6 months",
              "Off ART, HIV during breastfeeding at 6-12 months",
              "Off ART, HIV during breastfeeding at >12 months",
              "On ART, [0,6) months",
              "On ART, [6,12) months",
              "On ART, 12+ months"),
  art.dur = c("On ART, [0,6) months", "On ART, [6,12) months", "On ART, 12+ months"),
  art.dur.agg = c("On ART, [0,12) months", "On ART, 12+ months"),
  life.table = c("Coale-Demeny West", "Coale-Demeny North", "Coale-Demeny East", "Coale-Demeny South", "UN General", "UN Latin America", "UN Chile", "UN South Asia", "UN East Asia", "Country-specific", "Custom"),
  pmtct_time    = c("Prenatal", "Postnatal"),
  pmtct_regimen = c("Single dose NVP", "Dual ARV", "Option A", "Option B", "ART started before current pregnancy", "ART started during current pregnancy at least 4 weeks before delivery", "ART started during current pregnancy < 4 weeks before delivery"),
  kos.source = c("Case reports", "Shiny90", "CSAVR", "ECDC", "Direct"),
  incidence.model = c("Direct", "EPP", "AEM", "CSAVR", "Mortality", "ECDC"),
  csavr.model = c("None", "Double logistic", "Single logistic", "Splines", "rLogistic"),
  epi.patterns = c("Generalized", "Concentrated non-IDU", "Concentrated IDU", "Custom", "Fitted: fixed over time", "Fitted: time-varying", "Fitted to HIV prevalence or ART", "CSAVR", "Web"),
  opt.region = c("Asia", "Central Africa", "Developed Countries", "East Africa", "Eastern Europe", "Latin America and Caribbean", "North Africa Middle East", "Southern Africa", "West Africa"),
  month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

output.labels = list(
  value = "Value"
)

dp_not_avail = -9999 # Value used to indicate missing data in DemProj
