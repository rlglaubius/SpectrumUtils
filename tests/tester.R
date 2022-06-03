library(readxl)

source("../R/utils.R")
source("../R/extract-dp.R")
source("../R/extract-const.R")

## TODO: follow best practices for unit testing in R packages

check = function(bool) {return(ifelse(bool, "PASS", "FAIL"))}

ref_xlsx = "reference-values.xlsx"
tab_dp_input = "DP_Input"
tab_aim_input_art = "AIM_Input_ART"

dp.raw = read.raw.dp("mwi-example.pjnz")

tfr = dp.inputs.tfr(dp.raw, direction="wide")
srb = dp.inputs.srb(dp.raw, direction="wide")

adult_inits_wide = dp.inputs.adult.art.initiations(dp.raw, direction="wide")
adult_inits_long = dp.inputs.adult.art.initiations(dp.raw, direction="long")
child_inits_wide = dp.inputs.child.art.initiations(dp.raw, direction="wide")
child_inits_long = dp.inputs.child.art.initiations(dp.raw, direction="long")

adult_restart_wide = dp.inputs.adult.art.reinitiations(dp.raw, direction="wide")
adult_restart_long = dp.inputs.adult.art.reinitiations(dp.raw, direction="long")
child_restart_wide = dp.inputs.child.art.reinitiations(dp.raw, direction="wide")
child_restart_long = dp.inputs.child.art.reinitiations(dp.raw, direction="long")

art_adult_ltfu = dp.inputs.adult.art.ltfu(dp.raw, direction="wide")
art_child_ltfu = dp.inputs.child.art.ltfu(dp.raw, direction="wide")

ref_tfr = read_excel(ref_xlsx, sheet=tab_dp_input, range="B2:BE3")
ref_srb = read_excel(ref_xlsx, sheet=tab_dp_input, range="B6:BE7")

ref_adult_inits = read_excel(ref_xlsx, sheet=tab_aim_input_art, range="B2:BE6")
ref_child_inits = read_excel(ref_xlsx, sheet=tab_aim_input_art, range="B9:BE11")
ref_art_ltfu = read_excel(ref_xlsx, sheet=tab_aim_input_art, range="B14:BE16")

cat(sprintf("dp.inputs.tfr:                     %s\n", check(all(tfr == ref_tfr))))
cat(sprintf("dp.inputs.srb:                     %s\n", check(all(srb == ref_srb))))

cat(sprintf("dp.inputs.adult.art.initiations:   %s\n", check(all(ref_adult_inits[1:2,] == adult_inits_wide[,2:ncol(adult_inits_wide)]))))
cat(sprintf("dp.inputs.adult.art.reinitiations: %s\n", check(all(ref_adult_inits[4,  ] == adult_restart_wide))))
cat(sprintf("dp.inputs.adult.art.ltfu:          %s\n", check(all(ref_art_ltfu[2,] == art_adult_ltfu))))
cat(sprintf("dp.inputs.child.art.initiations:   %s\n", check(all(ref_child_inits[1,] == child_inits_wide))))
cat(sprintf("dp.inputs.child.art.reinitiations: %s\n", check(all(ref_child_inits[2,] == child_restart_wide))))
cat(sprintf("dp.inputs.child.art.ltfu:          %s\n", check(all(ref_art_ltfu[1,] == art_child_ltfu))))



