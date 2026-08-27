# SpectrumUtils 0.3.0

## New features

### Goals RSM inputs (`R/extract-hv.R`)

Added extraction functions for Goals Risk-Structured Model (RSM) assumptions
from `.HV` module data, developed and validated against Goals RSM PJNZ files
in a companion analysis project:

- Scalar epidemic parameters: `hv.inputs.trans.hiv.f`, `hv.inputs.trans.mult.m`,
  `hv.inputs.trans.mult.sti`, `hv.inputs.trans.mult.msm`, `hv.inputs.condom.eff`,
  `hv.inputs.epidemic.start.year`, `hv.inputs.initial.pulse`,
  `hv.inputs.months.primary.stage`
- Behavioural time series: `hv.inputs.age.first.sex`, `hv.inputs.force.inf`,
  `hv.inputs.idu.sharing`, `hv.inputs.num.partners`, `hv.inputs.sex.acts`,
  `hv.inputs.sti.prev`, `hv.inputs.condom.percent`
- Static risk-group/behavioural inputs: `hv.inputs.perc.married`,
  `hv.inputs.infectiousness`, `hv.inputs.behavior`, `hv.inputs.recruitment`,
  `hv.inputs.impact.matrix`
- Model fitting configuration: `hv.inputs.fit.params`, `hv.inputs.fit.control`
- Wrappers: `extract_all_hv()` returns a tidy `list(const, time_varying)` for
  a single Goals RSM PJNZ file; `extract_all_hv_folder()` runs this across
  every PJNZ in a folder and combines the results, tagging rows with `iso3`
  derived from the filename

`hv.inputs.hiv.prevalence()` and `hv.inputs.calibration.data()` already
existed and are unchanged.

New shared label constants added to `strata.labels`:
`rsm.condom.groups`, `rsm.infectiousness.stages`, `rsm.impact.interventions`,
`rsm.impact.outcomes`, `rsm.fit.param.names`.

### Goals analysis scripts (`R/analyse-goals.R`)

- `compare_goals_files()` — compares Goals RSM inputs between two PJNZ files
  (e.g. two scenarios, or before/after an update) and reports differences in
  both the constant/static and time-varying indicators.
- `plot_goals_fit()` — plots modelled HIV prevalence against calibration
  (survey/study) data points with confidence intervals, one plot per
  population, to visually check fit quality.

### Package dependencies

Added `reshape2`, `openxlsx`, and `ggplot2` to `Imports` (previously used
without being declared).

## Known limitations carried over from the source analysis

- `<RiskGroupPercent MV>` is not yet implemented; risk group percent labels
  are not present in the `.HV` file and require additional row-mapping code.
- `hv.inputs.months.primary.stage()` may be absent in some file versions; it
  is wrapped in `tryCatch()` inside `extract_all_hv()`.
