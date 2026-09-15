# 3_analysis

Statistical methods and data-visualisation code — consumes the
analysis-ready datasets from `2_preprocessing/`, fits models, computes
derived scientific quantities, and produces every figure/table. "Scripts"
here means the `R/` function library orchestrated by the project's
`_targets.R` pipeline (each function is a cacheable, individually testable
pipeline step), rather than flat top-to-bottom scripts.

`R/` — sourced by `_targets.R`'s `tar_source("3_analysis/R")`:

| File | Role |
|---|---|
| `functions_statistics.R` | Generic GLMM model factory (`fit_glmm_outcome()`), applied identically across all four outcomes |
| `functions_no3_leaching.R` | Drainage-Linear NO3-N leaching method (Vogeler et al. 2020) + its 4 preliminary figures + Daisy cross-validation |
| `functions_yield_analysis.R` | Yield summary table + its 2 preliminary figures |
| `functions_soil_survey.R` | Soil-texture kriging (derived spatial covariates) |
| `functions_ler.R` | Apparent Land Equivalent Ratio (a-LER) calculation + figures |
| `functions_fertilisation.R` | Digestate/mineral-fertiliser NPK-equivalent calculators |
| `utils.R` | Shared colour palettes, `save_report_figure()`/`save_report_table()`, small helpers |

`reports/study2_pipeline_summary.Rmd` — the report `tar_render()`s into
`4_outputs/reports/`; every number and figure in it is `tar_load()`ed live
from the pipeline, never hand-edited.

See each file's header comment for what it was ported from and why.
