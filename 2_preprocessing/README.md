# 2_preprocessing

Import, cleaning, and derivation code — reads raw files from `1_input_data/`
and produces the analysis-ready datasets that `3_analysis/` consumes. No
fitted statistical models and no plotting live here (see `3_analysis/`).

`R/` — one file per data domain, all sourced by `_targets.R`'s
`tar_source("2_preprocessing/R")`:

| File | Domain |
|---|---|
| `functions_yield_quality.R` | Crop yield/quality workbooks → `study2_yield_analysis_ready` |
| `functions_soil_water.R` | Suction-cup NO3-N concentration workbook → long-format series |
| `functions_tdr.R` | TDR soil-water-content CSVs → `tdr_analysis_ready` |
| `functions_management.R` | Field management records → crop calendar / strip-crop lookup |
| `functions_disease.R` | Winter-wheat disease severity register → DSI |

No standalone exploratory-data-analysis (EDA) script exists yet in this
pipeline (the EDA that motivated some of the cleaning choices above — e.g.
NA/UD proportion checks — lives only in the source notebooks in
`V4F_agrivoltaics`); a future EDA pass belongs in this folder once written.
