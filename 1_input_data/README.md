# 1_input_data

Raw input files this pipeline reads — nothing in this folder is ever written to by
`targets::tar_make()`; it is a read-only, checksummable snapshot.

## Provenance

Copied on 2026-09-15 from the companion [`V4F_agrivoltaics`](https://github.com/kamaulindhardt/V4F_agrivoltaics)
repository (Aarhus University, Foulum — Value4Farm project), which remains the
canonical source for new data collection and for Study 1 (microclimate/DAISY)
and Study 3 (whole-system), which are not part of this pipeline. To refresh a
file here after new/corrected data lands upstream, re-copy it from the
corresponding path below — there is no automatic sync.

| Folder | Source in `V4F_agrivoltaics` | Contents |
|---|---|---|
| `crop_yield_quality/` | `DATA/CROP_YIELD_QUALITY_DATA/<year>/` | Yearly yield & quality workbooks (2023-2025), sheet `ALL_CROPS_COMBINED` |
| `disease/` | `DATA/CROP_YIELD_QUALITY_DATA/2025/` | Winter wheat disease severity register (2025) |
| `soil_water/` | `DATA/SOIL_WATER/` | Suction-cup NO3-N concentration workbook, 1 m depth |
| `soil_survey/` | `DATA/SOIL_SURVEY/` | Combined soil texture/survey workbook (`SOIL_SURVEY_DATA_COMBINED.xlsx`, single baseline survey, no repeat structure) + IUNG lab soil-chemistry results (`RESULTS_SOIL_ANALYSIS_IUNG.xlsx`, copied 2026-09-16 — 133 samples across 2 survey rounds, 2024 term 0 and 2025 terms 1/2; pH, C org. %, N %, available P/K/Mg, trace metals; see 2_preprocessing/README.md for caveats on using `term`/`year` as a repeated-measures factor) |
| `pv_data/` | `DATA/PV_DATA/` | PV panel configuration reference data |
| `field_management/` | `DATA/FIELD_MANAGEMENT/COMPLETE_YEAR_MNGMT/` + the import script | Field management records (`Handlinger_*.xlsx`) and `read_foulumngaard_mngmnt.py`, which derives `crop_calendar.csv`/`management_actions.csv` into `4_outputs/data/field_management/` at pipeline run time (those two CSVs are themselves derived, not raw, so they don't live here) |
| `tdr_data/` | `DATA/TDR_DATA/CLEAN_TDR_DATA/` | Cleaned TDR soil-water-content readings, one CSV per sampling round |
| `daisy_water_balance/` | `FIELD_WATER_BALANCE_DAISY/OUTPUT/` | Daisy v7.1.3 crop-soil model outputs (drainage and NO3-N leaching, per rotation strip A2/A3) — these are *model outputs* from a Study-1 DAISY run, consumed here as read-only inputs to the Drainage-Linear NO3-leaching method; the DAISY model orchestration itself is not part of this pipeline |

Total: ~8.2 MB.
