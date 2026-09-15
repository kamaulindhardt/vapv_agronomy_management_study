# tables

Destination for `save_report_table()` (`3_analysis/R/utils.R`), which writes
a `gt` table as HTML plus a plain `.csv` of its underlying data. Not yet
called by `_targets.R` — the pipeline's statistical summaries currently live
only inside the rendered report (`4_outputs/reports/study2_pipeline_summary.html`)
as `gt`/`kable` tables, not as standalone files. Empty until a `tar_target()`
calls `save_report_table()`.
