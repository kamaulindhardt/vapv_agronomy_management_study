# 4_outputs

Everything `targets::tar_make()` writes. Figures/tables/reports are tracked
in git (small, reviewable, and *are* the actual supplementary-material
deliverables); `data/` is gitignored except the one derived dataset kept as
a python-free fallback (see below) — regenerable, not meant to be reviewed
file-by-file.

| Folder | Contents |
|---|---|
| `preliminary_figures/` | Draft figures made during pipeline development — not yet reviewed for the manuscript |
| `main_figures/` | Reserved: manuscript-body figures (empty until the figure list is finalised) |
| `suppl_figures/` | Reserved: manuscript Supplementary Material figures (empty until finalised) |
| `tables/` | `save_report_table()` output — not yet used (all current tables live inside the rendered report instead) |
| `reports/` | The rendered `study2_pipeline_summary.html` |
| `data/` | Regenerable derived data. Gitignored, **except** `data/field_management/*.csv` (`crop_calendar.csv`, `management_actions.csv`), which are committed so the pipeline still runs on a fresh clone without a `python` interpreter (see `run_management_import()`, `2_preprocessing/R/functions_management.R`) |
