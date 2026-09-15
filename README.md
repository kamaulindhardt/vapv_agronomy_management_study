# vapv_agronomy_management_study

A reproducible, self-contained [`targets`](https://docs.ropensci.org/targets/)
pipeline for **Study 2** (agronomic management) of a PhD research programme on
vertical agrivoltaics at Aarhus University, Foulum (Value4Farm project): the
effects of fertilisation (biogas digestate vs. mineral fertiliser) and crop
residue management (removed vs. retained), and their interaction with
position relative to the vertical PV panels, on crop yield and quality,
nitrate leaching, and soil water content.

This repository is the FAIR/open-source Supplementary Material for the
manuscript *"Crop and soil responses to fertilisation and residue management
under vertical agrivoltaics: productivity, nitrogen losses, and carbon
balance."* Every raw input file the analysis depends on is bundled here (see
`1_input_data/`) — no separate repository or dataset access is needed to
reproduce it end to end.

See [`docs/concept_paper_study2_agronomy_management.md`](docs/concept_paper_study2_agronomy_management.md)
for the full scientific framing: aim, objectives, research questions and
hypotheses, site and experimental design, preliminary results, and open
methods questions.

## Repository layout

```
1_input_data/       raw input files this pipeline reads (self-contained, read-only)
2_preprocessing/     import, cleaning, and covariate-derivation R/ code
3_analysis/          statistical models, DL/kriging/LER methods, and all
                      figure/table-producing R/ code + the report source
4_outputs/           everything tar_make() writes (figures, tables, the
                      rendered report, and gitignored intermediate data)
docs/                concept paper and planning documents
_targets.R           pipeline definition (stays at the project root -- a
                      `targets` package requirement, the one exception to
                      the numbered layout above)
renv.lock, renv/, .Rprofile, DESCRIPTION   pinned R environment (renv)
vapv_agronomy_management_study.Rproj       RStudio project file (see Setup)
```

Each of the four numbered folders has its own `README.md` describing its
contents in more detail. See `1_input_data/README.md` specifically for the
provenance of every bundled data file and how to refresh one if corrected
data arrives upstream.

## Setup

Open `vapv_agronomy_management_study.Rproj` in RStudio first (double-click it,
or File > Open Project) -- this sets the working directory to the project
root and is what the `here::here()` calls throughout `2_preprocessing/*.qmd`
and `3_analysis/*.qmd` (see below) rely on to find files regardless of which
document you have open. Then, in the R console:

```r
install.packages("renv")
renv::restore()       # installs the exact package versions in renv.lock
targets::tar_make()   # runs the full pipeline
```

Regenerating the field-management calendar (`4_outputs/data/field_management/`)
requires a `python` interpreter with `pandas` on `PATH`; if none is found,
the pipeline falls back to the two CSVs already committed there.

The rendered summary report lands at `4_outputs/reports/study2_pipeline_summary.html`.
Inspect the pipeline with `targets::tar_manifest()` (list every target) or
`targets::tar_visnetwork()` (interactive dependency graph); read any target's
value directly with `targets::tar_read(<name>)`.

### Running individual steps interactively

Each `2_preprocessing/*.qmd` and `3_analysis/*.qmd` document is a standalone,
human-readable walkthrough of one domain (field management, yield import, the
DL nitrate-leaching method, the statistical models, ...) that calls the same
`R/` functions the pipeline does -- open one in RStudio (with the `.Rproj`
project active) and either Render the whole document or run its chunks one
at a time to inspect intermediate objects. See `2_preprocessing/README.md`
and `3_analysis/README.md` for what each document covers.

## Relationship to `V4F_agrivoltaics`

The data bundled under `1_input_data/` was copied from the companion
[`V4F_agrivoltaics`](https://github.com/kamaulindhardt/V4F_agrivoltaics)
repository, which remains the canonical source for **new** data collection
and for the ad hoc analysis notebooks this pipeline's `R/` functions were
ported from (documented function-by-function in each file's header comment),
as well as the Study 1 (microclimate/DAISY) and Study 3 (whole-system)
analyses, which are not part of this pipeline.

## What this pipeline does that the original notebooks didn't

A generic statistical model factory (`3_analysis/R/functions_statistics.R`,
`fit_glmm_outcome()`), applied identically across four outcomes via
`tarchetypes::tar_map()`, that -- for the first time -- includes crop-residue
management (`RESIDUE`) as a predictor (previously derived as a data column
but never entered into a fitted model) and gives NO3-N leaching and TDR soil
water content a formal statistical test (previously reported only as
descriptive means ± SE).

Preliminary figures for crop yield and percolation-adjusted (Drainage-Linear
/ Vogeler et al. 2020) NO3-N leaching -- by fertilisation, residue
management, PV-panel location and crop -- are generated by
`3_analysis/R/functions_yield_analysis.R`'s and `functions_no3_leaching.R`'s
`plot_*()` functions and land under `4_outputs/preliminary_figures/<outcome_id>/`;
the rendered report embeds them via the matching `fig_*` targets.

## License

CC0 1.0 Universal -- see `LICENSE`. See `CITATION.cff` for citation metadata.
