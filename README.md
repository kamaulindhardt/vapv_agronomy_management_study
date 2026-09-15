# vapv_agronomy_management_study

A reproducible [`targets`](https://docs.ropensci.org/targets/) pipeline for **Study 2** (agronomic management) of a PhD research programme on vertical agrivoltaics at Aarhus University, Foulum (Value4Farm project): the effects of fertilisation (biogas digestate vs. mineral fertiliser) and crop residue management (removed vs. retained), and their interaction with position relative to the vertical PV panels, on crop yield and quality, nitrate leaching, and soil water content.

See [`docs/concept_paper_study2_agronomy_management.md`](docs/concept_paper_study2_agronomy_management.md) for the full scientific framing: aim, objectives, research questions and hypotheses, site and experimental design, preliminary results, and open methods questions.

## Relationship to `V4F_agrivoltaics`

This repo holds the Study 2 analysis **code** only. The raw and intermediate **data** it reads (crop yield/quality workbooks, soil-water suction-cup data, TDR soil moisture, soil survey, Daisy crop-soil model outputs, PV electricity data, field management records) live in the separate, much larger [`V4F_agrivoltaics`](https://github.com/kamaulindhardt/V4F_agrivoltaics) repository, which also holds the full set of ad hoc analysis notebooks this pipeline's `R/` functions were ported from (documented function-by-function in each file's header comment) and the Study 1 (microclimate/DAISY) and Study 3 (whole-system) analyses, not yet part of this pipeline.

By default, `_targets.R` looks for `V4F_agrivoltaics` as a **sibling directory**:

```
Documents/
├── V4F_agrivoltaics/          <- data + original notebooks
└── vapv_agronomy_management_study/   <- this repo
```

If your checkout of `V4F_agrivoltaics` lives somewhere else, set the `V4F_DATA_ROOT` environment variable to its path (e.g. in a project-local `.Renviron` file, not committed):

```
V4F_DATA_ROOT=C:/path/to/V4F_agrivoltaics
```

## Setup

```r
install.packages("renv")
renv::restore()       # installs the exact package versions in renv.lock
targets::tar_make()   # runs the full pipeline
```

Regenerating the field-management calendar requires a `python` interpreter with `pandas` on `PATH`; if none is found, the pipeline falls back to the CSVs already committed in `V4F_agrivoltaics/DATA/FIELD_MANAGEMENT/`.

The rendered summary report lands at `PIPELINE_OUTPUTS/reports/study2_pipeline_summary.html`.

## Repository layout

```
_targets.R                 pipeline definition
R/                          function library (one file per analysis domain)
renv.lock, renv/, .Rprofile, DESCRIPTION   pinned R environment (renv)
reports/                    R Markdown source rendered by the pipeline
PIPELINE_OUTPUTS/           pipeline-owned outputs (figures/tables tracked; data/ gitignored)
docs/                       concept paper and planning documents
```

Inspect the pipeline with `targets::tar_manifest()` (list every target) or `targets::tar_visnetwork()` (interactive dependency graph); read any target's value directly with `targets::tar_read(<name>)`.

## What this pipeline does that the original notebooks didn't

A generic statistical model factory (`R/functions_statistics.R`, `fit_glmm_outcome()`), applied identically across four outcomes via `tarchetypes::tar_map()`, that -- for the first time -- includes crop-residue management (`RESIDUE`) as a predictor (previously derived as a data column but never entered into a fitted model) and gives NO3-N leaching and TDR soil water content a formal statistical test (previously reported only as descriptive means ± SE).

## License

CC0 1.0 Universal -- see `LICENSE`. See `CITATION.cff` for citation metadata.
