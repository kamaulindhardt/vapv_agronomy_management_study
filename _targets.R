# Study 2 (agronomic management) targets pipeline -- vertical agrivoltaics,
# Value4Farm. See docs/concept_paper_study2_agronomy_management.md for the
# scientific framing (aim, RQs, hypotheses) and README.md for the full
# migration-status table (what this pipeline covers vs. Study 1/3, which
# remain as ad hoc .Rmd scripts in the V4F_agrivoltaics repo).
#
# Run with: targets::tar_make()
# Inspect with: targets::tar_manifest(), targets::tar_visnetwork()

library(targets)
library(tarchetypes)

tar_source("R")

tar_option_set(
  packages = c(
    "tidyverse", "readxl", "janitor", "stringr", "lubridate", "purrr",
    "glmmTMB", "DHARMa", "emmeans", "car", "lme4", "broom.mixed",
    "gt", "kableExtra", "knitr", "patchwork",
    "sf", "gstat", "terra"
  ),
  format = "rds"
)

# ================================================================
# DATA LOCATION -- this repo holds the Study 2 analysis CODE only.
# The raw/intermediate DATA and the Daisy model outputs it reads live in
# the (much larger, separately-versioned) V4F_agrivoltaics repo. By default
# this looks for that repo as a sibling directory; override by setting the
# V4F_DATA_ROOT environment variable (e.g. in a project-local .Renviron)
# if your checkout lives somewhere else.
# ================================================================
V4F_DATA_ROOT <- Sys.getenv("V4F_DATA_ROOT", unset = "../V4F_agrivoltaics")
if (!dir.exists(V4F_DATA_ROOT)) {
  stop(
    "Can't find the V4F_agrivoltaics data repo at '", normalizePath(V4F_DATA_ROOT, mustWork = FALSE), "'.\n",
    "Set the V4F_DATA_ROOT environment variable to its location, e.g. in a ",
    "project-local .Renviron:\n  V4F_DATA_ROOT=C:/Users/.../V4F_agrivoltaics"
  )
}
data_path  <- function(...) file.path(V4F_DATA_ROOT, "DATA", ...)
daisy_path <- function(...) file.path(V4F_DATA_ROOT, "FIELD_WATER_BALANCE_DAISY", ...)

# Static configuration for tar_map() below -- this table must be a plain R
# object available when _targets.R is sourced (it decides WHICH targets to
# generate), not a tar_target() result, which would only exist after
# tar_make() has already run.
outcome_specs <- tibble::tibble(
  outcome_id = c("yield_dry", "quality_n", "no3_leaching", "soil_water_vwc"),
  data_target = rlang::syms(c(
    "study2_yield_analysis_ready", "study2_yield_analysis_ready",
    "no3_leaching_analysis_ready", "tdr_analysis_ready"
  )),
  model_fn = rlang::syms(c(
    "fit_yield_dry_model", "fit_quality_model", "fit_no3_model", "fit_soil_water_model"
  )),
  emm_fn = rlang::syms(c(
    "emm_yield_dry_model", "emm_quality_model", "emm_no3_model", "emm_soil_water_model"
  ))
)

list(

  # ============================================================
  # STAGE 1: RAW FILE TARGETS -- invalidate when source data changes
  # ============================================================

  tar_target(raw_yield_xlsx_2023,
             data_path("CROP_YIELD_QUALITY_DATA", "2023", "HYPERFARM_YIELD_DATA_2023_v2.xlsx"),
             format = "file"),
  tar_target(raw_yield_xlsx_2024,
             data_path("CROP_YIELD_QUALITY_DATA", "2024", "HYPERFARM_VALUE4FARM_YIELD_DATA_2024_v3.xlsx"),
             format = "file"),
  tar_target(raw_yield_xlsx_2025,
             data_path("CROP_YIELD_QUALITY_DATA", "2025", "VALUE4FARM_YIELD_DATA_2025.xlsx"),
             format = "file"),
  tar_target(raw_soil_water_xlsx,
             data_path("SOIL_WATER", "VALUE4FARM_SOIL_WATER_DATA.xlsx"),
             format = "file"),
  tar_target(raw_soil_survey_xlsx,
             data_path("SOIL_SURVEY", "SOIL_SURVEY_DATA_COMBINED.xlsx"),
             format = "file"),
  tar_target(raw_disease_xlsx,
             data_path("CROP_YIELD_QUALITY_DATA", "2025", "WW_sortskimmel_reg_30_07_2025.xlsx"),
             format = "file"),
  tar_target(raw_pv_panel_xlsx,
             data_path("PV_DATA", "PV_PANEL_DATA.xlsx"),
             format = "file"),

  tar_target(raw_daisy_drainage_a2, daisy_path("OUTPUT", "V4F_drainage_A2_1m_daily.rds"), format = "file"),
  tar_target(raw_daisy_drainage_a3, daisy_path("OUTPUT", "V4F_drainage_A3_1m_daily.rds"), format = "file"),
  tar_target(raw_daisy_no3_a2, daisy_path("OUTPUT", "V4F_NO3_leaching_A2_1m_daily.rds"), format = "file"),
  tar_target(raw_daisy_no3_a3, daisy_path("OUTPUT", "V4F_NO3_leaching_A3_1m_daily.rds"), format = "file"),

  tar_target(mngmt_script_file, data_path("FIELD_MANAGEMENT", "read_foulumngaard_mngmnt.py"), format = "file"),
  tar_target(mngmt_xlsx_files,
             list.files(data_path("FIELD_MANAGEMENT", "COMPLETE_YEAR_MNGMT"),
                        pattern = "^Handlinger_.*\\.xlsx$", full.names = TRUE),
             format = "file"),
  tar_target(raw_tdr_clean_files,
             list.files(data_path("TDR_DATA", "CLEAN_TDR_DATA"), pattern = "\\.csv$", full.names = TRUE),
             format = "file"),

  # ============================================================
  # STAGE 2: IMPORT / CLEAN
  # ============================================================

  # -- Field management (external python step; see R/functions_management.R).
  # Reads/writes back into the V4F_agrivoltaics DATA/ tree (that's where the
  # source Handlinger_*.xlsx files and the crop_calendar.csv/
  # management_actions.csv outputs belong -- not duplicated into this repo).
  tar_target(mngmt_csv_outputs,
             { mngmt_xlsx_files  # dependency only: forces rerun when Handlinger_*.xlsx change
               run_management_import(mngmt_script_file,
                                      mngmt_dir = data_path("FIELD_MANAGEMENT", "COMPLETE_YEAR_MNGMT"),
                                      out_dir   = data_path("FIELD_MANAGEMENT")) },
             format = "file"),
  tar_target(crop_calendar, load_crop_calendar(mngmt_csv_outputs[2])),

  # -- Crop yield & quality (Study 2 core dataset) --
  tar_target(merged_yield_data,
             load_merged_yield_data(raw_yield_xlsx_2023, raw_yield_xlsx_2024, raw_yield_xlsx_2025)),
  tar_target(study2_yield_analysis_ready, add_study2_covariates(merged_yield_data)),

  # -- Soil water / suction cups --
  tar_target(soil_water_raw, load_soil_water_xlsx(raw_soil_water_xlsx)),
  tar_target(soil_water_window, soil_water_measurement_window(soil_water_raw)),
  tar_target(strip_crop_lookup_tbl,
             lookup_strip_crop(crop_calendar, soil_water_window["start"], soil_water_window["end"])),
  tar_target(soil_water_clean, clean_soil_water(soil_water_raw, strip_crop_lookup_tbl)),
  tar_target(soil_water_long, reshape_soil_water_long(soil_water_clean)),

  # -- TDR soil water content --
  tar_target(tdr_clean_combined,
             { raw_tdr_clean_files; combine_clean_tdr_data(data_path("TDR_DATA", "CLEAN_TDR_DATA")) }),
  tar_target(tdr_analysis_ready, derive_tdr_covariates(tdr_clean_combined)),

  # -- Winter wheat disease severity --
  tar_target(disease_severity_data, { raw_disease_xlsx; load_disease_severity_data(raw_disease_xlsx) }),

  # ============================================================
  # STAGE 3: DERIVED SPATIAL COVARIATES
  # ============================================================

  tar_target(soil_kriging_results, { raw_soil_survey_xlsx; krige_soil_covariates(raw_soil_survey_xlsx) }),

  # ============================================================
  # STAGE 4: NO3 LEACHING (DL METHOD) -- see R/functions_no3_leaching.R
  # ============================================================

  tar_target(no3_leaching_result,
             { raw_daisy_drainage_a2; raw_daisy_drainage_a3
               build_no3_leaching_dataset(soil_water_long, daisy_output_dir = daisy_path("OUTPUT")) }),
  tar_target(no3_leaching_analysis_ready, no3_leaching_result$total),
  tar_target(daisy_no3_leaching,
             { raw_daisy_no3_a2; raw_daisy_no3_a3; load_daisy_no3_leaching(daisy_path("OUTPUT")) }),
  tar_target(no3_dl_vs_daisy_validation,
             validate_dl_vs_daisy(no3_leaching_result$daily, daisy_no3_leaching)),

  # ============================================================
  # STAGE 5: STATISTICAL MODELS -- generic factory mapped over outcomes
  # (R/functions_statistics.R). Each branch fits, DHARMa-diagnoses,
  # car::Anova Type II tests and emmeans-summarises its model in ONE target
  # (run_outcome_model()) and returns only portable results -- see that
  # function's docstring for why the live glmmTMB object never crosses a
  # target boundary.
  # ============================================================

  tar_map(
    values = outcome_specs,
    names = "outcome_id",

    tar_target(model_result, run_outcome_model(data_target, model_fn, emm_fn, outcome_label = outcome_id)),
    tar_target(gt_summary, make_anova_gt_table_from_result(model_result))
  ),

  tar_target(model_overview, make_model_overview_table(list(
    yield_dry = model_result_yield_dry, quality_n = model_result_quality_n,
    no3_leaching = model_result_no3_leaching, soil_water_vwc = model_result_soil_water_vwc
  ))),

  # ============================================================
  # STAGE 6: OTHER STUDY 2 OUTPUTS (fertilisation planning, LER, disease)
  # ============================================================

  # Reference fertilisation plan built from the most recent digestate lab
  # certificate on file (values below from FERTILISATION_CALCULATION.Rmd's
  # worked example -- update with each new AgroLab batch result).
  tar_target(fertilisation_plan_example, calculate_fertilisation_plan(
    digestate_nh4_conc_kg_per_ton = 1.35,
    digestate_p_conc_kg_per_ton = 0.56,
    digestate_k_conc_kg_per_ton = 3.8,
    split_applications_ton_ha = c(35, 35),
    extra_n_applied_in_yara_extran_kg_ha = 230
  )),

  tar_target(disease_dsi_summary, disease_severity_data),

  # a-LER: needs BOTH PV setups (VERTICAL + SOUTH_ORIENTED), so it reads
  # merged_yield_data (pre-VERTICAL-filter) rather than study2_yield_analysis_ready.
  # Flagged in the concept paper as needing reconciliation with measured (not
  # reference-assumption) PV electricity figures -- this target is a starting
  # point, not a finished result, and is deliberately NOT a dependency of any
  # other branch so it can fail independently without blocking the rest of
  # the pipeline.
  tar_target(pv_panel_data, preprocess_pv_data(readxl::read_excel(raw_pv_panel_xlsx))),
  tar_target(ler_grain_result, tryCatch(
    calculate_a_LER(process_yield_data(merged_yield_data, pv_panel_data, "GRAIN",
                                        reference_data = tibble::tibble(
                                          PV_SETUP_2 = "REFERENCE", mean_dry_weight = NA_real_,
                                          GCR = 0.5, LL_HA = 0, TOTAL_AREA_OCCUPIED_M2 = 1104,
                                          ELECTRICITY_KW_M2_YR = 93.47, ELECTRICITY_OUTPUT_MWH_HA_YR = 935))),
    error = function(e) {
      message("a-LER calculation failed -- see docs/concept_paper_study2_agronomy_management.md TODOs. Error: ", e$message)
      NULL
    }
  )),
  tar_target(ler_clover_result, tryCatch(
    calculate_a_LER(process_yield_data(merged_yield_data, pv_panel_data, "GRASS_CLOVER",
                                        reference_data = tibble::tibble(
                                          PV_SETUP_2 = "REFERENCE", mean_dry_weight = NA_real_,
                                          GCR = 0.5, LL_HA = 0, TOTAL_AREA_OCCUPIED_M2 = 1104,
                                          ELECTRICITY_KW_M2_YR = 93.47, ELECTRICITY_OUTPUT_MWH_HA_YR = 935))),
    error = function(e) {
      message("a-LER (grass-clover) calculation failed -- see docs/concept_paper_study2_agronomy_management.md TODOs. Error: ", e$message)
      NULL
    }
  )),

  # ============================================================
  # STAGE 7: REPORT
  # ============================================================

  tarchetypes::tar_render(
    report_study2_summary,
    "reports/study2_pipeline_summary.Rmd",
    output_dir = "PIPELINE_OUTPUTS/reports"
  )

  # ============================================================
  # STUDY 1 (microclimate + DAISY) -- SCAFFOLDED, NOT PORTED THIS PASS.
  # DAISY's existing .rds outputs are consumed above (STAGE 4) as file-target
  # inputs only; the DAISY model orchestration itself
  # (FIELD_WATER_BALANCE_DAISY.Rmd) and the microclimate QC/import pipeline
  # remain scripts in the V4F_agrivoltaics repo. See README.md.
  # ============================================================

  # ============================================================
  # STUDY 3 (whole-system) -- SCAFFOLDED, NOT PORTED THIS PASS.
  # PV inverter QC/ML-gapfill and the whole-system assessment remain
  # scripts in the V4F_agrivoltaics repo. See README.md.
  # ============================================================
)
