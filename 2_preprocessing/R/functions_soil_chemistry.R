# IUNG lab soil-chemistry results (RESULTS_SOIL_ANALYSIS_IUNG.xlsx): pH,
# organic carbon/nitrogen, available nutrients and trace metals from two
# survey rounds (2024 "term 0" baseline; 2025 "term 1"/"term 2" follow-up),
# for the Fig03 soil-carbon manuscript figure. Import/clean only -- modelling
# lives in 3_analysis/R/functions_statistics.R, plotting in
# 3_analysis/R/fig03_soil_carbon.R.
#
# IMPORTANT SCOPE CAVEAT: sample codes are NOT the same physical points
# resampled over time (every `sample code` value in the sheet is unique --
# confirmed empirically), so `term`/`year` differences are a comparison
# between DIFFERENT nearby sampling points at two points in time, not a
# true paired before/after measurement of the same spot. Treat any
# term-related contrast as "SOC across survey rounds", never as "change in
# SOC" (Delta SOC) for an individual location -- see the Fig03 caption.
#
# FERTILISATION coverage caveat: the sheet's own `fertilization`/`straw`
# columns are unreliable (73/133 rows NA, and the 60 non-NA values are only
# "no"/"?" -- not the study's BIOGAS_DIGESTATE/MINERAL_FERTILISER vocabulary,
# and not further documented). Where a sample code encodes a nest + plot
# number (term 1 "NEST/Pn" and term 2 "STRIPNEST n" formats), FERTILISATION
# is instead derived via the SAME hardcoded nest+plot treatment lookup used
# for TDR (tdr_treatment_lookup(), functions_tdr.R) -- real field-layout
# knowledge, not inferred from this sheet. That lookup only covers the TDR
# instrumentation nests, so most rows still end up NA; RESIDUE is never
# derivable this way (same limitation as the TDR branch) and is always NA.

#' Parse an IUNG `sample code` value into (strip, nest, plot_no). Three
#' formats occur in the source sheet:
#'   "A2a/N7"     (term 0, 2024 baseline)   -> strip = "A2a", nest = "N7"
#'   "N10/P4"     (term 1, 2025)            -> nest = "N10", plot_no = 4
#'   "A2aN7 1"    (term 2, 2025)            -> strip = "A2a", nest = "N7", plot_no = 1
#' Returns NA for any component not present in a given code's format,
#' rather than guessing.
parse_iung_sample_code <- function(sample_code) {
  strip   <- stringr::str_extract(sample_code, "^[A-Z][0-9][ab]")
  rest    <- stringr::str_remove(sample_code, "^[A-Z][0-9][ab]/?")
  nest    <- stringr::str_extract(rest, "^[NS][0-9]+")
  plot_no <- dplyr::case_when(
    stringr::str_detect(rest, "/P[0-9]+$") ~ stringr::str_extract(rest, "(?<=/P)[0-9]+$"),
    stringr::str_detect(rest, "\\s[0-9]+$") ~ stringr::str_extract(rest, "[0-9]+$"),
    TRUE ~ NA_character_
  )
  tibble::tibble(sample_code = sample_code, strip = strip, nest = nest,
                 plot_no = as.integer(plot_no))
}

#' Load and clean the IUNG soil-chemistry workbook into an analysis-ready
#' data frame, scoped to Study 2 (vertical PV setup only, matching
#' add_study2_covariates()'s PV_SETUP == "VERTICAL" filter convention -- the
#' 18 "fixed-tilted" baseline rows belong to the south-oriented reference
#' setup, out of scope here).
#'
#' @param path path to RESULTS_SOIL_ANALYSIS_IUNG.xlsx
build_iung_soil_dataset <- function(path) {
  raw <- readxl::read_excel(path, sheet = "Arkusz1")
  names(raw) <- c(
    "sample_code", "year", "term", "panel_type", "shading", "position",
    "fertilization_raw", "straw_raw", "pH", "dry_matter_pct", "SOC_PCT",
    "no3_mg_kg", "nh4_mg_kg", "p2o5_mg_100g", "k2o_mg_100g", "mg_mg_100g",
    "cd_mg_kg", "cr_mg_kg", "cu_mg_kg", "ni_mg_kg", "pb_mg_kg", "zn_mg_kg",
    "N_PCT", "C_PCT", "note"
  )
  raw <- dplyr::filter(raw, !is.na(sample_code))

  codes <- parse_iung_sample_code(raw$sample_code)

  raw |>
    dplyr::bind_cols(dplyr::select(codes, strip, nest, plot_no)) |>
    dplyr::filter(panel_type == "vertical") |>
    dplyr::mutate(
      LOCATION = dplyr::case_when(
        shading == "with panel"    ~ "BETWEEN_PV",
        shading == "without panel" ~ "OUTSIDE_PV",
        TRUE ~ NA_character_
      ),
      SUB_STRIP_LOCATION = toupper(position),
      SURVEY_ROUND = dplyr::case_when(
        year == 2024 & term == 0 ~ "2024_baseline",
        year == 2025 & term == 1 ~ "2025_T1",
        year == 2025 & term == 2 ~ "2025_T2",
        TRUE ~ NA_character_
      ),
      # Real field-layout lookup (functions_tdr.R), reused here because the
      # term-1/term-2 sample codes carry the same nest+plot_no grain the TDR
      # branch uses -- NOT derived from this sheet's own unreliable
      # fertilization_raw/straw_raw columns (kept only for provenance).
      FERTILISATION = tdr_treatment_lookup(nest, plot_no),
      RESIDUE = NA_character_
    ) |>
    dplyr::select(
      sample_code, strip, nest, plot_no, YEAR = year, TERM = term, SURVEY_ROUND,
      LOCATION, SUB_STRIP_LOCATION, FERTILISATION, RESIDUE,
      pH, dry_matter_pct, SOC_PCT, N_PCT, C_PCT,
      no3_mg_kg, nh4_mg_kg, p2o5_mg_100g, k2o_mg_100g, mg_mg_100g,
      cd_mg_kg, cr_mg_kg, cu_mg_kg, ni_mg_kg, pb_mg_kg, zn_mg_kg,
      fertilization_raw, straw_raw
    )
}
