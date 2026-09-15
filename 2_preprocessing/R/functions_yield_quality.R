# Crop yield & quality: import, cleaning, and the field-layout covariates
# (GLOBAL_SUB_STRIP_LOCATION, ROW, COLUMN, FERTILISATION, RESIDUE) that feed
# the Study 2 statistical models. Ported from CROP_YIELD_ANALYSIS_2025.Rmd
# STEP 0-3 -- the canonical, most current yield/quality script (2023-2025,
# all crops). The four earlier scripts covering overlapping subsets of the
# same data (CROP_ANALYSIS_GC.Rmd, HYPERFARM_2023_CROP_YIELD_ANALYSIS.Rmd,
# HYPERFARM_VALUE4FARM_2023_2024_CROP_YIELD_ANALYSIS.Rmd,
# VALUE4FARM_CROP_YIELD_ANALYSIS_2024.Rmd) are superseded and not ported.
#
# The analysis/visualisation half (summary table + preliminary figures) is
# in 3_analysis/R/functions_yield_analysis.R -- this file is preprocessing
# only, no plotting or statistical-model code.

#' Type-coerce and lightly clean one year's raw yield/quality workbook sheet.
#' Ported verbatim from CROP_YIELD_ANALYSIS_2025.Rmd's preprocessing function.
preprocess_yield_data <- function(data) {
  data <- data |>
    dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(.x, "na"))) |>
    dplyr::mutate(
      MAIN_STRIP = as.factor(MAIN_STRIP),
      SUB_STRIP = as.factor(SUB_STRIP),
      PLOT_NO = as.factor(PLOT_NO),
      PV_SETUP = as.factor(PV_SETUP),
      LOCATION = as.factor(LOCATION),
      SUB_STRIP_LOCATION = as.factor(SUB_STRIP_LOCATION),
      PRODUCT = factor(PRODUCT, levels = c("GRAIN", "GRASS_CLOVER", "STRAW", "RESIDUE", "POD", "GREEN_BIOMASS")),
      CUT = as.factor(CUT),
      CUT_YEAR = as.character(CUT_YEAR),
      DRY_WEIGHT_TON_HA = as.numeric(DRY_WEIGHT_TON_HA),
      FRESH_WEIGHT_PLOT_KG = as.numeric(FRESH_WEIGHT_PLOT_KG),
      FRESH_WEIGHT_TON_HA = as.numeric(FRESH_WEIGHT_TON_HA)
    )

  if ("ANALYSIS_ID" %in% names(data)) data <- dplyr::mutate(data, ANALYSIS_ID = as.numeric(ANALYSIS_ID))
  lobenr_col <- grep("^L.BE_NR$", names(data), value = TRUE)  # Danish "LØBE_NR" (run/serial number); matched by pattern to avoid non-ASCII source-encoding issues
  if (length(lobenr_col) == 1) data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(lobenr_col), as.numeric))

  data |>
    dplyr::mutate(
      CUT_DATE = dplyr::if_else(
        !is.na(CUT_YEAR) & !is.na(CUT_MONTH) & !is.na(CUT_DAY),
        as.Date(paste(CUT_YEAR, CUT_MONTH, CUT_DAY, sep = "-"), format = "%Y-%m-%d"),
        as.Date(NA)
      )
    ) |>
    dplyr::mutate(
      SETUP_BETWEEN_PV_vs_REFERENCE = dplyr::if_else(
        LOCATION == "BETWEEN_PV", as.character(PV_SETUP), "REFERENCE"
      )
    ) |>
    dplyr::mutate(
      SUB_STRIP_LOCATION_BETWEEN_PV_vs_REFERENCE = dplyr::if_else(
        LOCATION == "OUTSIDE_PV", "REFERENCE", as.character(SUB_STRIP_LOCATION)
      )
    ) |>
    dplyr::mutate(
      FRESH_WEIGHT_1_M2_PLOT_G = as.numeric(FRESH_WEIGHT_1_M2_PLOT_G),
      HEIGHT_OF_FIRST_POD_INCERTION_CM = as.numeric(HEIGHT_OF_FIRST_POD_INCERTION_CM),
      NO_PLANTS_M2 = as.numeric(NO_PLANTS_M2),
      CUT_HEIGHT_CM = as.numeric(CUT_HEIGHT_CM)
    )
}

#' Read and preprocess the three canonical yearly yield/quality workbooks
#' (2023/2024/2025, sheet "ALL_CROPS_COMBINED") and bind them into one table.
load_merged_yield_data <- function(
    xlsx_2023 = "1_input_data/crop_yield_quality/2023/HYPERFARM_YIELD_DATA_2023_v2.xlsx",
    xlsx_2024 = "1_input_data/crop_yield_quality/2024/HYPERFARM_VALUE4FARM_YIELD_DATA_2024_v3.xlsx",
    xlsx_2025 = "1_input_data/crop_yield_quality/2025/VALUE4FARM_YIELD_DATA_2025.xlsx",
    sheet = "ALL_CROPS_COMBINED") {

  read_year <- function(path) {
    readxl::read_excel(path, sheet = sheet) |>
      preprocess_yield_data() |>
      dplyr::mutate(
        TS_PCT = as.numeric(TS_PCT),
        DRY_WEIGHT_PLOT_KG = as.numeric(DRY_WEIGHT_PLOT_KG)
      )
  }

  dplyr::bind_rows(read_year(xlsx_2023), read_year(xlsx_2024), read_year(xlsx_2025))
}

#' ROW assignment from MAIN_STRIP suffix (a$/b$) + PLOT_NO, with the
#' SUB_STRIP-specific overrides used for plots without a numbered PLOT_NO
#' (nest locations N1-N3/N25-N27 -> row 2, N4-N6/N28-N30 -> row 9) and for
#' 2023 rows recorded only at the MAIN_STRIP level. This is the field's
#' physical plot layout, not derivable from any other column -- ported
#' verbatim from CROP_YIELD_ANALYSIS_2025.Rmd STEP 3.
derive_row <- function(main_strip, plot_no, sub_strip) {
  dplyr::case_when(
    stringr::str_detect(main_strip, "a$") & plot_no == "1" ~ "1",
    stringr::str_detect(main_strip, "a$") & plot_no == "2" ~ "2",
    stringr::str_detect(main_strip, "a$") & plot_no == "3" ~ "3",
    stringr::str_detect(main_strip, "a$") & plot_no == "4" ~ "4",
    stringr::str_detect(main_strip, "b$") & plot_no == "1" ~ "7",
    stringr::str_detect(main_strip, "b$") & plot_no == "2" ~ "8",
    stringr::str_detect(main_strip, "b$") & plot_no == "3" ~ "9",
    stringr::str_detect(main_strip, "b$") & plot_no == "4" ~ "10",
    sub_strip %in% c("N1", "N2", "N3", "N25", "N26", "N27") & is.na(plot_no) ~ "2",
    sub_strip %in% c("N4", "N5", "N6", "N28", "N29", "N30") & is.na(plot_no) ~ "9",
    stringr::str_detect(main_strip, "a$") & is.na(plot_no) ~ "2",
    stringr::str_detect(main_strip, "b$") & is.na(plot_no) ~ "9",
    TRUE ~ NA_character_
  )
}

#' COLUMN assignment from SUB_STRIP (nest number), five strip-pairs (A1..A5)
#' each spanning three columns. Ported verbatim from
#' CROP_YIELD_ANALYSIS_2025.Rmd STEP 3.
derive_column <- function(sub_strip) {
  dplyr::case_when(
    sub_strip %in% c("N1", "N4")   ~ "1",
    sub_strip %in% c("N2", "N5")   ~ "2",
    sub_strip %in% c("N3", "N6")   ~ "3",
    sub_strip %in% c("N7", "N10")  ~ "5",
    sub_strip %in% c("N8", "N11")  ~ "6",
    sub_strip %in% c("N9", "N12")  ~ "7",
    sub_strip %in% c("N13", "N16") ~ "9",
    sub_strip %in% c("N14", "N17") ~ "10",
    sub_strip %in% c("N15", "N18") ~ "11",
    sub_strip %in% c("N19", "N22") ~ "13",
    sub_strip %in% c("N20", "N23") ~ "14",
    sub_strip %in% c("N21", "N24") ~ "15",
    sub_strip %in% c("N25", "N28") ~ "17",
    sub_strip %in% c("N26", "N29") ~ "18",
    sub_strip %in% c("N27", "N30") ~ "19",
    TRUE ~ NA_character_
  )
}

#' Add the Study 2 modelling covariates to the merged (VERTICAL-only) yield
#' data: FERTILISATION and RESIDUE (both derived from the four-letter
#' TREATMENT code A/B/C/D), CROP_PRODUCT, PRODUCT_SIMPLE,
#' GLOBAL_SUB_STRIP_LOCATION (the 6-level location factor behind the "chi2 =
#' 14.0, df = 5, p = 0.015" result reported for the glmmtmb_loc model), and
#' the ROW/COLUMN spatial-structure factors used as
#' `dispformula = ~ ROW + COLUMN` in the generic statistical model factory.
#' Ported verbatim from CROP_YIELD_ANALYSIS_2025.Rmd STEP 3.
add_study2_covariates <- function(merged_yield_data) {
  merged_yield_data <- merged_yield_data |>
    dplyr::filter(PV_SETUP == "VERTICAL") |>
    dplyr::mutate(
      SUB_STRIP_LOCATION_BETWEEN_PV_vs_REFERENCE = forcats::fct_relevel(
        SUB_STRIP_LOCATION_BETWEEN_PV_vs_REFERENCE, "REFERENCE", "WEST", "MIDDLE", "EAST"
      ),
      YEAR = as.factor(YEAR),
      FERTILISATION = dplyr::case_when(
        TREATMENT %in% c("A", "C") ~ "BIOGAS_DIGESTATE",
        TREATMENT %in% c("B", "D") ~ "MINERAL_FERTILISER"
      ),
      RESIDUE = dplyr::case_when(
        TREATMENT %in% c("A", "B") ~ "REMOVED",
        TREATMENT %in% c("C", "D") ~ "REMAIN"
      )
    )

  merged_yield_data <- merged_yield_data |>
    dplyr::mutate(
      CROP_PRODUCT = paste(CROP, PRODUCT, sep = "_"),
      PRODUCT_SIMPLE = dplyr::case_when(
        PRODUCT == "GRAIN" ~ "GRAIN",
        PRODUCT == "STRAW" ~ "RESIDUE",
        PRODUCT == "POD" ~ "POD",
        PRODUCT == "RESIDUE" ~ "RESIDUE",
        PRODUCT == "GRASS_CLOVER" ~ "BIOMASS",
        PRODUCT == "GREEN_BIOMASS" ~ "BIOMASS"
      ),
      GLOBAL_SUB_STRIP_LOCATION = paste(LOCATION, SUB_STRIP_LOCATION, sep = "_")
    )

  merged_yield_data |>
    dplyr::mutate(
      ROW    = derive_row(MAIN_STRIP, PLOT_NO, SUB_STRIP),
      COLUMN = derive_column(SUB_STRIP)
    ) |>
    dplyr::relocate(PROJECT, CROP, MAIN_STRIP, PLOT_NO, ROW, COLUMN, GLOBAL_SUB_STRIP_LOCATION)
}

#' End-to-end Study 2 yield dataset: three yearly workbooks -> merged,
#' cleaned, VERTICAL-only data with all modelling covariates attached. This
#' is the function `_targets.R` calls to build `study2_yield_analysis_ready`.
build_study2_yield_dataset <- function(xlsx_2023, xlsx_2024, xlsx_2025) {
  load_merged_yield_data(xlsx_2023, xlsx_2024, xlsx_2025) |>
    add_study2_covariates()
}

#' Observation counts by year and crop product -- a data-coverage sanity
#' check (which crop x year combinations have data, and how many
#' observations). New, non-preliminary figure -- uses the validated design
#' system (plot_theme.R) rather than a fresh palette; a sequential blue ramp
#' is appropriate here since count is a magnitude, not a category.
#'
#' @param data study2_yield_analysis_ready (or a subset of it)
plot_yield_data_coverage <- function(data) {
  coverage <- data |>
    dplyr::count(YEAR, CROP_PRODUCT) |>
    dplyr::mutate(text_colour = ifelse(n > max(n) * 0.6, "white", "grey20"))

  ggplot2::ggplot(coverage, ggplot2::aes(x = YEAR, y = CROP_PRODUCT, fill = n)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = n, colour = text_colour), fontface = "bold", size = 3.5) +
    ggplot2::scale_fill_distiller(palette = "Blues", direction = 1, name = "n obs.") +
    ggplot2::scale_colour_identity() +
    ggplot2::labs(
      title = "Yield data coverage",
      subtitle = "Number of observations per crop product and year",
      x = "Year", y = "Crop product"
    ) +
    theme_study2() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
