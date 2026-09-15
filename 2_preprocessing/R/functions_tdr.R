# TDR (Time-Domain Reflectometry) soil water content: raw .TRA import,
# cleaning, and treatment/location derivation. Ported from
# TDR_PREPROCESSING.Rmd (4-stage RAW -> PREPROCESSED -> CLEAN -> ANALYSIS-READY
# pipeline, documented in-file) and TDR_ANALYSIS.Rmd (treatment/location
# lookup, STEP 2).

#' Import one raw fixed-width .TRA logger file and extract the 9 columns of
#' interest, subsetting every `interval`-th row (the logger repeats a
#' multi-line record block per reading).
#'
#' Not called by `_targets.R` -- this pipeline bundles only the later,
#' manually-reviewed CLEAN_TDR_DATA stage (see 1_input_data/tdr_data/ and
#' combine_clean_tdr_data() below); raw .TRA logger files and the
#' PREPROCESSED_TDR_DATA intermediate are not part of this self-contained
#' repo. Kept here for provenance/traceability of how CLEAN_TDR_DATA was
#' originally derived.
#'
#' @return the preprocessed data frame (also written to output_dir as
#'   <basename>_preproc.csv, matching the source Rmd's side effect)
preprocess_TDR_data <- function(
    file_path,
    start_row = 1,
    interval = 19,
    col_names = c("Year", "Month", "Day", "Hour", "Minute", "Probe_No_Measurement_ID",
                  "Probe_Length_m", "TDR_Length_m", "Soil_Water_Content_pct"),
    output_dir = "4_outputs/data/tdr_preprocessed") {

  data <- read.table(file_path, header = FALSE, sep = "", fill = TRUE)

  total_rows <- nrow(data)
  rows_of_interest <- seq(from = start_row, to = total_rows, by = interval)
  filtered_data <- data[rows_of_interest, ]
  filtered_data_nine_cols <- filtered_data[, 1:9]
  colnames(filtered_data_nine_cols) <- col_names

  base_name <- basename(file_path)
  base_name_no_ext <- sub("\\.TRA$", "", base_name, ignore.case = TRUE)
  output_file_name <- paste0(base_name_no_ext, "_preproc.csv")
  output_file_path <- file.path(output_dir, output_file_name)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  write.csv(filtered_data_nine_cols, output_file_path, row.names = FALSE)

  filtered_data_nine_cols
}

#' Read and type-coerce one manually-reviewed "CLEAN_TDR_DATA" csv2 file.
#'
#' Ported from the STEP 4 (error-handled) version of read_and_process_csv() in
#' TDR_PREPROCESSING.Rmd -- the file defines this function twice; the STEP 3
#' version is syntactically broken (an orphan `TDR_Length_m = ...` line
#' outside any pipe) and was never the one actually used downstream. This is
#' the corrected, later version: it tolerates a missing TDR_Length_m column
#' and reports (rather than aborts on) per-file read errors.
read_and_process_csv <- function(file) {
  tryCatch({
    data <- readr::read_csv2(file, show_col_types = FALSE)

    if (!"TDR_Length_m" %in% colnames(data)) {
      data$TDR_Length_m <- NA
    }

    data |>
      dplyr::mutate(
        Probe_Length_m = as.numeric(Probe_Length_m),
        TDR_Length_m   = as.numeric(TDR_Length_m)
      ) |>
      dplyr::select(Crop, Sub_strip, Plot_no, Year, Month, Day,
                    Probe_No_Measurement_ID, Probe_Length_m, TDR_Length_m,
                    Soil_Water_Content_pct)
  }, error = function(e) {
    warning("Error processing TDR file ", file, ": ", e$message)
    NULL
  })
}

#' Combine all manually-reviewed CLEAN_TDR_DATA csv2 files into one
#' analysis-ready data frame.
combine_clean_tdr_data <- function(clean_dir = "1_input_data/tdr_data") {
  csv_files <- list.files(clean_dir, pattern = "\\.csv$", full.names = TRUE)
  purrr::map_df(csv_files, read_and_process_csv)
}

#' Sub_strip + Plot_no -> fertilisation TREATMENT lookup for the TDR nest
#' locations. Ported verbatim from TDR_ANALYSIS.Rmd STEP 2 (hardcoded field
#' record of which plots at each TDR nest received biogas digestate vs.
#' mineral fertiliser) -- this is real field-layout knowledge, not derivable
#' from the TDR data itself, so it is preserved exactly rather than
#' re-inferred. Values recased to "MINERAL_FERTILISER" (originally
#' "MIN_FERTILISER", the source Rmd's own abbreviation) to match the
#' FERTILISATION convention used by the yield/soil-water branches and
#' utils.R's fertilisation_colors key, so a TREATMENT-coloured TDR figure
#' uses the same palette as every other branch.
tdr_treatment_lookup <- function(sub_strip, plot_no) {
  dplyr::case_when(
    sub_strip == "N8"  & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N8"  & plot_no == 3 ~ "MINERAL_FERTILISER",
    sub_strip == "N10" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N10" & plot_no == 3 ~ "MINERAL_FERTILISER",
    sub_strip == "N11" & plot_no == 1 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N11" & plot_no == 2 ~ "MINERAL_FERTILISER",
    sub_strip == "N11" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N11" & plot_no == 4 ~ "MINERAL_FERTILISER",
    sub_strip == "N12" & plot_no == 2 ~ "MINERAL_FERTILISER",
    sub_strip == "N12" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N14" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N14" & plot_no == 3 ~ "MINERAL_FERTILISER",
    sub_strip == "N16" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N16" & plot_no == 3 ~ "MINERAL_FERTILISER",
    sub_strip == "N17" & plot_no == 1 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N17" & plot_no == 2 ~ "MINERAL_FERTILISER",
    sub_strip == "N17" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N17" & plot_no == 4 ~ "MINERAL_FERTILISER",
    sub_strip == "N18" & plot_no == 2 ~ "MINERAL_FERTILISER",
    sub_strip == "N18" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N20" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N20" & plot_no == 3 ~ "MINERAL_FERTILISER",
    sub_strip == "N22" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N22" & plot_no == 3 ~ "MINERAL_FERTILISER",
    sub_strip == "N23" & plot_no == 1 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N23" & plot_no == 2 ~ "MINERAL_FERTILISER",
    sub_strip == "N23" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N23" & plot_no == 4 ~ "MINERAL_FERTILISER",
    sub_strip == "N24" & plot_no == 2 ~ "MINERAL_FERTILISER",
    sub_strip == "N24" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    TRUE ~ NA_character_
  )
}

#' Derive LOCATION (BETWEEN_PV / OUTSIDE_PV), TREATMENT (fertilisation) and
#' RESIDUE for the combined TDR dataset, and rename to the pipeline's
#' standard column names.
#'
#' Note: RESIDUE is NOT available at the TDR-nest level in the source data --
#' TDR nests were positioned to sample fertilisation contrasts (digestate vs.
#' mineral), not the full fertilisation x residue factorial used in the yield
#' trial. Callers fitting a statistical model on this outcome should drop
#' RESIDUE from fixed_effects rather than have it silently imputed.
#'
#' Ported from TDR_ANALYSIS.Rmd STEP 2.
derive_tdr_covariates <- function(tdr_data) {
  tdr_data |>
    dplyr::mutate(
      PV_panel = dplyr::case_when(
        Sub_strip %in% c("N8", "N14", "N20") ~ "OUTSIDE_PV",
        TRUE ~ "BETWEEN_PV"
      ),
      Treatment = tdr_treatment_lookup(Sub_strip, Plot_no)
    ) |>
    dplyr::rename(
      CROP = Crop, SUB_STRIP = Sub_strip, LOCATION = PV_panel,
      PLOT_NO = Plot_no, TREATMENT = Treatment, YEAR = Year, MONTH = Month,
      DAY = Day, PROBE_ID = Probe_No_Measurement_ID,
      PROBE_LENGTH = Probe_Length_m, TDR_LENGTH = TDR_Length_m,
      SOIL_WATER_CONTENT = Soil_Water_Content_pct
    ) |>
    dplyr::relocate(CROP, SUB_STRIP, PLOT_NO, LOCATION, TREATMENT, YEAR, MONTH,
                    DAY, PROBE_ID, PROBE_LENGTH, TDR_LENGTH, SOIL_WATER_CONTENT)
}

#' Quick time-series preview of TDR soil water content by location, across
#' the combined dataset -- a lightweight EDA check, not a final analysis
#' figure. New, non-preliminary figure: reuses the established
#' location_simple_colors mapping (utils.R) and theme_study2() (plot_theme.R).
#'
#' @param tdr_analysis_ready output of derive_tdr_covariates()
plot_tdr_time_series <- function(tdr_analysis_ready) {
  ts_df <- tdr_analysis_ready |>
    dplyr::mutate(date = as.Date(paste(YEAR, MONTH, DAY, sep = "-"))) |>
    dplyr::group_by(date, LOCATION) |>
    dplyr::summarise(
      mean_swc = mean(SOIL_WATER_CONTENT, na.rm = TRUE),
      se_swc   = sd(SOIL_WATER_CONTENT, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups  = "drop"
    )

  ggplot2::ggplot(ts_df, ggplot2::aes(x = date, y = mean_swc, colour = LOCATION, fill = LOCATION)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_swc - se_swc, ymax = mean_swc + se_swc),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_colour_manual(values = location_simple_colors, name = "Location") +
    ggplot2::scale_fill_manual(values = location_simple_colors, name = "Location") +
    ggplot2::scale_x_date(labels = format_month_year_en) +
    ggplot2::labs(
      title = "TDR soil water content over time",
      subtitle = "Mean +/- SE per sampling date and location",
      x = NULL, y = "Soil water content (%)"
    ) +
    theme_study2()
}
