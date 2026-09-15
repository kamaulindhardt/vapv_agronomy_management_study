# TDR (Time-Domain Reflectometry) soil water content: raw .TRA import,
# cleaning, and treatment/location derivation. Ported from
# TDR_PREPROCESSING.Rmd (4-stage RAW -> PREPROCESSED -> CLEAN -> ANALYSIS-READY
# pipeline, documented in-file) and TDR_ANALYSIS.Rmd (treatment/location
# lookup, STEP 2).

#' Import one raw fixed-width .TRA logger file and extract the 9 columns of
#' interest, subsetting every `interval`-th row (the logger repeats a
#' multi-line record block per reading).
#'
#' @return the preprocessed data frame (also written to output_dir as
#'   <basename>_preproc.csv, matching the source Rmd's side effect)
preprocess_TDR_data <- function(
    file_path,
    start_row = 1,
    interval = 19,
    col_names = c("Year", "Month", "Day", "Hour", "Minute", "Probe_No_Measurement_ID",
                  "Probe_Length_m", "TDR_Length_m", "Soil_Water_Content_pct"),
    output_dir = "DATA/TDR_DATA/PREPROCESSED_TDR_DATA") {

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
combine_clean_tdr_data <- function(clean_dir = "DATA/TDR_DATA/CLEAN_TDR_DATA") {
  csv_files <- list.files(clean_dir, pattern = "\\.csv$", full.names = TRUE)
  purrr::map_df(csv_files, read_and_process_csv)
}

#' Sub_strip + Plot_no -> fertilisation TREATMENT lookup for the TDR nest
#' locations. Ported verbatim from TDR_ANALYSIS.Rmd STEP 2 (hardcoded field
#' record of which plots at each TDR nest received biogas digestate vs.
#' mineral fertiliser) -- this is real field-layout knowledge, not derivable
#' from the TDR data itself, so it is preserved exactly rather than
#' re-inferred.
tdr_treatment_lookup <- function(sub_strip, plot_no) {
  dplyr::case_when(
    sub_strip == "N8"  & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N8"  & plot_no == 3 ~ "MIN_FERTILISER",
    sub_strip == "N10" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N10" & plot_no == 3 ~ "MIN_FERTILISER",
    sub_strip == "N11" & plot_no == 1 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N11" & plot_no == 2 ~ "MIN_FERTILISER",
    sub_strip == "N11" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N11" & plot_no == 4 ~ "MIN_FERTILISER",
    sub_strip == "N12" & plot_no == 2 ~ "MIN_FERTILISER",
    sub_strip == "N12" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N14" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N14" & plot_no == 3 ~ "MIN_FERTILISER",
    sub_strip == "N16" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N16" & plot_no == 3 ~ "MIN_FERTILISER",
    sub_strip == "N17" & plot_no == 1 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N17" & plot_no == 2 ~ "MIN_FERTILISER",
    sub_strip == "N17" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N17" & plot_no == 4 ~ "MIN_FERTILISER",
    sub_strip == "N18" & plot_no == 2 ~ "MIN_FERTILISER",
    sub_strip == "N18" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N20" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N20" & plot_no == 3 ~ "MIN_FERTILISER",
    sub_strip == "N22" & plot_no == 2 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N22" & plot_no == 3 ~ "MIN_FERTILISER",
    sub_strip == "N23" & plot_no == 1 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N23" & plot_no == 2 ~ "MIN_FERTILISER",
    sub_strip == "N23" & plot_no == 3 ~ "BIOGAS_DIGESTATE",
    sub_strip == "N23" & plot_no == 4 ~ "MIN_FERTILISER",
    sub_strip == "N24" & plot_no == 2 ~ "MIN_FERTILISER",
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
