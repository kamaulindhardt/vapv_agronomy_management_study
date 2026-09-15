# Soil-water suction cup data: import, crop/treatment join, and long-format
# reshape with below-detection-limit handling. Ported from SOIL_WATER.Rmd
# STEP 1 ("READING SOIL WATER EXCEL DATA") and STEP 2 (reshape to long).
#
# The Drainage-Linear (DL) NO3 leaching computation that consumes this
# cleaned/reshaped data lives in R/functions_no3_leaching.R.

#' Lab detection limit for NO3-N (mg/L). "UD" (Under detektionsgraensen)
#' entries are substituted with DL/2 per standard practice; "NA" entries
#' (not reported by the lab at all) remain NA and are excluded.
#' Source: AgroLab analysis certificate -- update if a new batch reports a
#' different DL.
DL_NO3_MG_L <- 0.5

#' Read the VALUE4FARM soil water (suction cup) workbook and convert its
#' Excel-serial-number date columns (e.g. "x45548") to "dd-mm-yyyy" labels.
load_soil_water_xlsx <- function(path = "DATA/SOIL_WATER/VALUE4FARM_SOIL_WATER_DATA.xlsx",
                                  sheet = "ALL_TAPPPING_COMBINED") {
  soil_water <- readxl::read_excel(path, sheet = sheet) |> janitor::clean_names()

  date_cols <- names(soil_water)[
    suppressWarnings(!is.na(as.numeric(gsub("^x", "", names(soil_water)))))
  ]
  date_labels <- as.Date(as.numeric(gsub("^x", "", date_cols)), origin = "1899-12-30")
  formatted_labels <- format(date_labels, "%d-%m-%Y")
  names(soil_water)[match(date_cols, names(soil_water))] <- formatted_labels

  soil_water
}

#' Join the suction-cup workbook to the strip -> crop lookup (see
#' R/functions_management.R) and derive the fertilisation treatment.
#'
#' @param soil_water output of load_soil_water_xlsx()
#' @param strip_crop_lookup output of lookup_strip_crop()
clean_soil_water <- function(soil_water, strip_crop_lookup) {
  soil_water |>
    dplyr::mutate(
      fertilisation = dplyr::case_when(
        treatment %in% c("A", "C") ~ "Biogas Digestate",
        treatment %in% c("B", "D") ~ "Mineral Fertiliser",
        TRUE ~ NA_character_
      ),
      # RESIDUE, extending the same TREATMENT-code convention used for the
      # yield data (see preprocess_yield_data() STEP 3): A/B = removed,
      # C/D = residue retained. Not derived in the original SOIL_WATER.Rmd,
      # added here so the NO3-leaching branch can use the same generic
      # statistical model factory as the yield/quality branches.
      residue = dplyr::case_when(
        treatment %in% c("A", "B") ~ "REMOVED",
        treatment %in% c("C", "D") ~ "REMAIN",
        TRUE ~ NA_character_
      ),
      base_strip = stringr::str_remove(main_strip, "[ab]$")
    ) |>
    dplyr::left_join(strip_crop_lookup, by = c("base_strip" = "strip")) |>
    dplyr::rename(crop = crop_name) |>
    dplyr::select(-base_strip) |>
    dplyr::relocate(
      project, main_strip, sub_strip, plot_no, suction_cup, cabinet, treatment,
      location, sub_strip_location, fertilisation, residue, crop
    )
}

#' Detect the [start, end] measurement window from the soil-water workbook's
#' "dd-mm-yyyy"-labelled date columns (post load_soil_water_xlsx()).
soil_water_measurement_window <- function(soil_water) {
  meas_dates <- as.Date(
    names(soil_water)[stringr::str_detect(names(soil_water), "^\\d{2}-\\d{2}-\\d{4}$")],
    format = "%d-%m-%Y"
  )
  c(start = min(meas_dates, na.rm = TRUE), end = max(meas_dates, na.rm = TRUE))
}

#' Reshape the wide (one column per sampling date) suction-cup workbook to
#' long format, applying the UD -> DL/2 and NA -> NA_real_ substitution.
reshape_soil_water_long <- function(soil_water_clean, dl_no3_mg_l = DL_NO3_MG_L) {
  no3_date_cols <- names(soil_water_clean) |>
    stringr::str_subset("^\\d{2}-\\d{2}-\\d{4}$")

  soil_water_clean |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(no3_date_cols),
      names_to = "date",
      values_to = "raw_value"
    ) |>
    dplyr::mutate(
      date     = lubridate::dmy(date),
      below_dl = toupper(as.character(raw_value)) == "UD",
      no3_mg_l = dplyr::case_when(
        toupper(as.character(raw_value)) == "UD" ~ dl_no3_mg_l / 2,
        toupper(as.character(raw_value)) == "NA" ~ NA_real_,
        TRUE ~ suppressWarnings(as.numeric(raw_value))
      )
    ) |>
    dplyr::select(-raw_value) |>
    dplyr::arrange(project, suction_cup, date)
}
