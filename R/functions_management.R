# Field management data: wraps DATA/FIELD_MANAGEMENT/read_foulumngaard_mngmnt.py
# (a pandas CLI, not ported to R) and provides the strip -> crop lookup logic
# used across the Study 2 branches (ported from SOIL_WATER.Rmd "Crop calendar
# from field management records").

#' Find a usable python interpreter on PATH.
#' Returns NA_character_ if none is found.
find_python <- function(candidates = c("python", "python3", "py")) {
  for (cand in candidates) {
    path <- Sys.which(cand)
    if (nzchar(path)) return(unname(path))
  }
  NA_character_
}

#' Run read_foulumngaard_mngmnt.py to (re)generate crop_calendar.csv and
#' management_actions.csv from the Handlinger_YYYY.xlsx files.
#'
#' If no python interpreter is found, or the script errors, falls back to
#' whatever CSVs are already on disk (with a warning) rather than failing the
#' whole pipeline -- but still hard-fails if neither is available, since a
#' Study 2 target with no crop calendar cannot legitimately run.
#'
#' @return character(2): c(actions_csv_path, calendar_csv_path)
run_management_import <- function(script_path,
                                   mngmt_dir = "DATA/FIELD_MANAGEMENT/COMPLETE_YEAR_MNGMT",
                                   out_dir   = "DATA/FIELD_MANAGEMENT",
                                   python = find_python()) {
  actions_csv  <- file.path(out_dir, "management_actions.csv")
  calendar_csv <- file.path(out_dir, "crop_calendar.csv")

  if (is.na(python)) {
    message("No python interpreter found on PATH -- using existing CSVs in ", out_dir,
            " without regenerating them.")
  } else {
    result <- suppressWarnings(system2(
      python,
      args = c(shQuote(script_path), "--dir", shQuote(mngmt_dir), "--out", shQuote(out_dir)),
      stdout = TRUE, stderr = TRUE
    ))
    exit_code <- attr(result, "status")
    if (!is.null(exit_code) && exit_code != 0L) {
      warning("read_foulumngaard_mngmnt.py exited with status ", exit_code,
              " -- falling back to existing CSVs in ", out_dir, ". Output:\n",
              paste(result, collapse = "\n"))
    }
  }

  if (!all(file.exists(c(actions_csv, calendar_csv)))) {
    stop("Management CSVs are missing and could not be (re)generated: ",
         actions_csv, " / ", calendar_csv,
         ". Install python + pandas, or place the CSVs manually.")
  }
  c(actions_csv, calendar_csv)
}

#' Load crop_calendar.csv with proper Date columns.
load_crop_calendar <- function(calendar_csv) {
  readr::read_csv(calendar_csv, show_col_types = FALSE) |>
    dplyr::mutate(
      start_date = as.Date(start_date),
      end_date   = as.Date(end_date)
    )
}

#' For each field strip, pick the crop occupying it for the largest share of
#' a given measurement window (handles strip transitions across seasons),
#' and attach the immediately preceding crop for context (e.g. autumn/winter
#' leaching often reflects mineralisation from the PRECEDING crop).
#'
#' Ported verbatim from SOIL_WATER.Rmd STEP 1 ("Crop calendar from field
#' management records"), including its hardcoded 2023 grass-clover fallback
#' for strips A3/A5/B1/B3 (management records only start in 2024, so lag()
#' cannot see their true 2023 predecessor -- the V4F rotation table confirms
#' it was also grass-clover for those four strips).
#'
#' @param crop_calendar output of load_crop_calendar()
#' @param meas_start,meas_end Date bounds of the measurement window
lookup_strip_crop <- function(crop_calendar, meas_start, meas_end) {
  strip_crop_lookup <- crop_calendar |>
    dplyr::mutate(
      overlap_start = pmax(start_date, meas_start),
      overlap_end   = pmin(end_date,   meas_end),
      overlap_days  = as.numeric(overlap_end - overlap_start + 1),
      overlap_days  = pmax(overlap_days, 0)
    ) |>
    dplyr::filter(overlap_days > 0) |>
    dplyr::arrange(strip, dplyr::desc(overlap_days)) |>
    dplyr::distinct(strip, .keep_all = TRUE) |>
    dplyr::select(strip, crop_name)

  prev_crop_tbl <- crop_calendar |>
    dplyr::arrange(strip, start_date) |>
    dplyr::group_by(strip) |>
    dplyr::mutate(prev_crop = dplyr::lag(crop_name)) |>
    dplyr::ungroup() |>
    dplyr::select(strip, crop_name, prev_crop)

  strip_crop_lookup <- strip_crop_lookup |>
    dplyr::left_join(prev_crop_tbl, by = c("strip", "crop_name"))

  # Strips whose first calendar entry is a 2023 grass-clover block that
  # predates the management records (which start in 2024) -- confirmed
  # against the V4F General Crop Rotation table (NEW 1-yr GC version).
  prev_crop_rotation_fallback <- tibble::tribble(
    ~strip, ~crop_name,      ~prev_crop_fb,
    "A3",   "Grass-Clover",  "Grass-Clover",
    "A5",   "Grass-Clover",  "Grass-Clover",
    "B1",   "Grass-Clover",  "Grass-Clover",
    "B3",   "Grass-Clover",  "Grass-Clover"
  )

  strip_crop_lookup |>
    dplyr::left_join(prev_crop_rotation_fallback, by = c("strip", "crop_name")) |>
    dplyr::mutate(
      prev_crop  = dplyr::coalesce(prev_crop, prev_crop_fb),
      crop_label = dplyr::if_else(
        !is.na(prev_crop),
        paste0(crop_name, "\n(prev: ", prev_crop, ")"),
        crop_name
      )
    ) |>
    dplyr::select(-prev_crop_fb)
}
