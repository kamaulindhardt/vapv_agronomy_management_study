# Field management data: wraps 1_input_data/field_management/read_foulumngaard_mngmnt.py
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
                                   mngmt_dir = "1_input_data/field_management",
                                   out_dir   = "4_outputs/data/field_management",
                                   python = find_python()) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
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

#' Gantt-style crop rotation timeline: one horizontal segment per
#' (strip, crop, season) block. The EDA figure the original SOIL_WATER.Rmd
#' had (crop rotation timeline, restricted to strips with suction cups) but
#' this pipeline never ported until now. Uses utils.R's crop_colors (via
#' shorten_crop(), since crop_calendar's crop_name is the full name, e.g.
#' "Winter Wheat") and plot_theme.R's theme_study2() -- this is a new,
#' non-preliminary figure, so it follows the validated design system.
#'
#' @param crop_calendar output of load_crop_calendar()
#' @param strips optional character vector to restrict which strips are shown
#'   (default: all strips present)
plot_crop_rotation_timeline <- function(crop_calendar, strips = NULL) {
  cal <- crop_calendar |>
    dplyr::mutate(
      start_date = as.Date(start_date),
      end_date   = as.Date(end_date),
      crop_code  = shorten_crop(crop_name)
    )
  if (!is.null(strips)) cal <- dplyr::filter(cal, strip %in% strips)

  ggplot2::ggplot(cal, ggplot2::aes(x = start_date, xend = end_date,
                                     y = strip, yend = strip, colour = crop_code)) +
    ggplot2::geom_segment(lineend = "round", linewidth = 5) +
    ggplot2::scale_colour_manual(values = crop_colors, name = "Crop", na.value = "grey70") +
    ggplot2::scale_x_date(date_breaks = "6 months", labels = format_month_year_en) +
    ggplot2::labs(
      title = "Crop rotation timeline",
      subtitle = "One segment per crop occupying a field strip, derived from management records",
      x = NULL, y = "Field strip"
    ) +
    theme_study2() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
}
