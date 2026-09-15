# Winter wheat disease severity index (DSI). Ported from
# WW_DISEASE_SEVERITY_INDEX_2025.Rmd (septoria scoring register, 2025 season).

#' Read the four septoria scoring sheets (A2a/A2b/B4a/B4b -- one per
#' winter-wheat main-strip half) and compute + combine their DSI tables.
load_disease_severity_data <- function(
    path = "DATA/CROP_YIELD_QUALITY_DATA/2025/WW_sortskimmel_reg_30_07_2025.xlsx") {
  read_sheet <- function(sheet) {
    readxl::read_excel(path, sheet = sheet, skip = 3) |> janitor::clean_names()
  }

  dplyr::bind_rows(
    calculate_dsi(read_sheet("A2a_data")) |> dplyr::mutate(mainstrip = "A2a"),
    calculate_dsi(read_sheet("A2b_data")) |> dplyr::mutate(mainstrip = "A2b"),
    calculate_dsi(read_sheet("B4a_data")) |> dplyr::mutate(mainstrip = "B4a"),
    calculate_dsi(read_sheet("B4b_data")) |> dplyr::mutate(mainstrip = "B4b")
  )
}

#' Calculate Disease Severity Index (DSI, %) per plot from a septoria
#' severity-percent scoring register.
#'
#' Input `data` must have columns: substrip, angrebet_0_1 (0/1 infection
#' flag), severity_percent (one of the six banded categories below), and
#' optionally plot.
#'
#' @return tibble with one row per substrip_plot: n_total_plants,
#'   n_infected_plants, sum_midpoints, DSI_percent
calculate_dsi <- function(data) {
  severity_midpoints <- c(
    "1-3"     = 2,
    "4-10"    = 7,
    "11-25"   = 18,
    "26-50"   = 38,
    "51-75"   = 63,
    "76-100"  = 88
  )

  has_plot <- "plot" %in% names(data)

  data <- data |>
    dplyr::mutate(
      substrip_plot = if (has_plot) {
        paste0(substrip, "_", plot)
      } else {
        substrip
      },
      severity_midpoint = severity_midpoints[severity_percent],
      is_infected = angrebet_0_1 == 1
    )

  data |>
    dplyr::group_by(substrip_plot) |>
    dplyr::summarise(
      n_total_plants    = dplyr::n(),
      n_infected_plants = sum(is_infected, na.rm = TRUE),
      sum_midpoints     = sum(severity_midpoint[is_infected], na.rm = TRUE),
      DSI_percent       = sum_midpoints / n_total_plants,
      .groups = "drop"
    )
}
