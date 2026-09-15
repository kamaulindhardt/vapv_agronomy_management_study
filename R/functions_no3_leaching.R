# NO3-N leaching via the Drainage-Linear (DL) method (Vogeler et al. 2020):
# N_i [kg N/ha] = C_i [mg NO3-N/L] x D_i [mm] x 0.01, combining biweekly
# suction-cup concentrations (interpolated to daily) with Daisy-simulated
# daily drainage at 1 m depth. Ported from SOIL_WATER.Rmd STEP 4
# ("DL-INTERPOLATION") and its model-evaluation metrics (STEP 6.9).
#
# Reference: Vogeler, I., Thomas, S., van der Linden, A., & Bolan, N. (2020).
# Nitrate leaching from suction cup data: Influence of method of drainage
# calculation and concentration interpolation. J. Environ. Qual., 49(2), 338-347.
#
# Key assumption carried over from the source analysis: Daisy provides ONE
# drainage time series per rotation strip group (A2 = WW-SY-WW, A3 = GC-GC-SB).
# BETWEEN_PV vs OUTSIDE_PV differences in DL leaching therefore reflect
# differences in NO3 concentration only, not spatially differentiated
# drainage -- see the Study 2 concept paper's discussion of this limitation.

# ---- Loading Daisy drainage output -------------------------------------------

#' Read one Daisy drainage .rds export and tag it with a rotation-strip
#' column label (e.g. "A2", "A3").
load_drain_col <- function(path, col_label) {
  readRDS(path) |>
    dplyr::mutate(
      date        = as.Date(datetime),
      drainage_mm = pmax(drainage_mm, 0),   # upward capillary rise -> 0
      daisy_col   = col_label
    ) |>
    dplyr::select(daisy_col, date, drainage_mm)
}

#' Load per-rotation-strip Daisy drainage, preferring the two-column
#' (A2 + A3) export and falling back to a single backward-compatible file.
load_daisy_drainage <- function(
    output_dir = "FIELD_WATER_BALANCE_DAISY/OUTPUT") {
  drain_A2_path <- file.path(output_dir, "V4F_drainage_A2_1m_daily.rds")
  drain_A3_path <- file.path(output_dir, "V4F_drainage_A3_1m_daily.rds")
  drain_bc_path <- file.path(output_dir, "V4F_drainage_1m_daily.rds")

  if (file.exists(drain_A2_path) && file.exists(drain_A3_path)) {
    dplyr::bind_rows(
      load_drain_col(drain_A2_path, "A2"),
      load_drain_col(drain_A3_path, "A3")
    )
  } else if (file.exists(drain_bc_path)) {
    warning("Per-column drainage files not found -- using backward-compat A2 file for all strips.")
    load_drain_col(drain_bc_path, "A2")
  } else {
    stop("No Daisy drainage file found under ", output_dir,
         ". Run FIELD_WATER_BALANCE_DAISY.Rmd first.")
  }
}

#' Field strip -> Daisy rotation-column mapping.
#' A2 rotation (WW-SY-WW): strips A1, A2, A4, B2, B4, B5.
#' A3 rotation (GC-GC-SB): strips A3, A5, B1, B3.
daisy_strip_col_map <- function() {
  tibble::tribble(
    ~strip, ~daisy_col,
    "A1",   "A2",
    "A2",   "A2",
    "A3",   "A3",
    "A4",   "A2",
    "A5",   "A3",
    "B1",   "A3",
    "B2",   "A2",
    "B3",   "A3",
    "B4",   "A2",
    "B5",   "A2"
  )
}

# ---- Preparing suction-cup concentration data ---------------------------------

#' Filter soil_water_long to valid NO3 observations, attach the Daisy
#' rotation-column each strip belongs to, and keep only the columns needed
#' downstream.
#'
#' @param soil_water_long output of reshape_soil_water_long()
#' @param daisy_drain_cols unique daisy_col values actually available (from
#'   load_daisy_drainage()) -- when only one column is available, all strips
#'   collapse onto it so no strip gets NA drainage.
prepare_sc_valid <- function(soil_water_long, daisy_drain_cols) {
  daisy_col_map_eff <- if (length(daisy_drain_cols) == 1) {
    daisy_strip_col_map() |> dplyr::mutate(daisy_col = daisy_drain_cols[1])
  } else {
    daisy_strip_col_map()
  }

  soil_water_long |>
    dplyr::filter(!is.na(no3_mg_l)) |>
    dplyr::mutate(strip = stringr::str_remove(main_strip, "[ab]$")) |>
    dplyr::left_join(daisy_col_map_eff, by = "strip") |>
    dplyr::mutate(daisy_col = dplyr::coalesce(daisy_col, daisy_drain_cols[1])) |>
    dplyr::select(suction_cup, strip, plot_no, fertilisation, residue, location,
                  crop, daisy_col, date, no3_mg_l, below_dl) |>
    dplyr::arrange(suction_cup, date)
}

# ---- Daily interpolation and leaching calculation -----------------------------

#' Interpolate one suction cup's biweekly concentration series to a daily
#' series using three methods (Vogeler et al. 2020 nomenclature):
#'   LI = linear interpolation (recommended by Vogeler et al.)
#'   FC = forward constant / left step
#'   BC = backward constant / right step
#' Returns NULL for cups with fewer than 2 valid observations.
interp_daily_no3 <- function(cup_df) {
  if (nrow(cup_df) < 2L) return(NULL)
  dates <- cup_df$date
  conc  <- cup_df$no3_mg_l
  xnum  <- as.numeric(dates)
  daily <- seq(min(dates), max(dates), by = "day")
  xd    <- as.numeric(daily)

  tibble::tibble(
    date = daily,
    C_LI = approx(xnum, conc, xout = xd, method = "linear",   rule = 1L)$y,
    C_FC = approx(xnum, conc, xout = xd, method = "constant", f = 0, rule = 1L)$y,
    C_BC = approx(xnum, conc, xout = xd, method = "constant", f = 1, rule = 1L)$y
  )
}

#' Build the daily concentration table per suction cup (cups with >= 2 valid
#' observations only).
build_dl_conc_daily <- function(sc_valid) {
  cups_enough <- sc_valid |>
    dplyr::count(suction_cup) |>
    dplyr::filter(n >= 2) |>
    dplyr::pull(suction_cup)

  sc_valid |>
    dplyr::filter(suction_cup %in% cups_enough) |>
    dplyr::group_by(suction_cup, strip, plot_no, fertilisation, residue,
                    location, crop, daisy_col) |>
    dplyr::group_modify(~ interp_daily_no3(dplyr::select(.x, date, no3_mg_l))) |>
    dplyr::ungroup()
}

#' Join strip-specific Daisy drainage onto the daily concentration table and
#' compute daily N leaching: N [kg N/ha/d] = C [mg/L] x D [mm/d] x 0.01.
join_dl_drainage <- function(dl_conc_daily, daisy_drain_all) {
  dl_conc_daily |>
    dplyr::left_join(daisy_drain_all, by = c("daisy_col", "date")) |>
    dplyr::mutate(
      drainage_mm = tidyr::replace_na(drainage_mm, 0),
      N_LI = pmax(C_LI, 0) * drainage_mm * 0.01,
      N_FC = pmax(C_FC, 0) * drainage_mm * 0.01,
      N_BC = pmax(C_BC, 0) * drainage_mm * 0.01
    )
}

#' Aggregate daily leaching into sampling-period totals (each day assigned to
#' the sampling event that precedes it).
aggregate_dl_by_period <- function(dl_leaching_daily, sampling_dates) {
  breaks_all <- c(sampling_dates, max(dl_leaching_daily$date) + 1L)

  dl_leaching_daily |>
    dplyr::mutate(
      period_start = as.Date(as.character(
        cut(date, breaks = breaks_all, labels = as.character(sampling_dates),
            right = FALSE, include.lowest = TRUE)
      ))
    ) |>
    dplyr::filter(!is.na(period_start)) |>
    dplyr::group_by(suction_cup, strip, plot_no, fertilisation, residue,
                    location, crop, daisy_col, period_start) |>
    dplyr::summarise(
      drainage_mm = sum(drainage_mm, na.rm = TRUE),
      N_LI_kgNha  = sum(N_LI, na.rm = TRUE),
      N_FC_kgNha  = sum(N_FC, na.rm = TRUE),
      N_BC_kgNha  = sum(N_BC, na.rm = TRUE),
      n_days      = dplyr::n(),
      .groups     = "drop"
    )
}

#' Total (cumulative over the whole monitoring period) N leaching per cup --
#' this is the pipeline's Study 2 "analysis-ready" NO3-leaching dataset,
#' i.e. one row per suction cup with a fixed set of treatment covariates and
#' a cumulative leaching outcome, matched to the crop yield/quality datasets'
#' shape so the same generic statistical model factory
#' (R/functions_statistics.R) can be applied.
summarise_dl_total <- function(dl_leaching_daily) {
  dl_leaching_daily |>
    dplyr::group_by(suction_cup, strip, plot_no, fertilisation, residue,
                    location, crop, daisy_col) |>
    dplyr::summarise(
      period_days = dplyr::n(),
      drainage_mm = sum(drainage_mm, na.rm = TRUE),
      N_LI_kgNha  = sum(N_LI, na.rm = TRUE),
      N_FC_kgNha  = sum(N_FC, na.rm = TRUE),
      N_BC_kgNha  = sum(N_BC, na.rm = TRUE),
      .groups     = "drop"
    )
}

#' End-to-end DL leaching pipeline: soil_water_long + Daisy drainage ->
#' per-cup cumulative leaching totals. This is the function `_targets.R`
#' calls to build the NO3-leaching analysis-ready dataset.
build_no3_leaching_dataset <- function(soil_water_long, daisy_output_dir = "FIELD_WATER_BALANCE_DAISY/OUTPUT") {
  daisy_drain_all  <- load_daisy_drainage(daisy_output_dir)
  daisy_drain_cols <- sort(unique(daisy_drain_all$daisy_col))

  sc_valid        <- prepare_sc_valid(soil_water_long, daisy_drain_cols)
  dl_conc_daily   <- build_dl_conc_daily(sc_valid)
  dl_leaching_daily <- join_dl_drainage(dl_conc_daily, daisy_drain_all)

  list(
    daily   = dl_leaching_daily,
    by_period = aggregate_dl_by_period(dl_leaching_daily, sort(unique(sc_valid$date))),
    total   = summarise_dl_total(dl_leaching_daily)
  )
}

#' Load Daisy's own simulated NO3 leaching output per rotation strip (the
#' model's internal N-leaching estimate, distinct from the DL field-derived
#' estimate computed above) for cross-validation.
load_daisy_no3_leaching <- function(output_dir = "FIELD_WATER_BALANCE_DAISY/OUTPUT") {
  cols <- c("A2", "A3")
  out <- list()
  for (dc in cols) {
    p <- file.path(output_dir, paste0("V4F_NO3_leaching_", dc, "_1m_daily.rds"))
    if (file.exists(p)) {
      out[[dc]] <- readRDS(p) |>
        dplyr::mutate(date = as.Date(datetime), daisy_col = dc) |>
        dplyr::select(daisy_col, date, N_daisy = N_leach_kgN_ha)
    }
  }
  dplyr::bind_rows(out)
}

#' Cross-validate the field-derived DL leaching estimate against Daisy's own
#' simulated NO3 leaching, per rotation strip. Ported from SOIL_WATER.Rmd
#' STEP 4.10 -- this produced the draft report's "R2 = 0.50 (winter-wheat
#' rotation), R2 = 0.17 (grass-clover)" cross-validation figures.
#'
#' @param dl_leaching_daily the `$daily` element of build_no3_leaching_dataset()
#' @param daisy_no3 output of load_daisy_no3_leaching()
validate_dl_vs_daisy <- function(dl_leaching_daily, daisy_no3) {
  dl_daily_mean_by_col <- dl_leaching_daily |>
    dplyr::group_by(daisy_col, date) |>
    dplyr::summarise(N_LI_field = mean(N_LI, na.rm = TRUE), .groups = "drop")

  compare_df <- daisy_no3 |>
    dplyr::inner_join(dl_daily_mean_by_col, by = c("daisy_col", "date"))

  # sim = Daisy (modelled), obs = field DL (measured) -- matches the
  # sim/obs convention used for the Daisy-vs-TDR soil-water metrics.
  compare_df |>
    dplyr::group_by(daisy_col) |>
    dplyr::group_modify(~ compute_metrics(.x$N_daisy, .x$N_LI_field, label = .y$daisy_col)) |>
    dplyr::ungroup()
}

# ---- Model-performance metrics (Daisy vs. TDR/field validation) --------------
# Ported verbatim from SOIL_WATER.Rmd STEP 6.9.

rmse  <- function(sim, obs) sqrt(mean((sim - obs)^2, na.rm = TRUE))
mae   <- function(sim, obs) mean(abs(sim - obs), na.rm = TRUE)
mbe   <- function(sim, obs) mean(sim - obs, na.rm = TRUE)
pbias <- function(sim, obs) 100 * sum(sim - obs, na.rm = TRUE) / sum(obs, na.rm = TRUE)

nse <- function(sim, obs) {
  obs_mean <- mean(obs, na.rm = TRUE)
  1 - sum((sim - obs)^2, na.rm = TRUE) / sum((obs - obs_mean)^2, na.rm = TRUE)
}

willmott_d <- function(sim, obs) {
  obs_mean <- mean(obs, na.rm = TRUE)
  num <- sum((sim - obs)^2, na.rm = TRUE)
  den <- sum((abs(sim - obs_mean) + abs(obs - obs_mean))^2, na.rm = TRUE)
  1 - num / den
}

r_squared <- function(sim, obs) {
  cc <- cor(sim, obs, use = "complete.obs")
  cc^2
}

kge <- function(sim, obs) {
  r     <- cor(sim, obs, use = "complete.obs")
  beta  <- mean(sim, na.rm = TRUE) / mean(obs, na.rm = TRUE)
  gamma <- (sd(sim, na.rm = TRUE) / mean(sim, na.rm = TRUE)) /
           (sd(obs, na.rm = TRUE) / mean(obs, na.rm = TRUE))
  1 - sqrt((r - 1)^2 + (beta - 1)^2 + (gamma - 1)^2)
}

#' Compute the full RMSE/MAE/MBE/PBIAS/NSE/d/r2/KGE suite for one
#' simulated-vs-observed pair, returned as a one-row tibble.
compute_metrics <- function(sim, obs, label = NULL) {
  tibble::tibble(
    label = label,
    n     = sum(!is.na(sim) & !is.na(obs)),
    RMSE  = rmse(sim, obs),
    MAE   = mae(sim, obs),
    MBE   = mbe(sim, obs),
    PBIAS = pbias(sim, obs),
    NSE   = nse(sim, obs),
    d     = willmott_d(sim, obs),
    r2    = r_squared(sim, obs),
    KGE   = kge(sim, obs)
  )
}
