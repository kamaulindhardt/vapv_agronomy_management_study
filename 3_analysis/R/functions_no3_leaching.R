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
    output_dir = "1_input_data/daisy_water_balance") {
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
build_no3_leaching_dataset <- function(soil_water_long, daisy_output_dir = "1_input_data/daisy_water_balance") {
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
load_daisy_no3_leaching <- function(output_dir = "1_input_data/daisy_water_balance") {
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

#' Join field DL-LI daily leaching (averaged across cups, per rotation strip)
#' with Daisy's own simulated daily NO3 leaching, by rotation strip and date.
#' Shared by validate_dl_vs_daisy() (metrics) and plot_no3_dl_vs_daisy()
#' (figure) so the two never drift apart.
#'
#' @param dl_leaching_daily the `$daily` element of build_no3_leaching_dataset()
#' @param daisy_no3 output of load_daisy_no3_leaching()
build_dl_vs_daisy_paired <- function(dl_leaching_daily, daisy_no3) {
  dl_daily_mean_by_col <- dl_leaching_daily |>
    dplyr::group_by(daisy_col, date) |>
    dplyr::summarise(N_LI_field = mean(N_LI, na.rm = TRUE), .groups = "drop")

  daisy_no3 |>
    dplyr::inner_join(dl_daily_mean_by_col, by = c("daisy_col", "date"))
}

#' Cross-validate the field-derived DL leaching estimate against Daisy's own
#' simulated NO3 leaching, per rotation strip. Ported from SOIL_WATER.Rmd
#' STEP 4.10 -- this produced the draft report's "R2 = 0.50 (winter-wheat
#' rotation), R2 = 0.17 (grass-clover)" cross-validation figures.
#'
#' @param dl_leaching_daily the `$daily` element of build_no3_leaching_dataset()
#' @param daisy_no3 output of load_daisy_no3_leaching()
validate_dl_vs_daisy <- function(dl_leaching_daily, daisy_no3) {
  compare_df <- build_dl_vs_daisy_paired(dl_leaching_daily, daisy_no3)

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

# ---- Preliminary figures ------------------------------------------------------
# Descriptive companions to the ANOVA/emmeans results in 3_analysis/R/functions_statistics.R
# (no plotting function existed for this outcome before). Ported/extended from
# SOIL_WATER.Rmd STEPs 4.6/4.7/4.9/5.1a/5.3a; RESIDUE faceting is new -- the
# source notebook predates RESIDUE being derived for this branch (see
# clean_soil_water() in functions_soil_water.R). Use utils.R's palettes
# (fertilisation_colors, residue_colors, location_simple_colors, crop_colors +
# shorten_crop()) so these share a visual language with the yield figures.

#' Boxplot comparing the three DL concentration-interpolation methods
#' (LI/FC/BC, Vogeler et al. 2020) side by side, coloured by fertilisation and
#' faceted by location -- a sensitivity check on the interpolation choice.
#' Ported from SOIL_WATER.Rmd STEP 4.6.
#'
#' @param dl_total the `$total` element of build_no3_leaching_dataset()
#'   (== no3_leaching_analysis_ready)
plot_no3_method_comparison <- function(dl_total) {
  method_labels <- c(
    N_LI_kgNha = "Linear (LI)",
    N_FC_kgNha = "Forward-constant (FC)",
    N_BC_kgNha = "Backward-constant (BC)"
  )

  dl_long <- dl_total |>
    tidyr::pivot_longer(
      cols = c(N_LI_kgNha, N_FC_kgNha, N_BC_kgNha),
      names_to = "method", values_to = "N_leach_kgNha"
    ) |>
    dplyr::mutate(method = factor(method_labels[method], levels = method_labels))

  ggplot2::ggplot(dl_long, ggplot2::aes(x = method, y = N_leach_kgNha, fill = fertilisation)) +
    ggplot2::geom_boxplot(position = ggplot2::position_dodge(width = 0.8), width = 0.7,
                          alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_point(ggplot2::aes(colour = fertilisation),
                        position = ggplot2::position_jitterdodge(jitter.width = 0.08, dodge.width = 0.8),
                        size = 1.8, alpha = 0.7) +
    ggplot2::facet_wrap(~ location) +
    ggplot2::scale_fill_manual(values = fertilisation_colors, name = "Fertilisation") +
    ggplot2::scale_colour_manual(values = fertilisation_colors, guide = "none") +
    ggplot2::labs(
      title = "Sensitivity of NO3-N leaching to concentration-interpolation method",
      subtitle = "Drainage-Linear (DL) method, Vogeler et al. (2020)",
      x = "Interpolation method", y = expression("Total NO"[3]*"-N leaching (kg N ha"^{-1}*")")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                  axis.text.x = ggplot2::element_text(angle = 20, hjust = 1),
                  legend.position = "top")
}

#' Box + jitter of cumulative NO3-N leaching (DL-LI method, the
#' Vogeler-recommended interpolation) by crop, fertilisation, crop-residue
#' management and panel location -- the core Study 2 leaching result.
#' Ported/extended from SOIL_WATER.Rmd STEPs 4.9/5.1a.
#'
#' @param dl_total the `$total` element of build_no3_leaching_dataset()
plot_no3_total_by_treatment <- function(dl_total) {
  plot_df <- dl_total |> dplyr::mutate(crop = shorten_crop(crop))

  ggplot2::ggplot(plot_df, ggplot2::aes(x = crop, y = N_LI_kgNha, fill = fertilisation)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7,
                          position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = fertilisation),
                        position = ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
                        size = 1.8, alpha = 0.75) +
    ggplot2::facet_grid(residue ~ location,
                        labeller = ggplot2::labeller(
                          residue = c(REMOVED = "Residue removed", REMAIN = "Residue retained")
                        )) +
    ggplot2::scale_fill_manual(values = fertilisation_colors, name = "Fertilisation") +
    ggplot2::scale_colour_manual(values = fertilisation_colors, guide = "none") +
    ggplot2::labs(
      title = "Cumulative NO3-N leaching by crop, fertilisation, residue and PV location",
      subtitle = "DL method, linear interpolation (LI) | suction cups, 1 m depth",
      x = "Crop", y = expression("Total NO"[3]*"-N leaching (kg N ha"^{-1}*")")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"), legend.position = "top")
}

#' Cumulative NO3-N leaching over the monitoring period (DL-LI method), mean
#' +/- SE by fertilisation and location. Ported from SOIL_WATER.Rmd STEP 4.7.
#'
#' @param dl_leaching_daily the `$daily` element of build_no3_leaching_dataset()
plot_no3_cumulative_leaching <- function(dl_leaching_daily) {
  cum_df <- dl_leaching_daily |>
    dplyr::arrange(suction_cup, date) |>
    dplyr::group_by(suction_cup, fertilisation, location) |>
    dplyr::mutate(cum_N_LI = cumsum(tidyr::replace_na(N_LI, 0))) |>
    dplyr::ungroup() |>
    dplyr::group_by(date, fertilisation, location) |>
    dplyr::summarise(
      mean_cum = mean(cum_N_LI, na.rm = TRUE),
      se_cum   = sd(cum_N_LI, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups  = "drop"
    )

  ggplot2::ggplot(cum_df, ggplot2::aes(x = date, y = mean_cum, colour = fertilisation,
                                       fill = fertilisation, linetype = location)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_cum - se_cum, ymax = mean_cum + se_cum),
                         alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_manual(values = fertilisation_colors, name = "Fertilisation") +
    ggplot2::scale_fill_manual(values = fertilisation_colors, name = "Fertilisation") +
    ggplot2::scale_linetype_manual(values = c(BETWEEN_PV = "solid", OUTSIDE_PV = "dashed"), name = "Location") +
    ggplot2::scale_x_date(date_breaks = "1 month", labels = format_month_year_en) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::labs(
      title = "Cumulative NO3-N leaching over the monitoring period",
      subtitle = "DL method, linear interpolation (LI) | mean +/- SE per fertilisation x location",
      x = NULL, y = expression("Cumulative NO"[3]*"-N leaching (kg N ha"^{-1}*")")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(legend.position = "top", axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

#' Scatter plot of field-derived (DL-LI) vs. Daisy-modelled daily NO3-N
#' leaching, one panel per rotation strip, with a 1:1 reference line and each
#' panel's R^2/RMSE (from validate_dl_vs_daisy()) annotated. Ported from
#' SOIL_WATER.Rmd STEP 5.3a.
#'
#' @param dl_leaching_daily the `$daily` element of build_no3_leaching_dataset()
#' @param daisy_no3 output of load_daisy_no3_leaching()
#' @param metrics optional pre-computed validate_dl_vs_daisy() result
#'   (avoids recomputing it if the caller already has it, e.g. from the
#'   `no3_dl_vs_daisy_validation` target)
plot_no3_dl_vs_daisy <- function(dl_leaching_daily, daisy_no3, metrics = NULL) {
  paired <- build_dl_vs_daisy_paired(dl_leaching_daily, daisy_no3)
  if (is.null(metrics)) metrics <- validate_dl_vs_daisy(dl_leaching_daily, daisy_no3)

  rotation_labels <- c(A2 = "A2 (WW-SY-WW rotation)", A3 = "A3 (GC-GC-SB rotation)")
  ann <- metrics |>
    dplyr::mutate(
      label = sprintf("R² = %.2f\nRMSE = %.3f", r2, RMSE),  # R² = proper superscript-2 glyph, not literal "R^2"
      x = -Inf, y = Inf
    )

  ggplot2::ggplot(paired, ggplot2::aes(x = N_daisy, y = N_LI_field)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(alpha = 0.5, colour = "#2B3990") +
    ggplot2::geom_text(
      data = ann, ggplot2::aes(x = x, y = y, label = label),
      hjust = -0.1, vjust = 1.3, size = 3.4, inherit.aes = FALSE
    ) +
    # scales = "free": the two rotation strips' field-vs-Daisy magnitudes
    # differ substantially (that gap is itself part of what the low R^2
    # values report) -- a shared/equal-aspect scale would compress one
    # panel into an unreadable sliver rather than showing it.
    ggplot2::facet_wrap(~ daisy_col, scales = "free",
                        labeller = ggplot2::labeller(daisy_col = rotation_labels)) +
    ggplot2::labs(
      title = "Field DL-LI vs. Daisy-modelled daily NO3-N leaching",
      subtitle = "Cross-validation by rotation strip | sim = Daisy, obs = field DL",
      x = expression("Daisy-modelled N leaching (kg N ha"^{-1}*" d"^{-1}*")"),
      y = expression("Field DL-LI N leaching (kg N ha"^{-1}*" d"^{-1}*")")
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))
}
