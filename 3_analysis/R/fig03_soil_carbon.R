# Figure 3 -- Soil organic carbon (SOC): is there measurable evidence that
# VAPV and row position affect soil C, and is it consistent across the two
# survey rounds sampled so far?
#
# Uses fit_soc_model_manuscript() (functions_statistics.R) on the IUNG lab
# soil-chemistry dataset (build_iung_soil_dataset(),
# 2_preprocessing/R/functions_soil_chemistry.R). This is a PARTIAL-DATA
# figure, not the full CASE A design the manuscript brief describes:
#   - No true same-point paired baseline/final SOC exists (every sample code
#     is a distinct physical point -- see build_iung_soil_dataset()); "across
#     survey rounds" language is used throughout instead of "change"/"Delta".
#   - FERTILISATION x RESIDUE SOC panels are NOT built: FERTILISATION only
#     resolves for 40/114 rows (fully confounded with survey round), and
#     RESIDUE is not resolvable at this sampling grain at all (same
#     limitation as the TDR soil-water outcome). A fertiliser x residue SOC
#     comparison is flagged NOT CURRENTLY SUPPORTED rather than built on 40
#     confounded rows.
# Panels actually supported by the data:
#   A: SOC by VAPV/open field x row position (west/middle/east)
#   B: SOC by VAPV/open field x survey round (temporal consistency check)
#   C: forest plot of the model's estimable effects

fig03_fit_model <- function(data) {
  data <- dplyr::mutate(
    data,
    LOCATION = forcats::fct_relevel(as.factor(LOCATION), "OUTSIDE_PV"),
    SURVEY_ROUND = forcats::fct_relevel(as.factor(SURVEY_ROUND), "2024_baseline", "2025_T1", "2025_T2"),
    SUB_STRIP_LOCATION = forcats::fct_relevel(as.factor(SUB_STRIP_LOCATION), "WEST", "MIDDLE", "EAST")
  )
  fit_soc_model_manuscript(data)
}

#' Panel A: model-adjusted SOC (%) by VAPV/open field x row position.
fig03_panel_a <- function(fit, data) {
  emm_df <- get_emmeans(fit, ~ LOCATION * SUB_STRIP_LOCATION) |>
    dplyr::mutate(LOCATION_LAB = factor(relabel_pv_location(LOCATION), levels = c("Open field", "VAPV")))

  raw <- dplyr::mutate(data, LOCATION_LAB = factor(relabel_pv_location(LOCATION), levels = c("Open field", "VAPV")))

  plot_emm_point_range(
    emm_df, x = "SUB_STRIP_LOCATION", y_lab = "Soil organic carbon (%)",
    colour = "LOCATION_LAB",
    colour_values = c("Open field" = unname(location_simple_colors["OUTSIDE_PV"]),
                       "VAPV" = unname(location_simple_colors["BETWEEN_PV"])),
    raw_data = raw, y_raw = "SOC_PCT"
  ) +
    ggplot2::labs(x = "Row position", colour = "Environment")
}

#' Panel B: model-adjusted SOC (%) by VAPV/open field x survey round.
fig03_panel_b <- function(fit) {
  emm_df <- get_emmeans(fit, ~ LOCATION * SURVEY_ROUND) |>
    dplyr::mutate(
      LOCATION_LAB = factor(relabel_pv_location(LOCATION), levels = c("Open field", "VAPV")),
      round_lab = factor(SURVEY_ROUND, levels = c("2024_baseline", "2025_T1", "2025_T2"),
                          labels = c("2024 (baseline)", "2025 (T1)", "2025 (T2)"))
    )

  plot_emm_point_range(
    emm_df, x = "round_lab", y_lab = "Soil organic carbon (%)",
    colour = "LOCATION_LAB",
    colour_values = c("Open field" = unname(location_simple_colors["OUTSIDE_PV"]),
                       "VAPV" = unname(location_simple_colors["BETWEEN_PV"]))
  ) +
    ggplot2::labs(x = "Survey round", colour = "Environment",
                  subtitle = "Different physical sampling points per round — not a paired change")
}

#' Panel C: forest plot of the model's estimable effects (differences in
#' SOC percentage points; gaussian/identity link, so NOT a ratio/percentage
#' response -- x-axis is in SOC percentage points).
fig03_panel_c <- function(fit) {
  loc <- get_pairwise_contrasts(fit, ~ LOCATION, method = "revpairwise") |>
    dplyr::filter(contrast == "BETWEEN_PV - OUTSIDE_PV") |>
    dplyr::mutate(label = "VAPV vs open field")
  pos <- get_pairwise_contrasts(fit, ~ SUB_STRIP_LOCATION, method = "revpairwise") |>
    dplyr::filter(contrast == "EAST - WEST") |>
    dplyr::mutate(label = "East vs west row position")
  round_t2 <- get_pairwise_contrasts(fit, ~ SURVEY_ROUND, method = "revpairwise") |>
    dplyr::filter(contrast == "2025_T2 - 2024_baseline") |>
    dplyr::mutate(label = "2025 (T2) vs 2024 (baseline)")
  interact <- calc_interaction_response(fit, c("LOCATION", "SURVEY_ROUND")) |>
    dplyr::filter(
      LOCATION_revpairwise == "BETWEEN_PV - OUTSIDE_PV",
      SURVEY_ROUND_revpairwise == "2025_T2 - 2024_baseline"
    ) |>
    dplyr::mutate(label = "VAPV effect: 2025 (T2) vs 2024 (baseline)")

  forest_df <- dplyr::bind_rows(
    dplyr::select(loc, label, estimate, lcl, ucl),
    dplyr::select(pos, label, estimate, lcl, ucl),
    dplyr::select(round_t2, label, estimate, lcl, ucl),
    dplyr::select(interact, label, estimate, lcl, ucl)
  ) |>
    dplyr::mutate(label = forcats::fct_inorder(label))

  plot_effect_forest(
    forest_df, label_col = "label",
    x_lab = "Effect on soil organic carbon (percentage points)", ref_line = 0
  )
}

#' Assemble the 3-panel Figure 3 composite. Portable-objects-only return
#' value (see assemble_fig01()'s docstring for why the live fit is not
#' included).
assemble_fig03 <- function(data) {
  fit <- fig03_fit_model(data)
  data_relevel <- dplyr::mutate(data, LOCATION = forcats::fct_relevel(as.factor(LOCATION), "OUTSIDE_PV"))

  pa <- fig03_panel_a(fit, data_relevel)
  pb <- fig03_panel_b(fit)
  pc <- tryCatch(fig03_panel_c(fit), error = function(e) {
    message("fig03_panel_c() failed -- returning a placeholder panel. Error: ", e$message)
    ggplot2::ggplot() + ggplot2::labs(title = "Effect forest not available", subtitle = e$message) + theme_study2()
  })

  composite <- (pa + pb) / pc +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(legend.position = "top")

  list(
    formula = paste(deparse(stats::formula(fit)), collapse = " "),
    n_obs = stats::nobs(fit),
    AIC = stats::AIC(fit),
    converged = isTRUE(fit$sdr$pdHess),
    anova = as.data.frame(anova_glmm(fit)),
    panel_a = pa, panel_b = pb, panel_c = pc, composite = composite
  )
}

fig03_caption <- function() {
  paste(
    "**Figure 3. Soil organic carbon (SOC) under vertical agrivoltaics (VAPV) -- partial-data template.**",
    "(A) Model-adjusted SOC (%) by growing environment (VAPV vs open field) and row position",
    "(west/middle/east); faint points are individual samples.",
    "(B) Model-adjusted SOC (%) by environment and survey round (2024 baseline; 2025 T1/T2).",
    "IMPORTANT: survey rounds sample DIFFERENT physical points each time (no location was resampled at",
    "the same spot) -- panel B shows SOC across survey rounds, not a paired change (Delta SOC) at any",
    "location.",
    "(C) Forest plot of the model's estimable effects (percentage points of SOC).",
    "Fertiliser x residue SOC comparisons are NOT shown: fertiliser source resolves for only 40/114",
    "samples (fully confounded with survey round) and residue management is not resolvable at this",
    "sampling grain (as for the TDR soil-water outcome) -- see",
    "3_analysis/R/functions_soil_chemistry.R for the coverage audit.",
    "Model: gaussian GLMM, SOC_PCT ~ LOCATION + SUB_STRIP_LOCATION + SURVEY_ROUND +",
    "LOCATION:SURVEY_ROUND, no random/spatial term (nest is fully crossed with the fixed effects).",
    "Error bars are 95% confidence intervals; points are estimated marginal means, not raw means.",
    "This figure will be rebuilt once repeated same-point SOC measurements and a resolved",
    "fertiliser x residue design are available from IUNG.",
    sep = "\n"
  )
}
