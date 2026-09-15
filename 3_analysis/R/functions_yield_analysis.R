# Crop yield summary table and preliminary figures. Split out of
# 2_preprocessing/R/functions_yield_quality.R (which still holds the
# import/clean/covariate-derivation half) so this stage-3 file holds only
# analysis/visualisation code: a generic summary table plus descriptive
# companions to fit_yield_dry_model()'s formal test (functions_statistics.R)
# -- no plotting function existed for either outcome before this pass. Uses
# utils.R's fertilisation_colors/location_simple_colors so these share a
# visual language with the NO3-leaching figures.

#' Generic dry-matter yield summary table (mean +/- SE) by crop product and
#' an arbitrary grouping variable (e.g. GLOBAL_SUB_STRIP_LOCATION,
#' FERTILISATION, or their interaction), rendered with gt.
#'
#' Adapted (not a verbatim port -- the source notebook defines
#' make_crop_yield_gt() twice, once hardcoded to crop_code = "WW" and once to
#' crop_code = "SB", a real naming collision) into one generic function keyed
#' on whatever grouping column is passed in, with no hardcoded crop default.
make_yield_summary_gt <- function(data, group_var, outcome = "DRY_WEIGHT_TON_HA",
                                   title = "Crop yield summary") {
  group_sym <- rlang::sym(group_var)
  outcome_sym <- rlang::sym(outcome)

  summary_tbl <- data |>
    dplyr::filter(!is.na(!!outcome_sym)) |>
    dplyr::group_by(CROP_PRODUCT, !!group_sym) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(!!outcome_sym, na.rm = TRUE),
      se = sd(!!outcome_sym, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  summary_tbl |>
    gt::gt(groupname_col = "CROP_PRODUCT") |>
    gt::tab_header(title = title) |>
    gt::fmt_number(columns = c(mean, se), decimals = 2) |>
    gt::cols_label(!!group_var := group_var, n = "n", mean = "Mean (t/ha)", se = "SE")
}

#' Box + jitter of dry-matter yield by crop product, fertilisation and
#' residue management -- the core Study 2 productivity result.
#'
#' @param data study2_yield_analysis_ready (or a subset of it)
plot_yield_by_treatment <- function(data) {
  ggplot2::ggplot(data |> dplyr::filter(!is.na(DRY_WEIGHT_TON_HA)),
                  ggplot2::aes(x = CROP_PRODUCT, y = DRY_WEIGHT_TON_HA, fill = FERTILISATION)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7,
                          position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_point(ggplot2::aes(colour = FERTILISATION),
                        position = ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
                        size = 1.6, alpha = 0.6) +
    ggplot2::facet_wrap(~ RESIDUE, labeller = ggplot2::labeller(
      RESIDUE = c(REMOVED = "Residue removed", REMAIN = "Residue retained")
    )) +
    ggplot2::scale_fill_manual(values = fertilisation_colors, name = "Fertilisation") +
    ggplot2::scale_colour_manual(values = fertilisation_colors, guide = "none") +
    ggplot2::labs(
      title = "Dry-matter yield by crop product, fertilisation and residue management",
      subtitle = "Vertical agrivoltaics (Study 2), 2023-2025, all crops pooled",
      x = "Crop product", y = "Dry-matter yield (t/ha)"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
                  axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                  legend.position = "top")
}

#' Box + jitter of dry-matter yield by position relative to the vertical PV
#' panels (between-row vs. open-field), faceted by crop product -- the
#' position-effect result (glmmtmb_loc; see the concept paper's ANOVA table).
#'
#' @param data study2_yield_analysis_ready (or a subset of it)
plot_yield_by_location <- function(data) {
  ggplot2::ggplot(data |> dplyr::filter(!is.na(DRY_WEIGHT_TON_HA)),
                  ggplot2::aes(x = LOCATION, y = DRY_WEIGHT_TON_HA, fill = LOCATION)) +
    ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.7, width = 0.6) +
    ggplot2::geom_jitter(ggplot2::aes(colour = LOCATION), width = 0.12, size = 1.6, alpha = 0.6) +
    ggplot2::facet_wrap(~ CROP_PRODUCT, scales = "free_y") +
    ggplot2::scale_fill_manual(values = location_simple_colors) +
    ggplot2::scale_colour_manual(values = location_simple_colors) +
    ggplot2::labs(
      title = "Dry-matter yield by position relative to vertical PV panels",
      subtitle = "Vertical agrivoltaics (Study 2), 2023-2025, all crops pooled",
      x = NULL, y = "Dry-matter yield (t/ha)"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"), legend.position = "none")
}
