# Figure 2 -- Nitrate leaching under VAPV, crop rotation and management.
# Uses no3_leaching_result$daily (build_no3_leaching_dataset()'s daily DL-LI
# table) for the temporal panels (A/B) and no3_leaching_analysis_ready (the
# per-cup cumulative total, `$total`) for the model-based panels (C/D/E), via
# 2 new candidate models (fit_no3_substrip_model, fit_no3_management_model)
# plus the existing fit_no3_model_manuscript (functions_statistics.R).
#
# Layout (one shared LOCATION legend in a dedicated top strip -- see
# fig01_crop_productivity.R's assemble_fig01() docstring for why
# patchwork::guide_area() is used instead of relying on
# plot_layout(guides = "collect") alone):
#   A: cumulative leaching through time, VAPV vs open field, faceted by
#      rotation (WW-SY-WW / GC-GC-SB) -- descriptive (mean +/- SE, NOT a
#      95% CI -- see fig02_panel_a()'s docstring), establishes the broad
#      temporal/rotational pattern, with real sowing/harvest event markers
#      (from crop_calendar). Deliberately does NOT break out the 6 substrip
#      trajectories (that is panel B's job) -- keeping A simple creates a
#      system -> substrip -> management -> synthesis visual hierarchy
#      across the whole figure.
#   B: cumulative leaching through time by the 6 substrip positions (VAPV
#      zone x west/centre/east) -- descriptive, "zooms in" on the spatial
#      mechanism panel A cannot show.
#   C: model-adjusted FINAL cumulative leaching by substrip position (EMM +/-
#      95% CI) -- the statistical summary of the spatial gradient panel B
#      traces through time. Coloured by ROW POSITION (same palette as B,
#      legend suppressed -- see fig02_panel_c()'s docstring), not by
#      environment, to visually link B and C.
#   D: model-adjusted leaching under the 4 fertiliser x residue combinations,
#      stratified by CROP PHASE (winter wheat / ryegrass -- these are the
#      crops growing on the 2 monitored strips, not the strips' full
#      multi-year rotation identities; see fig02_panel_d()'s docstring).
#      Cell counts allow this stratification: ~5-6 cups per crop phase x
#      fertiliser x residue cell, confirmed empirically.
#   E: forest plot of the model's main effects and supported interactions
#      (VAPV, fertiliser, residue, fertiliser x residue, VAPV x fertiliser,
#      VAPV x residue), separated by a blank spacer row (not just a line).
#
# Only 47 suction cups total across the whole dataset -- every panel here
# should be read with that sample size in mind (wide CIs are a property of
# the data, not a modelling defect).

#' Derive the 6-level GLOBAL_SUB_STRIP_LOCATION factor (VAPV zone x
#' west/centre/east) for the NO3 branch, matching the yield branch's
#' convention (see add_study2_covariates(), 2_preprocessing/R/functions_yield_quality.R)
#' but built here from the NO3 dataset's lowercase location/sub_strip_location
#' columns (see prepare_sc_valid()'s docstring, functions_no3_leaching.R, for
#' why those needed to be added to the DL pipeline in the first place).
#' Also relevels LOCATION/FERTILISATION/RESIDUE so revpairwise contrasts read
#' "VAPV - open field" / "Digestate - Mineral" / "Retained - Removed"
#' (same convention as fig01_crop_productivity.R).
fig02_relevel <- function(data) {
  dplyr::mutate(
    data,
    location = forcats::fct_relevel(as.factor(location), "OUTSIDE_PV"),
    fertilisation = forcats::fct_relevel(as.factor(fertilisation), "MINERAL_FERTILISER"),
    residue = forcats::fct_relevel(as.factor(residue), "REMOVED"),
    GLOBAL_SUB_STRIP_LOCATION = factor(
      paste(as.character(location), sub_strip_location, sep = "_"),
      levels = c("OUTSIDE_PV_WEST", "OUTSIDE_PV_MIDDLE", "OUTSIDE_PV_EAST",
                 "BETWEEN_PV_WEST", "BETWEEN_PV_MIDDLE", "BETWEEN_PV_EAST")
    )
  )
}

fig02_fit_models <- function(no3_total) {
  d <- fig02_relevel(no3_total)
  list(
    substrip   = fit_no3_substrip_model(d),
    management = fit_no3_management_model(d),
    forest     = fit_no3_model_manuscript(d)
  )
}

#' Real sowing/harvest dates for panel A's event markers, derived directly
#' from the `crop_calendar` target (never hand-typed/invented dates). Only
#' the crop phase that gives each rotation its name is used --
#' Winter Wheat for the WW-SY-WW strip (A2), Spring Barley for the
#' GC-GC-SB strip (A3, the "SB" in that name) -- so exactly 2 event TYPES
#' (Sowing, Harvest) appear, rotation-specific via `daisy_col` (== `strip`
#' for these 2 monitored strips), matching the "at most 2-3 key event
#' types, rotation-specific if the event structure differs" guidance.
#' Returns NULL rows silently dropped (e.g. if a phase's end_type isn't
#' "harvest" -- last_mention rows are not treated as a harvest date).
fig02_rotation_events <- function(crop_calendar) {
  crop_calendar |>
    dplyr::filter(
      (strip == "A2" & crop_name == "Winter Wheat") |
        (strip == "A3" & crop_name == "Spring Barley")
    ) |>
    dplyr::transmute(
      daisy_col = strip,
      Sowing = start_date,
      Harvest = dplyr::if_else(end_type == "harvest", end_date, as.Date(NA))
    ) |>
    tidyr::pivot_longer(c(Sowing, Harvest), names_to = "event", values_to = "date") |>
    dplyr::filter(!is.na(date))
}

# ---- Panel A: cumulative leaching through time, VAPV vs open, by rotation ---

#' @param dl_daily no3_leaching_result$daily
#' @param events optional output of fig02_rotation_events(crop_calendar) --
#'   adds thin grey sowing/harvest markers per rotation facet. NULL (default)
#'   omits them entirely (e.g. if crop_calendar isn't available to a caller).
#'
#' Shaded ribbon = mean +/- 1 SE across suction cups (NOT a 95% CI) -- this
#' is a descriptive summary of the per-cup cumulative time series, not a
#' model-based estimate, so it does not use the same 95% CI convention as
#' the model-derived panels (C/D/E). Stated explicitly here and in the
#' figure caption so the two are never confused.
fig02_panel_a <- function(dl_daily, events = NULL) {
  cum_df <- dl_daily |>
    dplyr::arrange(suction_cup, date) |>
    dplyr::group_by(suction_cup, location, daisy_col) |>
    dplyr::mutate(cum_N = cumsum(tidyr::replace_na(N_LI, 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(location_lab = factor(relabel_pv_location(location), levels = c("Open field", "VAPV"))) |>
    dplyr::group_by(date, location_lab, daisy_col) |>
    dplyr::summarise(
      mean_cum = mean(cum_N, na.rm = TRUE),
      se_cum = sd(cum_N, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )

  # Rotation names, not the A2/A3 strip codes -- those are documented in the
  # caption instead (per manuscript feedback: identifiers like A2/A3 belong
  # in the caption, not a facet strip that must stay short enough not to
  # truncate).
  rotation_labels <- c(A2 = "WW–SY–WW", A3 = "GC–GC–SB")

  p <- ggplot2::ggplot(cum_df, ggplot2::aes(x = date, y = mean_cum, colour = location_lab, fill = location_lab))

  if (!is.null(events) && nrow(events) > 0) {
    y_top <- max(cum_df$mean_cum + cum_df$se_cum, na.rm = TRUE)
    p <- p +
      ggplot2::geom_vline(data = events, ggplot2::aes(xintercept = date),
                          linewidth = 0.4, colour = "grey55") +
      ggplot2::geom_text(
        data = events, ggplot2::aes(x = date, y = y_top * 1.06, label = event), inherit.aes = FALSE,
        angle = 90, hjust = 1, vjust = -0.3, size = 2.6, colour = "grey40"
      )
  }

  p +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_cum - se_cum, ymax = mean_cum + se_cum), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~daisy_col, labeller = ggplot2::labeller(daisy_col = rotation_labels)) +
    ggplot2::scale_colour_manual(values = c("Open field" = unname(location_simple_colors["OUTSIDE_PV"]),
                                             "VAPV" = unname(location_simple_colors["BETWEEN_PV"])), name = "Environment") +
    ggplot2::scale_fill_manual(values = c("Open field" = unname(location_simple_colors["OUTSIDE_PV"]),
                                           "VAPV" = unname(location_simple_colors["BETWEEN_PV"])), name = "Environment") +
    ggplot2::scale_x_date(labels = format_month_year_en, date_breaks = "6 months") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.14))) +
    ggplot2::labs(
      title = "Cumulative NO₃–N leaching by rotation",
      x = NULL, y = expression("Cumulative NO"[3]*"–N (kg N ha"^{-1}*")")
    ) +
    theme_study2() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# ---- Panel B: cumulative leaching through time by substrip position --------

#' @param dl_daily no3_leaching_result$daily
fig02_panel_b <- function(dl_daily) {
  cum_df <- dl_daily |>
    dplyr::arrange(suction_cup, date) |>
    dplyr::group_by(suction_cup, location, sub_strip_location) |>
    dplyr::mutate(cum_N = cumsum(tidyr::replace_na(N_LI, 0))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      location_lab = factor(relabel_pv_location(location), levels = c("Open field", "VAPV")),
      position = factor(sub_strip_location, levels = c("WEST", "MIDDLE", "EAST"),
                         labels = c("West", "Centre", "East"))
    ) |>
    dplyr::group_by(date, location_lab, position) |>
    dplyr::summarise(mean_cum = mean(cum_N, na.rm = TRUE), .groups = "drop")

  # position_colors (utils.R) -- the SAME West/Centre/East palette used
  # everywhere else in the manuscript that encodes row position.
  ggplot2::ggplot(cum_df, ggplot2::aes(x = date, y = mean_cum, colour = position)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~location_lab) +
    ggplot2::scale_colour_manual(values = position_colors, limits = names(position_colors), name = "Row position") +
    ggplot2::scale_x_date(labels = format_month_year_en, date_breaks = "6 months") +
    ggplot2::labs(
      title = "Substrip-specific leaching dynamics",
      x = NULL, y = expression("Cumulative NO"[3]*"–N (kg N ha"^{-1}*")")
    ) +
    theme_study2() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

# ---- Panel C: final cumulative leaching by substrip (model EMMs) -----------

#' Coloured by ROW POSITION (not environment) so panel C visually links to
#' panel B (same West/Centre/East palette, `position_colors` in utils.R) --
#' per manuscript feedback: environment is already communicated by the
#' x-axis grouping (open field | VAPV) and the group headings, so colour is
#' free to instead carry the row-position dimension, strengthening the
#' visual connection between B (temporal substrip dynamics) and C (final
#' substrip outcome). C's own colour legend is therefore suppressed --
#' showing it again would just repeat panel B's "Row position" legend --
#' and the caption states explicitly that C's point colours match B's
#' legend. Two blank spacer levels (matching Figure 1A's gap) separate the
#' open-field and VAPV groups.
fig02_panel_c <- function(fit) {
  emm_df <- get_emmeans(fit, ~ GLOBAL_SUB_STRIP_LOCATION) |>
    dplyr::mutate(
      env_code = dplyr::if_else(stringr::str_starts(as.character(GLOBAL_SUB_STRIP_LOCATION), "BETWEEN"), "V", "O"),
      pos = stringr::str_extract(as.character(GLOBAL_SUB_STRIP_LOCATION), "WEST|MIDDLE|EAST"),
      POSITION = factor(
        paste(env_code, pos, sep = "_"),
        levels = c("O_WEST", "O_MIDDLE", "O_EAST", "GAP1", "GAP2", "V_WEST", "V_MIDDLE", "V_EAST")
      ),
      POSITION_LAB = factor(
        c(WEST = "West", MIDDLE = "Centre", EAST = "East")[pos],
        levels = names(position_colors)
      )
    )

  y_max <- max(emm_df$ucl, na.rm = TRUE)
  plot_emm_point_range(
    emm_df, x = "POSITION", y_lab = expression("Total NO"[3]*"–N leached (kg N ha"^{-1}*")"),
    colour = "POSITION_LAB", colour_values = position_colors,
    point_size = MS_POINT_SIZE, title = "Final cumulative leaching by substrip"
  ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::geom_vline(xintercept = 4.5, linetype = "dashed", linewidth = MS_REF_LINEWIDTH, colour = "grey60") +
    ggplot2::annotate("text", x = 2, y = y_max * 1.12, label = "Open field", vjust = 0, fontface = "bold", size = 3.8) +
    ggplot2::annotate("text", x = 7, y = y_max * 1.12, label = "VAPV", vjust = 0, fontface = "bold", size = 3.8) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.16))) +
    ggplot2::scale_x_discrete(
      labels = c(O_WEST = "West", O_MIDDLE = "Centre", O_EAST = "East", GAP1 = "", GAP2 = "",
                 V_WEST = "West", V_MIDDLE = "Centre", V_EAST = "East"),
      expand = ggplot2::expansion(add = 1.8)
    ) +
    # Small rotation -- same empirical justification as Figure 1 panel A's
    # identical 8-category axis (see add_env_headers()'s docstring there).
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1, vjust = 1))
}

# ---- Panel D: fertiliser x residue by rotation/crop ------------------------

#' IMPORTANT SEMANTIC NOTE (per manuscript feedback -- do not reintroduce
#' this mismatch): the facet variable here is `crop` (winter wheat /
#' ryegrass), i.e. WHICH CROP was growing on a monitored strip at the time
#' of measurement -- it is NOT the strip's multi-year rotation identity.
#' The 2 monitored strips' full rotations are WW-SY-WW (historically "A2")
#' and GC-GC-SB (historically "A3") -- see panel A. A cup on the WW-SY-WW
#' strip during the winter-wheat phase reads `crop == "Winter Wheat"`; the
#' SAME strip during a soybean phase would read differently, and no soybean
#' or grass-clover suction-cup readings exist in this dataset (confirmed:
#' `crop` only ever takes 2 values here, "Winter Wheat" and
#' "Ryegrass (undersown)"). The panel is therefore titled "by crop phase",
#' not "by rotation", and facets are labelled with the crop's full name, not
#' a rotation code.
fig02_panel_d <- function(fit) {
  # NB: the NO3 branch's columns are lowercase (fertilisation/residue, not
  # FERTILISATION/RESIDUE as in the yield branch -- see functions_statistics.R's
  # fit_no3_model()/fit_no3_management_model() docstrings for why the two
  # branches' naming conventions differ).
  crop_phase_labels <- c("Winter Wheat" = "Winter wheat", "Ryegrass (undersown)" = "Ryegrass")

  emm_df <- get_emmeans(fit, ~ fertilisation * residue | crop) |>
    dplyr::mutate(
      FERTILISATION_LAB = factor(fertilisation_labels[as.character(fertilisation)], levels = fertilisation_labels),
      RESIDUE_LAB = factor(residue_labels[as.character(residue)], levels = residue_labels),
      crop_phase = dplyr::coalesce(crop_phase_labels[as.character(crop)], as.character(crop))
    )

  plot_emm_point_range(
    emm_df, x = "RESIDUE_LAB", y_lab = expression("Total NO"[3]*"–N leached (kg N ha"^{-1}*")"),
    colour = "FERTILISATION_LAB",
    colour_values = stats::setNames(unname(fertilisation_colors), fertilisation_labels[names(fertilisation_colors)]),
    facet = "crop_phase", title = "Fertiliser × residue effects by crop phase"
  ) +
    ggplot2::labs(x = NULL, colour = "Fertiliser")
}

# ---- Panel E: model-effect forest plot -------------------------------------

#' Forest plot of the model's 3 main effects (VAPV, fertiliser, residue) and
#' 3 supported interactions (fertiliser x residue, VAPV x fertiliser,
#' VAPV x residue), in that order, with a blank spacer row (not just a
#' divider line) between the two groups -- the preferred implementation per
#' manuscript feedback: a small vertical GAP reads as a grouping cue more
#' reliably across print/screen than a faint line alone. Main effects
#' always appear above the gap, interactions below.
fig02_panel_e <- function(fit) {
  loc <- calc_vapv_response(fit, ~ location, method = "revpairwise") |> dplyr::mutate(label = "VAPV vs open field")
  fert <- calc_vapv_response(fit, ~ fertilisation, method = "revpairwise") |>
    dplyr::mutate(label = "Digestate vs mineral")
  resid <- calc_vapv_response(fit, ~ residue, method = "revpairwise") |>
    dplyr::mutate(label = "Retained vs removed")
  fert_resid_int <- calc_interaction_response(fit, c("fertilisation", "residue")) |>
    dplyr::mutate(label = "Fertiliser × residue")
  loc_fert_int <- calc_interaction_response(fit, c("location", "fertilisation")) |>
    dplyr::mutate(label = "VAPV × fertiliser")
  loc_resid_int <- calc_interaction_response(fit, c("location", "residue")) |>
    dplyr::mutate(label = "VAPV × residue")

  # A single-space label is a blank spacer row (see plot_effect_forest()'s
  # `na.rm = TRUE` note on `df` for why NA estimate/CI draws nothing) --
  # NOT a 7th contrast; it just reserves a category slot for visual spacing.
  gap_row <- tibble::tibble(label = " ", pct_response = NA_real_, pct_lcl = NA_real_, pct_ucl = NA_real_)

  forest_df <- dplyr::bind_rows(
    dplyr::select(loc, label, pct_response, pct_lcl, pct_ucl),
    dplyr::select(fert, label, pct_response, pct_lcl, pct_ucl),
    dplyr::select(resid, label, pct_response, pct_lcl, pct_ucl),
    gap_row,
    dplyr::select(fert_resid_int, label, pct_response, pct_lcl, pct_ucl),
    dplyr::select(loc_fert_int, label, pct_response, pct_lcl, pct_ucl),
    dplyr::select(loc_resid_int, label, pct_response, pct_lcl, pct_ucl)
  ) |>
    dplyr::mutate(label = forcats::fct_inorder(label))

  plot_effect_forest(
    forest_df, label_col = "label", estimate_col = "pct_response", lcl_col = "pct_lcl", ucl_col = "pct_ucl",
    x_lab = "Effect on NO₃–N leaching (%)", ref_line = 0,
    title = "Model effects on NO₃–N leaching"
  )
}

# ---- Assembly ---------------------------------------------------------------

#' Build every Figure 2 panel from the raw NO3 pipeline outputs, without
#' assembling the composite -- mirrors fig01_build_panels()/
#' fig01_assemble_composite()'s split so 01_no3_leaching_dl_method.qmd can
#' display each subfigure individually before compositing.
#'
#' @param crop_calendar optional -- the `crop_calendar` target. When
#'   supplied, panel A gets real sowing/harvest event markers (see
#'   fig02_rotation_events()); when NULL, panel A is built without them
#'   (e.g. for a caller that hasn't loaded crop_calendar).
fig02_build_panels <- function(no3_leaching_result, crop_calendar = NULL) {
  fits <- fig02_fit_models(no3_leaching_result$total)
  events <- if (!is.null(crop_calendar)) fig02_rotation_events(crop_calendar) else NULL
  list(
    fits = fits,
    panel_a = fig02_panel_a(no3_leaching_result$daily, events = events),
    panel_b = fig02_panel_b(no3_leaching_result$daily),
    panel_c = fig02_panel_c(fits$substrip),
    panel_d = fig02_panel_d(fits$management),
    panel_e = fig02_panel_e(fits$forest)
  )
}

#' Assemble the 5 panels into the Figure 2 composite: one shared
#' "Environment" legend (panel A's) in a dedicated top strip.
#'
#' Legend logic (A is the only one collected -- see below for why B/C/D are
#' handled differently): A maps colour to Environment (open field/VAPV) and
#' is the SOLE user of that palette now that C recolours by row position
#' instead (see fig02_panel_c()'s docstring) -- its legend collects into
#' guide_area() alone, no merge risk since nothing else shares it. B maps
#' colour to row position and keeps its own inline legend
#' (`plot_layout(guides = "keep")`). C ALSO maps colour to row position
#' (same `position_colors` palette as B) but its legend is suppressed
#' entirely -- not merged, just hidden -- because a point-range plot's
#' colour key glyph (point + vertical errorbar) and a line plot's (a line
#' segment) don't merge via `guides = "collect"` even for an identical
#' scale (the same key-glyph mismatch documented for Figure 1's point-range
#' vs forest-plot panels), so showing it again would either fail to merge
#' (duplicate legend) or require another `guide_area()` slot for no real
#' benefit -- the caption states explicitly that C's colours match B's
#' legend. D maps colour to fertiliser and keeps its own inline legend.
#' E is a plain forest (no colour mapping, no legend).
#'
#' Column widths: EQUAL (1:1), not the manuscript's suggested per-row ratios
#' (row 1 A:B = 1:1, row 2 C:D = 0.95:1.05) -- two things ruled that out.
#' First, patchwork's `plot_layout(widths = ...)` applies one width vector to
#' every row of a single design-string grid, so genuinely different per-row
#' ratios would require nesting separate patchwork objects per row, which
#' reintroduces exactly the guide-collection fragility documented above and
#' throughout Figure 1's development. Second, and decisively: giving C only
#' 0.95 of the width actually CLIPPED its "VAPV" header text at this
#' figure's final physical size (confirmed by rendering it) -- C's 6-position
#' x-axis needs at least as much room as D's, not less. Equal columns avoid
#' the clipping the brief's own top-priority "no clipping" requirement rules
#' out, at the cost of the minor (10 pp) width asymmetry being only
#' approximated rather than exact.
fig02_assemble_composite <- function(panels) {
  pa <- panels$panel_a
  pb <- panels$panel_b + patchwork::plot_layout(guides = "keep")
  pc <- panels$panel_c  # legend already off inside fig02_panel_c()
  pd <- panels$panel_d + patchwork::plot_layout(guides = "keep")

  layout <- "
    LLLL
    AABB
    AABB
    CCDD
    CCDD
    EEEE
    EEEE
  "
  (pa + pb + pc + pd + panels$panel_e + patchwork::guide_area()) +
    patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(
      design = layout, guides = "collect",
      heights = c(0.16, 1, 1, 1, 1, 1, 1), widths = c(1, 1, 1, 1)
    ) &
    manuscript_composite_theme()
}

#' End-to-end Figure 2 build: fit models, build all 5 panels, assemble the
#' composite. This is what `_targets.R`'s `fig02_result` target calls.
#' @param crop_calendar optional -- see fig02_build_panels()
assemble_fig02 <- function(no3_leaching_result, crop_calendar = NULL) {
  panels <- fig02_build_panels(no3_leaching_result, crop_calendar = crop_calendar)
  fits <- panels$fits
  composite <- fig02_assemble_composite(panels)

  extract_result <- function(fit) list(
    formula = paste(deparse(stats::formula(fit)), collapse = " "),
    n_obs = stats::nobs(fit),
    AIC = stats::AIC(fit),
    converged = isTRUE(fit$sdr$pdHess),
    anova = as.data.frame(anova_glmm(fit))
  )

  list(
    substrip = extract_result(fits$substrip),
    management = extract_result(fits$management),
    forest = extract_result(fits$forest),
    panel_a = panels$panel_a, panel_b = panels$panel_b, panel_c = panels$panel_c,
    panel_d = panels$panel_d, panel_e = panels$panel_e,
    composite = composite
  )
}

#' Omnibus (Type II Wald chi-square) test table for every Figure 2 model --
#' see fig01_omnibus_table()'s docstring for the same rationale. Reuses the
#' `anova` data frame already computed inside assemble_fig02(); no new
#' statistical test is run here.
#'
#' @param fig02_result the list returned by assemble_fig02()
fig02_omnibus_table <- function(fig02_result) {
  model_label <- c(
    substrip = "NO3-N leaching: substrip model",
    management = "NO3-N leaching: management model",
    forest = "NO3-N leaching: main-effects model"
  )
  rows <- lapply(names(model_label), function(nm) {
    aov <- fig02_result[[nm]]$anova
    if (is.null(aov) || !"Df" %in% names(aov)) return(NULL)
    tibble::rownames_to_column(aov, var = "Term") |>
      dplyr::transmute(
        Response = "NO3-N leaching", Model = model_label[[nm]], Term,
        Df = Df, Chisq = Chisq, P = `Pr(>Chisq)`
      )
  })
  dplyr::bind_rows(rows)
}

fig02_caption <- function() {
  paste(
    "**Figure 2. Nitrate leaching under vertical agrivoltaics (VAPV), crop rotation and agronomic",
    "management.** Based on n = 47 suction cups (Drainage-Linear method, linear interpolation; Vogeler",
    "et al. 2020) -- every panel should be read with this modest sample size in mind; wide confidence",
    "intervals reflect the data, not a modelling defect. West/centre/east row position uses the SAME",
    "colour mapping in every panel that shows it (B and C).",
    "(A) Cumulative NO₃–N leaching through time, open field vs VAPV, faceted by rotation",
    "(WW–SY–WW, historically labelled strip A2; GC–GC–SB, strip A3) -- establishes the broad",
    "temporal and rotational pattern. Shaded ribbon = mean ± 1 SE across suction cups -- a descriptive",
    "summary, NOT a 95% CI (unlike the model-derived panels C-E below). Thin grey vertical lines mark",
    "sowing and harvest of each rotation's namesake crop (winter wheat for WW–SY–WW; spring barley for",
    "GC–GC–SB), read directly from the crop_calendar target -- not inferred or invented dates.",
    "(B) Cumulative NO₃–N leaching through time by the 6 substrip positions (open field/VAPV x",
    "west/centre/east row position) -- \"zooms in\" on the spatial mechanism (A) cannot show; deliberately",
    "kept out of (A) to preserve a system -> substrip -> management -> synthesis visual hierarchy.",
    "(C) Model-adjusted FINAL cumulative leaching by substrip position (estimated marginal means ± 95%",
    "CI) -- the statistical summary of the spatial gradient (B) traces through time. Points are coloured",
    "by ROW POSITION (matching panel B's legend exactly, not repeated here) rather than by environment:",
    "open field vs VAPV is already communicated by the x-axis grouping and header text, so colour is used",
    "to visually connect this panel to (B) instead. Model: fit_no3_substrip_model(), Gamma(log),",
    "N_LI_kgNha ~ GLOBAL_SUB_STRIP_LOCATION + crop, no dispersion/spatial term (no ROW/COLUMN grid exists",
    "for suction-cup locations).",
    "(D) Model-adjusted leaching under the 4 fertiliser x residue combinations, stratified by CROP PHASE",
    "(winter wheat / ryegrass) rather than by rotation: `crop` records which crop was growing on a",
    "monitored strip at the time of measurement, not the strip's multi-year rotation identity (WW–SY–WW",
    "or GC–GC–SB, shown in panel A) -- only 2 crop phases were ever monitored, so \"by rotation\" would",
    "have mislabelled this panel. Cell counts allow this stratification (~5-6 cups per crop phase x",
    "fertiliser x residue cell, confirmed empirically). Model: fit_no3_management_model(), Gamma(log),",
    "N_LI_kgNha ~ crop * fertilisation * residue (full 3-way interaction, converged).",
    "(E) Forest plot of model-derived percentage effects with 95% CI: VAPV environment, fertiliser source,",
    "residue management (the 3 main effects, above the blank spacer row), and the supported fertiliser x",
    "residue, VAPV x fertiliser and VAPV x residue interactions (below it). Model:",
    "fit_no3_model_manuscript(), Gamma(log), N_LI_kgNha ~ location + fertilisation + residue + crop +",
    "location:fertilisation + location:residue + fertilisation:residue -- the same log-link",
    "percentage-response convention as Figure 1B/E. Dashed line at 0% = no effect; no significance stars",
    "-- a CI crossing zero already conveys the inferential result.",
    "Reference level for every contrast: open field (OUTSIDE_PV); mineral fertiliser; residue removed.",
    "",
    "Percentage-effect definitions (all computed on the model's log-link scale as a ratio, then expressed",
    "as a percentage -- not a ratio of raw arithmetic means; panel E):",
    "  VAPV vs open field (%)   = 100 x (VAPV - Open field) / Open field",
    "  Digestate vs mineral (%) = 100 x (Digestate - Mineral) / Mineral",
    "  Retained vs removed (%) = 100 x (Retained - Removed) / Removed",
    sep = "\n"
  )
}
