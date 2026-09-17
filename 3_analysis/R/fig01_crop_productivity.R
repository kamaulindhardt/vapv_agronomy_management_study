# Figure 1 -- Crop productivity, quality and agronomic management response to
# VAPV. Uses 5 candidate models in functions_statistics.R
# (fit_yield_substrip_model / fit_yield_pooled_model /
# fit_quality_substrip_model / fit_quality_pooled_model /
# fit_yield_management_model), fit on each crop's PRIMARY harvestable
# product only (PRODUCT_SIMPLE %in% c("GRAIN","BIOMASS")).
#
# Layout (3x2, one shared CROP legend in a dedicated top strip):
#   A: yield estimated marginal means by row position (west/centre/east),
#      open field vs VAPV on one combined x-axis (with a blank spacer level
#      + dashed divider between the two environments) and manual "Open
#      field"/"VAPV" group headers (no facet_wrap -- see
#      split_global_location()'s docstring for why)
#   B: overall VAPV effect on yield -- % response forest, one row per crop,
#      zero-reference line (the inferential counterpart to A)
#   C: substrip-specific VAPV effect on tissue N (percentage points), by row
#      position, POOLED ACROSS CROP (the fitted model has no
#      position:crop interaction -- see fig01_panel_c()'s docstring). The
#      spatial complement to D: C varies position but pools crop, D varies
#      crop but pools position.
#   D: overall VAPV effect on tissue N (percentage points), by crop, pooled
#      across position
#   E: agronomic management effects on yield (fertiliser, residue), per crop
#   F: VAPV x management interaction on yield (pooled across crop) -- does
#      VAPV change the management response itself? A difference-in-
#      differences contrast, not a main effect.
# Crop is double-encoded (colour AND point shape) in A/B/D/E (identity does
# not rely on colour alone); C and F are pooled across crop and have no crop
# legend. One shared legend via patchwork::guide_area() (see
# assemble_fig01()'s docstring for why plot_layout(guides = "collect") alone
# is not enough).

#' Keep only each crop's primary harvestable product (see
#' fit_yield_substrip_model()'s docstring for the crop -> product mapping):
#' WW/SY grain, GC/RG/SB biomass. Excludes straw/pod/residue by-products.
filter_primary_product <- function(data) {
  dplyr::filter(data, PRODUCT_SIMPLE %in% c("GRAIN", "BIOMASS"))
}

#' @param management also relevel FERTILISATION (reference =
#'   MINERAL_FERTILISER, so a revpairwise contrast reads "Digestate - Mineral")
#'   and RESIDUE (reference = REMOVED, so a revpairwise contrast reads
#'   "Retained - Removed") -- only needed for the panel E/F management model.
fig01_relevel <- function(data, management = FALSE) {
  data <- dplyr::mutate(
    data,
    LOCATION = forcats::fct_relevel(as.factor(LOCATION), "OUTSIDE_PV"),
    GLOBAL_SUB_STRIP_LOCATION = forcats::fct_relevel(
      as.factor(GLOBAL_SUB_STRIP_LOCATION),
      "OUTSIDE_PV_WEST", "OUTSIDE_PV_MIDDLE", "OUTSIDE_PV_EAST",
      "BETWEEN_PV_WEST", "BETWEEN_PV_MIDDLE", "BETWEEN_PV_EAST"
    ),
    CROP = factor(CROP, levels = intersect(study2_crop_order, unique(CROP)))
  )
  if (management) {
    data <- dplyr::mutate(
      data,
      FERTILISATION = forcats::fct_relevel(as.factor(FERTILISATION), "MINERAL_FERTILISER"),
      RESIDUE = forcats::fct_relevel(as.factor(RESIDUE), "REMOVED")
    )
  }
  data
}

#' Split GLOBAL_SUB_STRIP_LOCATION into a single x-axis factor for panel A
#' (the only panel that still uses this combined open-field/VAPV x-axis --
#' panel C was redesigned to show position-specific CONTRASTS instead, see
#' fig01_panel_c()), with UNIQUE internal level keys (env_pos, e.g.
#' "O_WEST"/"V_WEST") but a SHARED display label per position
#' ("West"/"Centre"/"East", applied via scale_x_discrete(labels = ...) in
#' fig01_point_range()) -- open field and VAPV each get their own 3
#' consecutive positions, separated by a blank spacer level (a small visual
#' gap, not just the dashed divider line) and a dashed divider, with "Open
#' field"/"VAPV" added as manual text annotations ABOVE the panel (see
#' add_env_headers()) rather than a facet_wrap() strip.
#'
#' facet_wrap() was tried first but patchwork::plot_layout(guides =
#' "collect") does not merge legends across faceted panels correctly -- it
#' produces one duplicate/partial legend per faceted panel instead of one
#' shared legend. Confirmed with two independent minimal reproducible
#' examples (plain `+`/`/` combination, and the documented `guide_area()`
#' workaround) -- both still split the legend when ANY panel in the
#' composite uses facet_wrap(), even panels with an IDENTICAL colour scale.
#' A single combined x-axis with a manual divider/header avoids the bug
#' entirely and (per direct feedback on the first draft) also reads more
#' compactly than repeating "Open field"/"VAPV" in every one of 6 tick labels.
#' `POSITION`'s underlying levels stay UNIQUE ("O_WEST".."V_EAST") -- do NOT
#' collapse them to duplicate "West"/"Centre"/"East" labels via
#' `factor(..., labels = ...)`: factor() requires unique labels and SILENTLY
#' merges any level whose label repeats an earlier one, which merged all 6
#' positions down to 3 and made every VAPV point invisible in an earlier
#' version of this file (confirmed by inspecting `levels(POSITION)` after
#' the merge -- only 3 levels remained instead of 6). The duplicate-looking
#' "West/Centre/East" tick text is applied separately and safely at the
#' ggplot layer instead, via `scale_x_discrete(labels = position_tick_labels)`
#' in fig01_point_range() -- that only changes DISPLAYED text, not the
#' underlying factor levels/positions.
#' Two blank "GAP" levels sit between the open-field and VAPV halves of the
#' x-axis -- no data ever maps to them, they just reserve empty horizontal
#' space so the two groups read as clearly visually distinct (per manuscript
#' feedback requesting more separation than a single spacer gave) without
#' relying on the dashed divider line alone, and so "East" (open field) and
#' "West" (VAPV) never visually merge into each other.
position_tick_labels <- c(
  O_WEST = "West", O_MIDDLE = "Centre", O_EAST = "East", GAP1 = "", GAP2 = "",
  V_WEST = "West", V_MIDDLE = "Centre", V_EAST = "East"
)

split_global_location <- function(df) {
  env_code <- c(OUTSIDE_PV = "O", BETWEEN_PV = "V")
  df |>
    dplyr::mutate(
      env = dplyr::if_else(stringr::str_starts(as.character(GLOBAL_SUB_STRIP_LOCATION), "BETWEEN"),
                            "BETWEEN_PV", "OUTSIDE_PV"),
      pos = stringr::str_extract(as.character(GLOBAL_SUB_STRIP_LOCATION), "WEST|MIDDLE|EAST"),
      POSITION = factor(
        paste(env_code[env], pos, sep = "_"),
        levels = names(position_tick_labels)
      )
    )
}

#' Add "Open field"/"VAPV" group-header text above a combined 6-position
#' x-axis (positions 1-3 = open field, 4-6 = VAPV), and a dashed divider
#' between them. The header sits WITHIN the normal panel/data area (headroom
#' added via `scale_y_continuous(expand = ...)`), not via `y = Inf` +
#' `coord_cartesian(clip = "off")` -- that combination lets the header
#' escape the panel's own allocated space entirely once several panels are
#' stacked in a patchwork grid with a shared collected legend, so it can
#' render on top of a NEIGHBOURING panel/legend instead of its own panel
#' (confirmed empirically). Computing an in-range y position keeps the text
#' safely inside this panel no matter what else surrounds it.
#'
#' @param y_max the highest value (e.g. max of the upper CI) actually
#'   plotted in this panel, used to place the header just above it
add_env_headers <- function(p, y_max) {
  header_y <- y_max * 1.12
  p +
    ggplot2::annotate("text", x = 2, y = header_y, label = "Open field", vjust = 0, fontface = "bold", size = 3.8) +
    ggplot2::annotate("text", x = 7, y = header_y, label = "VAPV", vjust = 0, fontface = "bold", size = 3.8) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.05, 0.16))) +
    # `labels =` here only changes DISPLAYED tick text -- POSITION's
    # underlying 8 levels (6 real + 2 blank GAP spacers) stay distinct (see
    # position_tick_labels' docstring for why that distinction matters).
    # Extra `add` expansion keeps "VAPV", centred on the last 3 categories,
    # from being clipped by the right edge.
    ggplot2::scale_x_discrete(labels = position_tick_labels, expand = ggplot2::expansion(add = 1.8)) +
    # A small (20 deg, not the 30-45 deg used for date axes) rotation --
    # tried without rotation first at the manuscript's mandated 8.0in figure
    # width and confirmed the 8-category "West/Centre/East x2" labels
    # crowd/touch at that width with 5 dodged crop points per category; a
    # small tilt was the only remaining lever once width was already at the
    # brief's specified maximum and font size reduction was explicitly
    # ruled out -- prioritises the brief's "no overlapping x-axis labels"
    # requirement over its (softer) "avoid rotation" preference.
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1, vjust = 1))
}

fig01_fit_models <- function(data) {
  primary <- fig01_relevel(filter_primary_product(data))
  primary_mgmt <- fig01_relevel(filter_primary_product(data), management = TRUE)
  quality_data <- dplyr::filter(primary, !is.na(N_TOTAL))
  list(
    yield_substrip   = fit_yield_substrip_model(primary),
    yield_pooled     = fit_yield_pooled_model(primary),
    quality_substrip = fit_quality_substrip_model(quality_data),
    quality_pooled   = fit_quality_pooled_model(quality_data),
    yield_management = fit_yield_management_model(primary_mgmt)
  )
}

#' Shared point-range builder for panel A: x = combined position category
#' (with a blank spacer level between the two environments -- see
#' position_tick_labels' docstring), colour + shape = CROP (double-encoded),
#' dashed divider + manual environment headers.
fig01_point_range <- function(emm_df, y_lab, title, dodge_width = 0.6) {
  y_max <- max(emm_df$ucl, na.rm = TRUE)
  p <- plot_emm_point_range(
    emm_df, x = "POSITION", y_lab = y_lab, colour = "CROP", shape = "CROP",
    colour_values = crop_colors[study2_crop_order], shape_values = crop_shapes[study2_crop_order],
    dodge_width = dodge_width, point_size = MS_POINT_SIZE, title = title
  ) +
    ggplot2::labs(colour = "Crop", shape = "Crop") +
    ggplot2::geom_vline(xintercept = 4.5, linetype = "dashed", linewidth = MS_REF_LINEWIDTH, colour = "grey60")
  add_env_headers(p, y_max)
}

#' Panel A: yield EMMs by row position, open field vs VAPV, coloured/shaped by crop.
fig01_panel_a <- function(fit) {
  emm_df <- get_emmeans(fit, ~ GLOBAL_SUB_STRIP_LOCATION * CROP) |> split_global_location()
  fig01_point_range(emm_df, y_lab = "Yield (Mg DM ha⁻¹)", title = "Yield by row position")
}

#' Panel B: overall VAPV effect on yield -- % response forest by crop (the
#' inferential counterpart to panel A, same underlying model
#' (fit_yield_pooled_model) and LOCATION x CROP interaction, displayed as a
#' contrast rather than as two separate group means).
fig01_panel_b <- function(fit) {
  resp <- calc_vapv_response(fit, ~ LOCATION | CROP, by = "CROP") |>
    dplyr::mutate(CROP = factor(CROP, levels = study2_crop_order)) |>
    dplyr::arrange(CROP)
  plot_effect_forest(
    resp, label_col = "CROP", estimate_col = "pct_response", lcl_col = "pct_lcl", ucl_col = "pct_ucl",
    x_lab = "Yield response to VAPV (%)", ref_line = 0,
    colour = "CROP", colour_values = crop_colors[study2_crop_order],
    shape = "CROP", shape_values = crop_shapes[study2_crop_order],
    title = "Overall VAPV effect on yield"
  ) +
    ggplot2::labs(colour = "Crop", shape = "Crop")
}

#' Panel C: substrip-specific VAPV effect on tissue N -- VAPV minus open
#' field, in tissue-N PERCENTAGE POINTS (fit_quality_substrip_model() is
#' gaussian/identity-link, so a plain difference is the correct unit, same
#' convention as panel D), estimated separately for each row position
#' (west/centre/east). The inferential complement to panel D: D pools
#' across position but keeps crop; C pools across crop but keeps position.
#'
#' LIMITATION (deliberately not worked around): a crop-specific version of
#' this contrast was requested (one line per crop, dodged within each
#' position) but is NOT supported by the fitted model.
#' fit_quality_substrip_model()'s formula is
#' `N_TOTAL ~ GLOBAL_SUB_STRIP_LOCATION + CROP` -- main effects only, no
#' GLOBAL_SUB_STRIP_LOCATION:CROP interaction, because that 18-cell
#' interaction did not converge on 204 rows (see that function's docstring).
#' Without the interaction term, "the VAPV effect at West" is mathematically
#' identical for every crop in this model (crop only shifts the intercept),
#' so plotting 3 crop-coloured points per position would just be the SAME
#' number drawn 3 times, dressed up to look like independent per-crop
#' estimates -- exactly the kind of invented contrast the manuscript brief
#' says not to fabricate. This panel therefore shows the POOLED-ACROSS-CROP
#' position effect only, with no crop legend, and the caption states this
#' explicitly.
#'
#' Uses CUSTOM contrasts (not a simple `revpairwise` by a factor level,
#' since GLOBAL_SUB_STRIP_LOCATION is a single 6-level factor rather than a
#' crossed location*position design) picking out exactly the 3
#' VAPV-vs-open-field differences at matching positions. Level order matches
#' fig01_relevel()'s explicit releveling: 1 OUTSIDE_PV_WEST,
#' 2 OUTSIDE_PV_MIDDLE, 3 OUTSIDE_PV_EAST, 4 BETWEEN_PV_WEST,
#' 5 BETWEEN_PV_MIDDLE, 6 BETWEEN_PV_EAST -- verified via
#' `levels(emmeans::emmeans(fit, ~ GLOBAL_SUB_STRIP_LOCATION))` before
#' hardcoding these coefficient vectors.
fig01_panel_c <- function(fit) {
  emm <- emmeans::emmeans(fit, ~ GLOBAL_SUB_STRIP_LOCATION)
  contr <- emmeans::contrast(emm, method = list(
    "West"   = c(-1, 0, 0, 1, 0, 0),
    "Centre" = c(0, -1, 0, 0, 1, 0),
    "East"   = c(0, 0, -1, 0, 0, 1)
  ))
  contr_df <- as.data.frame(stats::confint(contr))
  nm <- emm_col_names(contr_df)
  contr_df$estimate <- contr_df[[nm$est]]
  contr_df$lcl <- contr_df[[nm$lcl]]
  contr_df$ucl <- contr_df[[nm$ucl]]
  contr_df$POSITION <- factor(contr_df$contrast, levels = c("West", "Centre", "East"))

  plot_emm_point_range(
    contr_df, x = "POSITION", y_lab = "VAPV effect on tissue N\n(percentage points)",
    point_size = MS_POINT_SIZE, title = "Spatial VAPV effect on tissue N"
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = MS_REF_LINEWIDTH, colour = "grey50") +
    ggplot2::labs(x = NULL)
}

#' Panel D: overall VAPV effect on tissue N -- percentage-POINT difference
#' forest by crop (NOT a % response: fit_quality_pooled_model() is gaussian/
#' identity-link, so calc_vapv_response()'s ratio transform does not apply;
#' an absolute pp difference is also the natural unit for a concentration
#' already on a comparable 1.5-6.5% scale across crops, unlike yield's very
#' different per-crop magnitudes in panel B).
fig01_panel_d <- function(fit) {
  resp <- get_pairwise_contrasts(fit, ~ LOCATION | CROP, method = "revpairwise", by = "CROP") |>
    dplyr::mutate(CROP = factor(CROP, levels = intersect(study2_crop_order, unique(CROP)))) |>
    dplyr::arrange(CROP)
  plot_effect_forest(
    resp, label_col = "CROP", estimate_col = "estimate", lcl_col = "lcl", ucl_col = "ucl",
    x_lab = "VAPV effect on tissue N (percentage points)", ref_line = 0,
    colour = "CROP", colour_values = crop_colors[study2_crop_order],
    shape = "CROP", shape_values = crop_shapes[study2_crop_order],
    title = "Overall VAPV effect on tissue N"
  ) +
    ggplot2::labs(colour = "Crop", shape = "Crop")
}

#' Panel E: agronomic management effects on yield, PER CROP -- fertiliser
#' (Digestate vs Mineral) and residue (Retained vs Removed) contrasts,
#' faceted side by side, each with its own zero line. Model-derived %
#' response (same log-link ratio transform and formula as panel B --
#'   Digestate vs mineral (%)  = 100 x (Digestate  - Mineral) / Mineral
#'   Retained vs removed (%)   = 100 x (Retained   - Removed) / Removed
#' both computed as a ratio on the model's log-link scale, then expressed as
#' a percentage -- see calc_vapv_response()/calc_response_contrast() in
#' functions_manuscript_figures.R), from fit_yield_management_model()'s
#' FERTILISATION:CROP / RESIDUE:CROP terms.
fig01_panel_e <- function(fit) {
  # 3-line facet labels (not 2): at this manuscript's mandated 8.0in figure
  # width, Panel E's 2 side-by-side facets each get only ~2in, and
  # "Digestate - Mineral"/"Retained - Removed" as a single second line
  # truncated at that width (confirmed by rendering). Splitting the
  # dash-contrast onto its own short line avoids truncation without
  # reducing strip font size (explicitly ruled out by the brief) or
  # exceeding the specified width cap.
  fert <- calc_vapv_response(fit, ~ FERTILISATION | CROP, by = "CROP", method = "revpairwise") |>
    dplyr::mutate(panel = "Fertiliser\nDigestate −\nMineral")
  resid <- calc_vapv_response(fit, ~ RESIDUE | CROP, by = "CROP", method = "revpairwise") |>
    dplyr::mutate(panel = "Residue\nRetained −\nRemoved")

  resp <- dplyr::bind_rows(fert, resid) |>
    dplyr::mutate(
      CROP = factor(CROP, levels = study2_crop_order),
      panel = factor(panel, levels = c("Fertiliser\nDigestate −\nMineral", "Residue\nRetained −\nRemoved"))
    ) |>
    dplyr::arrange(panel, CROP)

  plot_effect_forest(
    resp, label_col = "CROP", estimate_col = "pct_response", lcl_col = "pct_lcl", ucl_col = "pct_ucl",
    x_lab = "Yield response (%)", ref_line = 0, facet = "panel",
    colour = "CROP", colour_values = crop_colors[study2_crop_order],
    shape = "CROP", shape_values = crop_shapes[study2_crop_order],
    title = "Management effects on yield"
  ) +
    ggplot2::labs(colour = "Crop", shape = "Crop")
}

#' Panel F: does VAPV modify the (pooled, across-crop) management response?
#' This is a genuine DIFFERENCE-IN-DIFFERENCES / interaction contrast, NOT a
#' main effect:
#'
#'   interaction = (management response under VAPV) - (management response under open field)
#'
#'   i.e. for fertiliser: [ (Digestate - Mineral) | VAPV ]  -  [ (Digestate - Mineral) | open field ]
#'        for residue:    [ (Retained  - Removed) | VAPV ]  -  [ (Retained  - Removed) | open field ]
#'
#' computed on the model's log-link scale as a ratio-of-ratios then expressed
#' as a percentage deviation from 1 (see calc_interaction_response(),
#' functions_manuscript_figures.R). 0% = the management effect is the same
#' size under VAPV and open field, i.e. no evidence VAPV changes how the
#' crop responds to that management practice. Pooled across crop rather
#' than per-crop -- see fit_yield_management_model()'s docstring for why a
#' per-crop version of this interaction was not attempted (sparse cells for
#' SY/SB). Colour is a single neutral dark tone (ggplot's default black),
#' not crop or environment: these are management-level contrasts, not
#' crop-level ones, so the crop palette does not apply here.
fig01_panel_f <- function(fit) {
  fert_int <- calc_interaction_response(fit, c("LOCATION", "FERTILISATION")) |>
    dplyr::mutate(label = "VAPV × fertiliser\n(Digestate − Mineral)")
  resid_int <- calc_interaction_response(fit, c("LOCATION", "RESIDUE")) |>
    dplyr::mutate(label = "VAPV × residue\n(Retained − Removed)")

  resp <- dplyr::bind_rows(fert_int, resid_int) |>
    dplyr::mutate(label = forcats::fct_inorder(label))

  plot_effect_forest(
    resp, label_col = "label", estimate_col = "pct_response", lcl_col = "pct_lcl", ucl_col = "pct_ucl",
    x_lab = "Change in management response\nunder VAPV (%)", ref_line = 0,
    title = "VAPV × management interactions"
  )
}

#' Fit the 5 Figure-1 candidate models and build all 6 panels from them,
#' WITHOUT assembling the composite -- split out from assemble_fig01() so a
#' caller (notably 02_yield_and_quality_models.qmd) can fit each model and
#' inspect/display each panel individually before compositing, rather than
#' the whole figure being built opaquely inside one function call.
#' `assemble_fig01()` below is a thin wrapper: `fig01_build_panels()` +
#' `fig01_assemble_composite()`, and remains the one `_targets.R` calls.
fig01_build_panels <- function(data) {
  fits <- fig01_fit_models(data)

  list(
    fits = fits,
    panel_a = fig01_panel_a(fits$yield_substrip),
    panel_b = fig01_panel_b(fits$yield_pooled),
    panel_c = fig01_panel_c(fits$quality_substrip),
    panel_d = fig01_panel_d(fits$quality_pooled),
    panel_e = fig01_panel_e(fits$yield_management),
    panel_f = fig01_panel_f(fits$yield_management)
  )
}

#' Assemble the 6 panels built by fig01_build_panels() into the Figure 1
#' composite: one shared CROP legend in a dedicated top strip
#' (patchwork::guide_area()), tight tag/title spacing, and reduced
#' inter-row whitespace.
#'
#' patchwork::plot_layout(guides = "collect") only MERGES guides it judges
#' structurally identical -- and a point-range plot's colour/shape legend key
#' (built from geom_point + a VERTICAL geom_errorbar) and a forest plot's
#' (geom_point + a HORIZONTAL geom_errorbarh) render different key glyphs
#' even with byte-identical scale breaks/labels/colours, so "collect" treats
#' them as different guides and places one copy per panel instead of merging
#' (confirmed empirically across several attempted fixes: matching scale
#' limits, matching legend titles, adding shape to every panel -- none of it
#' changed this specific mismatch). Because A-E all use the SAME crop
#' encoding, only ONE legend needs to be visible at all: panel A keeps its
#' legend, B-E have theirs turned off (F is pooled across crop and never had
#' one), and `guide_area()` + `design` pulls A's single legend into a
#' dedicated top strip spanning the full width -- this sidesteps "collect"'s
#' merge logic entirely (nothing to merge, only one guide exists to place).
fig01_assemble_composite <- function(panels) {
  pa <- panels$panel_a
  pb <- panels$panel_b + ggplot2::theme(legend.position = "none")
  pc <- panels$panel_c + ggplot2::theme(legend.position = "none")
  pd <- panels$panel_d + ggplot2::theme(legend.position = "none")
  pe <- panels$panel_e + ggplot2::theme(legend.position = "none")
  pf <- panels$panel_f  # no crop legend (pooled, not per-crop)

  layout <- "
    LLLL
    AABB
    AABB
    CCDD
    CCDD
    EEFF
    EEFF
  "
  # Column widths 1.05:1.00 (left:right, applied per grid column -- the
  # design string has 4 grid columns, A/C/E each span the first 2 (so their
  # combined width is 2.1 units) and B/D/F the last 2 (2.0 units)) --
  # per manuscript feedback, the left column carries the wider substrip
  # x-axis (A) and needed a touch more room; giving it slightly more space
  # throughout keeps every row visually aligned.
  (pa + pb + pc + pd + pe + pf + patchwork::guide_area()) +
    patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(
      design = layout, guides = "collect",
      heights = c(0.10, 1, 1, 1, 1, 1, 1), widths = c(1.05, 1.05, 1, 1)
    ) &
    # NOTE: legend.position is deliberately NOT set here -- broadcasting it
    # via `&` would re-enable the legends just turned off on pb/pc/pd/pe above,
    # defeating the single-visible-legend approach this function relies on.
    manuscript_composite_theme()
}

#' End-to-end Figure 1 build: fit models, build all 6 panels, assemble the
#' composite. This is what `_targets.R`'s `fig01_result` target calls; the
#' .qmd walkthrough instead calls fig01_build_panels() + individual panel
#' display + fig01_assemble_composite() directly so each subfigure is
#' visibly constructed before the final composite.
assemble_fig01 <- function(data) {
  panels <- fig01_build_panels(data)
  fits <- panels$fits
  composite <- fig01_assemble_composite(panels)

  extract_result <- function(fit) list(
    formula = paste(deparse(stats::formula(fit)), collapse = " "),
    n_obs = stats::nobs(fit),
    AIC = stats::AIC(fit),
    converged = isTRUE(fit$sdr$pdHess),
    anova = as.data.frame(anova_glmm(fit))
  )

  list(
    yield_substrip = extract_result(fits$yield_substrip),
    yield_pooled = extract_result(fits$yield_pooled),
    quality_substrip = extract_result(fits$quality_substrip),
    quality_pooled = extract_result(fits$quality_pooled),
    yield_management = extract_result(fits$yield_management),
    panel_a = panels$panel_a, panel_b = panels$panel_b, panel_c = panels$panel_c,
    panel_d = panels$panel_d, panel_e = panels$panel_e, panel_f = panels$panel_f,
    composite = composite
  )
}

#' Omnibus (Type II Wald chi-square) test table for every Figure 1 model --
#' the formal-test counterpart to the figure's effect-size panels ("Figures =
#' effect sizes + uncertainty; Table = omnibus/formal statistical tests" per
#' manuscript brief). Reuses the `anova` data frame ALREADY computed inside
#' assemble_fig01() (itself `car::Anova(fit, type = "II")` via anova_glmm(),
#' functions_statistics.R) -- no new statistical test is run here, this only
#' reshapes results that already exist into one tidy table.
#'
#' @param fig01_result the list returned by assemble_fig01()
fig01_omnibus_table <- function(fig01_result) {
  response_map <- c(
    yield_substrip = "Yield", yield_pooled = "Yield", yield_management = "Yield",
    quality_substrip = "Tissue N", quality_pooled = "Tissue N"
  )
  model_label <- c(
    yield_substrip = "Yield: substrip model", yield_pooled = "Yield: pooled model",
    yield_management = "Yield: management model",
    quality_substrip = "Tissue N: substrip model", quality_pooled = "Tissue N: pooled model"
  )
  rows <- lapply(names(response_map), function(nm) {
    aov <- fig01_result[[nm]]$anova
    if (is.null(aov) || !"Df" %in% names(aov)) return(NULL)
    tibble::rownames_to_column(aov, var = "Term") |>
      dplyr::transmute(
        Response = response_map[[nm]], Model = model_label[[nm]], Term,
        Df = Df, Chisq = Chisq, P = `Pr(>Chisq)`
      )
  })
  dplyr::bind_rows(rows)
}

fig01_caption <- function() {
  paste(
    "**Figure 1. Crop productivity, tissue N and agronomic management response to vertical agrivoltaics",
    "(VAPV).** For each crop's primary harvestable product (WW/SY: grain; GC/RG/SB: biomass). Crop is",
    "encoded redundantly by colour AND point/line shape wherever a per-crop estimate is shown (A, B, D, E).",
    "Panel A shows estimated marginal means (EMMs) with 95% confidence intervals from the fitted model --",
    "NOT raw arithmetic means. Panels B-F show model-derived CONTRASTS (differences or percentage",
    "responses) with 95% confidence intervals; a dashed reference line at 0 (0% for a percentage response)",
    "marks no effect. No significance stars are used anywhere -- whether a CI crosses the zero/no-effect",
    "line already conveys the inferential result.",
    "",
    "(A) Yield (Mg DM ha⁻¹; EMM ± 95% CI) by row position (west/centre/east), shown separately for",
    "open field and VAPV (a blank spacer plus a dashed line divides the two; header text labels which",
    "half is which).",
    "(B) Overall VAPV effect on yield: 100 x (VAPV − open field) / open field, i.e. the model-derived",
    "percentage response (computed as a ratio on the model's log-link scale, then expressed as %) with",
    "95% CI, one row per crop. The inferential counterpart of (A) from the same model",
    "(fit_yield_pooled_model).",
    "(C) Spatial VAPV effect on tissue N (percentage points, VAPV minus open field), by row",
    "position (west/centre/east). POOLED ACROSS CROP, not shown per crop: the underlying model",
    "(fit_quality_substrip_model) is `N_TOTAL ~ GLOBAL_SUB_STRIP_LOCATION + CROP`, main effects only --",
    "an 18-cell row-position x crop interaction did not converge on these 204 rows (see that function's",
    "docstring) and was not forced. Without that interaction, a crop-specific version of this contrast",
    "would just repeat the same pooled number for every crop, so it is not shown. (C) and (D) are",
    "complementary marginal views of the same VAPV-on-quality question: (C) varies row position and pools",
    "crop; (D) varies crop and pools row position -- neither model supports both at once.",
    "(D) Overall VAPV effect on tissue N: absolute difference (percentage POINTS, VAPV minus open field,",
    "not a percentage response) with 95% CI, one row per crop, pooled across row position -- an absolute",
    "difference is reported here (rather than (B)'s relative response) because the underlying model",
    "(fit_quality_pooled_model) is gaussian (identity link, so no ratio scale applies) and because tissue N",
    "already sits on a comparable 1.5-6.5% scale across crops, unlike yield's very different per-crop",
    "magnitudes in (B). Tissue N data are 2024 season only, and only for the 3 crops with any N_TOTAL data",
    "(winter wheat, soybean, grass-clover -- ryegrass and spring barley have none); this applies to (C) too.",
    "(E) Management effects on yield, per crop: percentage response to fertiliser source (digestate vs",
    "mineral) and residue management (retained vs removed), from fit_yield_management_model()'s",
    "FERTILISATION:CROP and RESIDUE:CROP terms -- same % response definition as (B).",
    "(F) VAPV x management interactions: does VAPV change the (pooled, across-crop) management response?",
    "A genuine DIFFERENCE-IN-DIFFERENCES / interaction contrast, not a main effect: how much the fertiliser",
    "(digestate vs mineral) and residue (retained vs removed) effects on yield differ between VAPV and",
    "open field, expressed as a percentage (a ratio-of-ratios on the log scale). 0% = the management effect",
    "is the same size under VAPV and open field; a CI crossing zero means limited evidence that VAPV",
    "modifies that management response. Pooled across crop rather than per-crop: a per-crop version of",
    "this 3-way interaction (location x management x crop) was not fit -- with only 2 x 2 x 5 = 20 cells",
    "over ~444 rows, low-replication crops (soybean: 24 rows, spring barley: 30 rows) would average under",
    "2 rows per cell, well below what the interaction could reliably estimate. Point/line colour is a",
    "single neutral dark tone here (not the crop palette), since these are management-level, not",
    "crop-level, contrasts.",
    "",
    "Models: yield models (A/B/E/F) are Gamma(log) with row/column dispersion",
    "(dispformula = ~ ROW + COLUMN); tissue-N models (C/D) are gaussian with no dispersion/spatial term",
    "(too sparse for one). Reference level for every VAPV contrast is open field (OUTSIDE_PV); for",
    "fertiliser, mineral fertiliser; for residue, removed.",
    "",
    "Percentage-response definitions (all computed on the model's log-link scale as a ratio, then",
    "expressed as a percentage -- not a ratio of raw arithmetic means):",
    "  Yield response to VAPV (%)   = 100 x (VAPV - Open field) / Open field           [panel B]",
    "  Digestate vs mineral (%)     = 100 x (Digestate - Mineral) / Mineral            [panel E]",
    "  Retained vs removed (%)     = 100 x (Retained - Removed) / Removed             [panel E]",
    "  VAPV x management (%)        = [management response | VAPV] - [management response | open field],",
    "                                  as a ratio-of-ratios expressed the same way              [panel F]",
    sep = "\n"
  )
}
