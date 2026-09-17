# Shared building blocks for the 6 publication-ready manuscript figures
# (main_figures/) and their supporting review/supplementary outputs. Builds
# on plot_theme.R (theme_study2()) and utils.R (palettes, save_report_figure
# pattern) without duplicating them -- this file adds only what those two
# didn't already provide: consistent EMM extraction across the two
# emmeans column-naming conventions already documented in
# plot_emmeans_effect() (functions_statistics.R), model-derived response
# ratios (not raw-mean ratios), generic point-range/forest-plot builders,
# and the manuscript figure-export convention.
#
# Design contract (shared across all 6 main figures):
#   - model estimates + 95% CI carry the inferential message (Midway (2020)
#     Principle #7: model output vs raw data are visually distinguished)
#   - one fixed palette per treatment factor, reused across every figure
#     (see study2_crop_order / pv_labels below; colours themselves are
#     utils.R's already-validated fertilisation_colors/residue_colors/
#     location_simple_colors/crop_colors -- never redefined here)
#   - panel labels A, B, C... via patchwork::plot_annotation(tag_levels="A")

# ---- Fixed ordering / display labels ----------------------------------------

#' Fixed crop ordering for every manuscript figure/table (crop_colors' keys,
#' in the paper's preferred narrative order: wheat, soybean, grass-clover,
#' then the small-grain/ryegrass pair). Adapt only if a crop is absent from
#' the actual dataset -- callers should intersect with `unique(data$CROP)`
#' rather than assuming all 5 are always present.
study2_crop_order <- c("WW", "SY", "GC", "SB", "RG")

#' Display labels for the 2-level PV/location factor (BETWEEN_PV/OUTSIDE_PV
#' in the yield & TDR branches, lowercase location in the NO3 branch) --
#' "VAPV" / "Open field" is the manuscript-facing terminology; the
#' underlying factor levels and utils.R's location_simple_colors keys are
#' left untouched so this is purely a label layer.
pv_location_labels <- c(
  "BETWEEN_PV" = "VAPV",
  "OUTSIDE_PV" = "Open field"
)

#' Case-insensitive lookup into pv_location_labels (the NO3 model uses
#' lowercase `location` levels "between_pv"/"outside_pv").
relabel_pv_location <- function(x) {
  x_up <- toupper(as.character(x))
  out <- pv_location_labels[x_up]
  dplyr::coalesce(unname(out), as.character(x))
}

#' Display labels for FERTILISATION / RESIDUE, matching the manuscript's
#' terminology exactly (shorten_fertilisation()/shorten_residue() in utils.R
#' produce compact axis codes instead -- these are the full-word versions for
#' legend titles and facet strips where space allows).
fertilisation_labels <- c("BIOGAS_DIGESTATE" = "Digestate", "MINERAL_FERTILISER" = "Mineral")
residue_labels       <- c("REMOVED" = "Removed", "REMAIN" = "Retained")

# ---- EMM extraction (handles both emmeans column-naming conventions) -------

#' Return the (estimate, lower CI, upper CI) column names actually present in
#' an emmeans-derived data frame, matching plot_emmeans_effect()'s existing
#' detection logic (functions_statistics.R) so every new helper here is
#' consistent with the emmeans plot already in the yield/quality .qmd files.
#' Extended beyond that function to also cover CONTRAST output: a back-
#' transformed (`type = "response"`) contrast on a log-link model is a
#' RATIO, and emmeans names that column "ratio" (or "odds.ratio" for
#' binomial models), not "response" -- confirmed empirically (a
#' `response`-only check silently produced a 0-length lookup and a cryptic
#' "replacement has 0 rows" error from downstream `df$pct_response <- ...`).
emm_col_names <- function(df) {
  est <- if ("response" %in% names(df)) "response"
    else if ("ratio" %in% names(df)) "ratio"
    else if ("odds.ratio" %in% names(df)) "odds.ratio"
    else if ("estimate" %in% names(df)) "estimate"
    else "emmean"
  lcl <- if ("asymp.LCL" %in% names(df)) "asymp.LCL" else if ("lower.CL" %in% names(df)) "lower.CL" else "LCL"
  ucl <- if ("asymp.UCL" %in% names(df)) "asymp.UCL" else if ("upper.CL" %in% names(df)) "upper.CL" else "UCL"
  list(est = est, lcl = lcl, ucl = ucl)
}

#' Estimated marginal means for `spec` from a fitted glmmTMB model, as a tidy
#' data frame with standardised `estimate`/`lcl`/`ucl` columns added
#' (originals kept too, so outcome-specific code can still see
#' `response`/`emmean` if needed).
#'
#' @param model a fitted glmmTMB model (or any model emmeans supports)
#' @param spec an emmeans specification, e.g. `~ LOCATION * CROP_PRODUCT`
#' @param type "response" (back-transformed, default) or "link"
#' @param ... passed to emmeans::emmeans()
get_emmeans <- function(model, spec, type = "response", ...) {
  emm_df <- as.data.frame(emmeans::emmeans(model, spec, type = type, ...))
  nm <- emm_col_names(emm_df)
  emm_df$estimate <- emm_df[[nm$est]]
  emm_df$lcl <- emm_df[[nm$lcl]]
  emm_df$ucl <- emm_df[[nm$ucl]]
  emm_df
}

#' Pairwise (or custom-method) contrasts for `spec`, as a tidy data frame
#' with `estimate`/`lcl`/`ucl`/`p.value`. Confidence intervals come from
#' `emmeans::confint()` (not shown by default `contrast()` alone).
#'
#' @param method e.g. "pairwise", "revpairwise", or a custom list of contrast
#'   coefficients
#' @param by optional grouping column(s) to compute contrasts within
#' @param type "response" (back-transform to a ratio, log-link models only),
#'   "link" (untransformed difference scale), or NULL (default) to
#'   auto-detect from the model's link -- "response" for a log-link model
#'   (so the contrast reads as a ratio), "link" otherwise. Auto-detection
#'   matters because `confint(..., type = "response")` on an
#'   identity-link (gaussian) model's contrast silently drops the numeric
#'   estimate column entirely rather than erroring -- confirmed empirically
#'   while building the Figure 3 (SOC, gaussian) forest plot.
get_pairwise_contrasts <- function(model, spec, method = "revpairwise", by = NULL,
                                    type = NULL, adjust = "tukey", ...) {
  if (is.null(type)) {
    link <- tryCatch(stats::family(model)$link, error = function(e) NA_character_)
    type <- if (isTRUE(link %in% c("log", "logit"))) "response" else "link"
  }
  emm <- emmeans::emmeans(model, spec, by = by, ...)
  contr <- emmeans::contrast(emm, method = method, adjust = adjust)
  contr_df <- as.data.frame(stats::confint(contr, type = type))
  nm <- emm_col_names(contr_df)
  contr_df$estimate <- contr_df[[nm$est]]
  contr_df$lcl <- contr_df[[nm$lcl]]
  contr_df$ucl <- contr_df[[nm$ucl]]
  contr_df
}

#' Model-derived percentage response, `100 * (treatment / reference - 1)`,
#' computed correctly for log-link models: contrasts are taken on the LINK
#' scale (so the CI back-transforms symmetrically as a ratio) and then
#' converted to a percentage, rather than taking a ratio of already
#' back-transformed point estimates (which is what "ratio of raw arithmetic
#' means" language in the brief warns against -- this is the model-based
#' equivalent). For a non-log-link (identity) model, falls back to a
#' percentage-of-reference computed from the difference contrast, which is
#' only approximate and flagged via the `link` attribute on the return value.
#'
#' @param model fitted glmmTMB model
#' @param spec emmeans spec whose factor's FIRST two levels are (in order)
#'   reference, treatment -- e.g. `~ LOCATION`, with LOCATION releveled so
#'   "OUTSIDE_PV" (open field) is the reference
#' @param by optional grouping column(s), e.g. "CROP_PRODUCT"
#' Generic model-derived percentage-response contrast (the engine behind
#' calc_vapv_response() below, but usable for any main-effect factor, e.g.
#' fertilisation or residue, not just PV location).
calc_response_contrast <- function(model, spec, by = NULL, method = "revpairwise") {
  link <- tryCatch(stats::family(model)$link, error = function(e) NA_character_)
  is_log_link <- isTRUE(link == "log")

  contr_df <- get_pairwise_contrasts(model, spec, method = method, by = by)
  if (is_log_link) {
    # emmeans back-transforms a log-link contrast to a RATIO automatically
    # when type = "response" is requested at confint() time -- estimate/lcl/ucl
    # above are therefore already ratios; convert to percentage response.
    contr_df$pct_response <- 100 * (contr_df$estimate - 1)
    contr_df$pct_lcl <- 100 * (contr_df$lcl - 1)
    contr_df$pct_ucl <- 100 * (contr_df$ucl - 1)
  } else {
    contr_df$pct_response <- NA_real_
    contr_df$pct_lcl <- NA_real_
    contr_df$pct_ucl <- NA_real_
  }
  attr(contr_df, "link") <- link %||% "identity"
  contr_df
}

#' Model-derived percentage response, `100 * (treatment / reference - 1)`,
#' computed correctly for log-link models: contrasts are taken on the LINK
#' scale (so the CI back-transforms symmetrically as a ratio) and then
#' converted to a percentage, rather than taking a ratio of already
#' back-transformed point estimates (which is what "ratio of raw arithmetic
#' means" language in the brief warns against -- this is the model-based
#' equivalent). For a non-log-link (identity) model, pct_response is left NA
#' (flagged via the `link` attribute) rather than silently misrepresenting a
#' difference as a percentage.
#'
#' @param model fitted glmmTMB model
#' @param spec emmeans spec whose factor's FIRST two levels are (in order)
#'   reference, treatment -- e.g. `~ LOCATION`, with LOCATION releveled so
#'   "OUTSIDE_PV" (open field) is the reference
#' @param by optional grouping column(s), e.g. "CROP_PRODUCT"
calc_vapv_response <- function(model, spec, by = NULL, method = "revpairwise") {
  calc_response_contrast(model, spec, by = by, method = method)
}

#' Two-way interaction contrast ("ratio of ratios" for a log-link model),
#' e.g. does the PV effect differ between fertiliser sources? Returns a
#' single-row (or per-`by`-group) tidy data frame with the same
#' estimate/lcl/ucl/pct_response columns as calc_response_contrast().
#'
#' @param spec_vars character vector of exactly 2 factor names, e.g.
#'   c("LOCATION", "FERTILISATION")
calc_interaction_response <- function(model, spec_vars, by = NULL, interaction_method = "revpairwise") {
  stopifnot(length(spec_vars) == 2)
  link <- tryCatch(stats::family(model)$link, error = function(e) NA_character_)
  is_log_link <- isTRUE(link == "log")

  type <- if (is_log_link) "response" else "link"
  emm <- emmeans::emmeans(model, stats::reformulate(paste(spec_vars, collapse = " * ")), by = by)
  intc <- emmeans::contrast(emm, interaction = interaction_method)
  df <- as.data.frame(stats::confint(intc, type = type))
  nm <- emm_col_names(df)
  df$estimate <- df[[nm$est]]
  df$lcl <- df[[nm$lcl]]
  df$ucl <- df[[nm$ucl]]
  if (is_log_link) {
    df$pct_response <- 100 * (df$estimate - 1)
    df$pct_lcl <- 100 * (df$lcl - 1)
    df$pct_ucl <- 100 * (df$ucl - 1)
  } else {
    df$pct_response <- NA_real_; df$pct_lcl <- NA_real_; df$pct_ucl <- NA_real_
  }
  attr(df, "link") <- link %||% "identity"
  df
}

# ---- Generic plot builders ---------------------------------------------------

#' Generic estimated-marginal-means point-range plot: point = model
#' estimate, error bar = 95% CI. Optional raw-data points can be drawn
#' faintly behind the estimates (Panel design note #4.1: "raw observations
#' may be shown faintly ... but model estimates should carry the inferential
#' message").
#'
#' @param emm_df output of get_emmeans()
#' @param x,colour,facet column names (strings) in emm_df
#' @param colour_values named colour vector (utils.R palette) or NULL
#' @param shape column name (string) to map to point shape, redundant with
#'   `colour` so category identity does not rely on colour alone
#'   (colour-blind/grayscale accessibility) -- typically the same column as
#'   `colour`. NULL disables shape mapping.
#' @param shape_values named shape (pch) vector (e.g. utils.R's crop_shapes)
#' @param raw_data optional raw observation data frame for jittered points
#'   underlay; must share the x/colour/facet columns (by name) plus `y_raw`
#' @param y_raw column name of the raw outcome in `raw_data`
#' @param title short panel title (plot.title, NOT subtitle) -- kept short
#'   (a few words) by convention; put modelling detail in the figure caption
#'   instead of a long in-panel subtitle (a long wrapped subtitle both
#'   crowds the panel and can get clipped by the plot device -- confirmed on
#'   the first Figure 1 draft).
plot_emm_point_range <- function(emm_df, x, y_lab, colour = NULL, facet = NULL,
                                  colour_values = NULL, shape = NULL, shape_values = NULL,
                                  dodge_width = 0.5, raw_data = NULL, y_raw = NULL,
                                  point_size = MS_POINT_SIZE, title = NULL) {
  x_sym <- rlang::sym(x)
  map <- ggplot2::aes(x = !!x_sym, y = estimate)
  if (!is.null(colour)) {
    colour_sym <- rlang::sym(colour)
    map <- utils::modifyList(map, ggplot2::aes(colour = !!colour_sym, group = !!colour_sym))
  }
  if (!is.null(shape)) {
    shape_sym <- rlang::sym(shape)
    map <- utils::modifyList(map, ggplot2::aes(shape = !!shape_sym))
  }

  p <- ggplot2::ggplot(emm_df, map)

  if (!is.null(raw_data) && !is.null(y_raw)) {
    y_raw_sym <- rlang::sym(y_raw)
    raw_map <- ggplot2::aes(x = !!x_sym, y = !!y_raw_sym)
    if (!is.null(colour)) {
      colour_sym <- rlang::sym(colour)
      raw_map <- utils::modifyList(raw_map, ggplot2::aes(colour = !!colour_sym, group = !!colour_sym))
    }
    p <- p + ggplot2::geom_jitter(
      data = raw_data, mapping = raw_map, alpha = 0.15, size = 1.2,
      position = ggplot2::position_jitterdodge(jitter.width = 0.1, dodge.width = dodge_width),
      show.legend = FALSE
    )
  }

  p <- p +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lcl, ymax = ucl), width = 0.15, linewidth = MS_CI_LINEWIDTH,
      position = ggplot2::position_dodge(width = dodge_width)
    ) +
    ggplot2::geom_point(size = point_size, position = ggplot2::position_dodge(width = dodge_width)) +
    ggplot2::labs(x = NULL, y = y_lab, title = title) +
    theme_study2()  # plot.title size/weight already standardised there

  # `limits = names(colour_values)` forces every panel using this palette to
  # declare the SAME full breaks/labels/colours, even where a given panel's
  # data only realises a subset of levels (e.g. crop_colors has 5 crops but
  # a quality-outcome panel might only have data for 3) -- without this,
  # patchwork::plot_layout(guides = "collect") treats panels with different
  # realised factor levels as genuinely DIFFERENT (non-mergeable) legends
  # and renders one per distinct level-set instead of one shared legend
  # (confirmed empirically while building Figure 1's crop-coloured panels).
  if (!is.null(colour) && !is.null(colour_values)) {
    p <- p + ggplot2::scale_colour_manual(values = colour_values, limits = names(colour_values))
  }
  if (!is.null(shape) && !is.null(shape_values)) {
    p <- p + ggplot2::scale_shape_manual(values = shape_values, limits = names(shape_values))
  }
  if (!is.null(facet)) p <- p + ggplot2::facet_wrap(stats::reformulate(facet))
  p
}

#' Generic forest plot: one row per contrast/effect, point = estimate,
#' horizontal error bar = 95% CI, vertical reference line (0 for
#' differences/percentage responses, 1 for ratios). Rows are plotted in the
#' order they appear in `df` (top to bottom) -- callers control ordering via
#' `forcats::fct_inorder()` / `fct_rev()` on `label_col` beforehand.
#'
#' @param df data frame with `label_col`, `estimate_col`, `lcl_col`, `ucl_col`.
#'   A row with NA estimate/CI is a valid, intentional way to reserve a
#'   blank category slot (e.g. visual spacing between a "main effects" and
#'   "interactions" group) -- point/errorbar geoms use `na.rm = TRUE` so
#'   that row draws nothing rather than emitting an "NA values removed"
#'   warning (see Figure 2 panel E for a caller that relies on this).
#' @param ref_line vertical reference (0 or 1)
#' @param colour,colour_values optional colour grouping (e.g. by outcome)
#' @param shape,shape_values optional shape grouping, typically the SAME
#'   column as `colour` -- see plot_emm_point_range()'s docstring. Matters
#'   beyond accessibility here: when this forest plot shares a composite
#'   with plot_emm_point_range() panels that map both colour AND shape to
#'   the same variable, giving this plot ONLY a colour guide makes
#'   patchwork::plot_layout(guides = "collect") treat the two guide sets as
#'   different (colour-only vs colour+shape) and split them into separate
#'   legends instead of merging -- confirmed empirically building Figure 1.
#' @param title short panel title (plot.title) -- see plot_emm_point_range()'s
#'   docstring for why this replaces a long in-panel subtitle
plot_effect_forest <- function(df, label_col, estimate_col = "estimate",
                                lcl_col = "lcl", ucl_col = "ucl",
                                x_lab = "Effect estimate", ref_line = 0,
                                facet = NULL, colour = NULL, colour_values = NULL,
                                shape = NULL, shape_values = NULL,
                                title = NULL) {
  label_sym <- rlang::sym(label_col)
  est_sym <- rlang::sym(estimate_col)
  lcl_sym <- rlang::sym(lcl_col)
  ucl_sym <- rlang::sym(ucl_col)

  map <- ggplot2::aes(x = !!est_sym, y = forcats::fct_rev(forcats::fct_inorder(!!label_sym)))
  if (!is.null(colour)) {
    colour_sym <- rlang::sym(colour)
    map <- utils::modifyList(map, ggplot2::aes(colour = !!colour_sym))
  }
  if (!is.null(shape)) {
    shape_sym <- rlang::sym(shape)
    map <- utils::modifyList(map, ggplot2::aes(shape = !!shape_sym))
  }

  p <- ggplot2::ggplot(df, map) +
    ggplot2::geom_vline(xintercept = ref_line, linetype = "dashed", linewidth = MS_REF_LINEWIDTH, colour = "grey50") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = !!lcl_sym, xmax = !!ucl_sym), height = 0.2,
                            linewidth = MS_CI_LINEWIDTH, na.rm = TRUE) +
    ggplot2::geom_point(size = MS_POINT_SIZE, na.rm = TRUE) +
    ggplot2::labs(x = x_lab, y = NULL, title = title) +
    theme_study2() +  # plot.title size/weight already standardised there
    ggplot2::theme(legend.position = if (is.null(colour)) "none" else "top")

  if (!is.null(colour_values)) p <- p + ggplot2::scale_colour_manual(values = colour_values, limits = names(colour_values))
  if (!is.null(shape) && !is.null(shape_values)) {
    p <- p + ggplot2::scale_shape_manual(values = shape_values, limits = names(shape_values))
  }
  if (!is.null(facet)) p <- p + ggplot2::facet_wrap(stats::reformulate(facet), scales = "free_x")
  p
}

# ---- Export convention -------------------------------------------------------

#' Standardised manuscript figure export: PDF (vector master) + 600-dpi PNG,
#' optionally SVG, into the repo's existing `4_outputs/<subdir>/` convention
#' (main_figures/ / suppl_figures/ / model_review/), sized for the
#' manuscript's intended column width.
#'
#' @param width_class "single" (3.5in), "1.5col" (5.5in), or "double" (7.5in)
#'   -- overridden by explicit `width` if supplied
save_manuscript_figure <- function(plot, filename_stem, subdir = "main_figures",
                                    width_class = c("double", "single", "1.5col"),
                                    width = NULL, height = 6, dpi = 600, svg = FALSE,
                                    base_dir = "4_outputs") {
  width_class <- match.arg(width_class)
  if (is.null(width)) {
    width <- switch(width_class, single = 3.5, `1.5col` = 5.5, double = 7.5)
  }
  out_dir <- file.path(base_dir, subdir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  pdf_path <- file.path(out_dir, paste0(filename_stem, ".pdf"))
  png_path <- file.path(out_dir, paste0(filename_stem, ".png"))
  ggplot2::ggsave(pdf_path, plot = plot, width = width, height = height, units = "in", device = grDevices::cairo_pdf)
  ggplot2::ggsave(png_path, plot = plot, width = width, height = height, units = "in", dpi = dpi)

  out_paths <- c(pdf_path, png_path)
  if (svg) {
    svg_path <- file.path(out_dir, paste0(filename_stem, ".svg"))
    ggplot2::ggsave(svg_path, plot = plot, width = width, height = height, units = "in")
    out_paths <- c(out_paths, svg_path)
  }
  invisible(normalizePath(out_paths))
}

#' Write a draft figure caption (plain Markdown) alongside the figure, in the
#' same directory, following the brief's 8-point caption checklist.
save_figure_caption <- function(caption_text, filename_stem, subdir = "main_figures", base_dir = "4_outputs") {
  out_dir <- file.path(base_dir, subdir)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(filename_stem, "_caption.md"))
  writeLines(caption_text, out_path)
  invisible(normalizePath(out_path))
}
