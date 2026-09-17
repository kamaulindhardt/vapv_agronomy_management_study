# Shared design system for every figure in this pipeline EXCEPT the 6
# "preliminary" figures (functions_yield_analysis.R's plot_yield_by_*() and
# functions_no3_leaching.R's plot_no3_*()), which keep their existing,
# already-verified look untouched.
#
# Grounded in Midway (2020) "Principles of Effective Data Visualization"
# (Patterns 1(9), 100141 -- Principle #4 "colors always mean something":
# sequential/diverging/qualitative color schemes, colorblind-safe palettes,
# grayscale-safe design) and this session's dataviz skill (fixed-order
# categorical hues, one-hue sequential ramps, validated via OKLab delta-E
# under simulated protanopia/deuteranopia/tritanopia -- see the crop_colors
# comment in utils.R for the method).
#
# Entity-color mappings that are ALREADY established and used by the
# preliminary figures (fertilisation_colors, residue_colors,
# location_simple_colors in utils.R) are deliberately reused as-is here too,
# for one consistent visual identity across the whole repo -- this module
# only adds what utils.R didn't already have: a shared theme, and validated
# palettes for the categories utils.R defined but the preliminary figures
# never actually rendered (crop_colors) or that had no palette at all
# (LER's PV_SETUP_2 levels).

#' Standardised style constants for every manuscript composite figure
#' (Fig01, Fig02, ...) -- centralised here so panel-title size, axis-text
#' size, point/CI linewidth etc. cannot silently drift apart between figures
#' as they are built and revised separately. Referenced from theme_study2(),
#' functions_manuscript_figures.R's plot_emm_point_range()/plot_effect_forest(),
#' and each fig0N_*.R's assemble function.
MS_BASE_SIZE      <- 12   # base theme font size (axis text derives from this)
MS_TITLE_SIZE     <- 11   # panel plot.title (short titles, not subtitles)
MS_TAG_SIZE       <- 12   # panel letter (A, B, C...)
MS_STRIP_SIZE     <- 10.5 # facet strip text
MS_POINT_SIZE     <- 2.8  # EMM/contrast point size
MS_CI_LINEWIDTH   <- 0.8  # error bar / CI linewidth
MS_REF_LINEWIDTH  <- 0.5  # dashed zero/reference-line linewidth
MS_GRID_COLOUR    <- "grey90"
MS_STRIP_FILL     <- "grey92"

#' Shared ggplot2 theme for every non-preliminary figure: theme_bw-based (same
#' family as the preliminary figures, for a consistent look), bold facet
#' strips, top legend, minor gridlines dropped (high data-ink ratio, Midway
#' Principle #3), light panel border. All sizes come from the MS_* constants
#' above so every manuscript figure shares one typographic scale.
theme_study2 <- function(base_size = MS_BASE_SIZE) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      text              = ggplot2::element_text(family = ""),
      # lineheight < 1 tightens the 2-line facet strips used by Figure 1E
      # ("Fertiliser\nDigestate - Mineral" etc.) and Figure 2A/D without
      # needing a taller strip; margin gives 2-line strips a touch of
      # top/bottom breathing room so descenders/ascenders never crowd the
      # strip border.
      strip.text        = ggplot2::element_text(face = "bold", size = MS_STRIP_SIZE, lineheight = 0.92,
                                                  margin = ggplot2::margin(t = 3, b = 3)),
      strip.background  = ggplot2::element_rect(fill = MS_STRIP_FILL, colour = NA),
      legend.position   = "top",
      legend.title      = ggplot2::element_text(size = base_size * 0.92),
      legend.text       = ggplot2::element_text(size = base_size * 0.88),
      # Compact legend keys/spacing so a 3-5 item top legend (crop, row
      # position, fertiliser, environment) reliably fits within the
      # manuscript's mandated 7.5-8.0in figure width without its last item
      # clipping at the right edge (confirmed clipped with ggplot's default
      # key size at that width).
      legend.key.size   = grid::unit(0.85, "lines"),
      legend.spacing.x  = grid::unit(4, "pt"),
      legend.margin     = ggplot2::margin(t = 0, b = 2),
      axis.title        = ggplot2::element_text(size = base_size * 0.92),
      axis.text         = ggplot2::element_text(size = base_size * 0.82, colour = "grey20"),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(colour = MS_GRID_COLOUR, linewidth = 0.35),
      panel.border      = ggplot2::element_rect(colour = "grey70", fill = NA),
      panel.spacing     = grid::unit(12, "pt"),
      plot.title        = ggplot2::element_text(size = MS_TITLE_SIZE, face = "plain", margin = ggplot2::margin(b = 4)),
      plot.subtitle     = ggplot2::element_text(colour = "grey30"),
      plot.margin       = ggplot2::margin(t = 4, r = 10, b = 6, l = 4)
    )
}

#' Theme additions applied once, at the composite (patchwork `&`) level, in
#' every fig0N_*.R assemble function -- standardises panel-letter size/
#' placement across figures from one source instead of each assemble
#' function repeating its own `theme(plot.tag = ...)` call (which had
#' already drifted to needing separate fixes in Figure 1 vs Figure 2 during
#' development).
manuscript_composite_theme <- function() {
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = MS_TAG_SIZE, face = "bold"),
    plot.tag.position = c(0, 1)
  )
}

# ---- LER (a-LER) palette ------------------------------------------------------
# Replaces functions_ler.R's original ad hoc, unvalidated
# ler_treatment_colors <- c(REFERENCE = "darkgreen", "SOUTH-ORIENTED" = "skyblue",
# VERTICAL = "orange"). Validated by OKLab delta-E simulation (all three pairs
# clear the safety floor by a wide margin under every CVD type tested):
# REFERENCE vs SOUTH-ORIENTED >= 32.7, REFERENCE vs VERTICAL >= 16.1,
# SOUTH-ORIENTED vs VERTICAL >= 29.2 (worst case across protan/deuter/tritan).
ler_colors <- c(
  "REFERENCE"      = "#4D4D4D",
  "SOUTH-ORIENTED" = "#E69F00",
  "VERTICAL"       = "#0072B2"
)
