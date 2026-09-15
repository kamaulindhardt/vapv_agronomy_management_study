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

#' Shared ggplot2 theme for every non-preliminary figure: theme_bw-based (same
#' family as the preliminary figures, for a consistent look), bold facet
#' strips, top legend, minor gridlines dropped (high data-ink ratio, Midway
#' Principle #3), light panel border.
theme_study2 <- function(base_size = 12) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      strip.text       = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey92", colour = NA),
      legend.position   = "top",
      panel.grid.minor  = ggplot2::element_blank(),
      panel.border      = ggplot2::element_rect(colour = "grey70", fill = NA),
      plot.subtitle     = ggplot2::element_text(colour = "grey30")
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
