# Shared helpers for the Study 2 targets pipeline: display palettes and a
# consistent figure-saving convention. Consolidates palettes that were
# redefined inline (with minor inconsistencies) across CROP_YIELD_ANALYSIS_2025.Rmd,
# FERTILISATION_CALCULATION.Rmd, TDR_ANALYSIS.Rmd and SOIL_WATER.Rmd.

# ---- Colour palettes --------------------------------------------------------

location_simple_colors <- c(
  "BETWEEN_PV" = "#E64B35FF",
  "OUTSIDE_PV" = "#00A087FF"
)

location_global_substrip_colors <- c(
  "BETWEEN_PV_WEST"   = "#E64B35FF",
  "BETWEEN_PV_MIDDLE" = "#B22222",
  "BETWEEN_PV_EAST"   = "#8B0000",
  "OUTSIDE_PV_WEST"   = "#00A087FF",
  "OUTSIDE_PV_MIDDLE" = "#007A66",
  "OUTSIDE_PV_EAST"   = "#00594A"
)

fertilisation_colors <- c(
  "BIOGAS_DIGESTATE"    = "#F1A340",
  "MINERAL_FERTILISER"  = "#998EC3"
)

residue_colors <- c(
  "REMOVED" = "#A6CEE3",
  "REMAIN"  = "#FB9A99"
)

crop_colors <- c(
  "WW" = "#F1A983",
  "SB" = "#FFD966",
  "GC" = "#4EA72E",
  "RG" = "#4EA78E",
  "SY" = "#44B3E1"
)

# ---- Label shortening --------------------------------------------------------

#' Shorten GLOBAL_SUB_STRIP_LOCATION to compact plot labels
#' e.g. "BETWEEN_PV_WEST" -> "B_PV_W", "OUTSIDE_PV_MIDDLE" -> "O_PV_M"
shorten_global_location <- function(x) {
  x <- as.character(x)
  x <- sub("^BETWEEN_PV_", "B_PV_", x)
  x <- sub("^OUTSIDE_PV_", "O_PV_", x)
  x <- sub("_WEST$",   "_W", x)
  x <- sub("_MIDDLE$", "_M", x)
  x <- sub("_EAST$",   "_E", x)
  x
}

#' Shorten FERTILISATION to a two-letter code ("BD" / "MF")
shorten_fertilisation <- function(x) {
  dplyr::case_when(
    x == "BIOGAS_DIGESTATE"   ~ "BD",
    x == "MINERAL_FERTILISER" ~ "MF",
    TRUE ~ as.character(x)
  )
}

#' Shorten RESIDUE to a short code ("RMVD" / "RMN")
shorten_residue <- function(x) {
  dplyr::case_when(
    x == "REMOVED" ~ "RMVD",
    x == "REMAIN"  ~ "RMN",
    TRUE ~ as.character(x)
  )
}

# ---- Figure / table saving --------------------------------------------------

#' Save a ggplot into PIPELINE_OUTPUTS/figures/<outcome_id>/, creating the
#' subfolder if needed. Returns the output path invisibly so it can be used
#' directly as a targets `format = "file"` return value.
save_report_figure <- function(plot, outcome_id, filename,
                                width = 8, height = 5, dpi = 300,
                                base_dir = "PIPELINE_OUTPUTS/figures") {
  out_dir <- file.path(base_dir, outcome_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, filename)
  ggplot2::ggsave(out_path, plot = plot, width = width, height = height, dpi = dpi)
  invisible(out_path)
}

#' Save a gt table (as HTML + a plain .csv of its underlying data) into
#' PIPELINE_OUTPUTS/tables/<outcome_id>/.
save_report_table <- function(gt_tbl, data, outcome_id, filename_stem,
                               base_dir = "PIPELINE_OUTPUTS/tables") {
  out_dir <- file.path(base_dir, outcome_id)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  html_path <- file.path(out_dir, paste0(filename_stem, ".html"))
  csv_path  <- file.path(out_dir, paste0(filename_stem, ".csv"))
  gt::gtsave(gt_tbl, html_path)
  readr::write_csv(data, csv_path)
  invisible(c(html_path, csv_path))
}

#' NULL/NA-coalescing operator for scalar values (strings, numbers). Not safe
#' for language objects (formulas, calls) -- `is.na()` on those returns a
#' vector, not a length-1 logical, which trips R's strict `||`. Guarded here
#' so a non-scalar `x` just short-circuits to `x` itself rather than erroring.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.atomic(x) && is.na(x)) return(y)
  x
}
