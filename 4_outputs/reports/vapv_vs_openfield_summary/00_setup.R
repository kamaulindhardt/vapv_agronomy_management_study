# VAPV vs open-field summary report -- shared setup, helpers, palette and theme.
#
# Run order:  00_setup.R -> 01_data_and_tables.R -> 02_figures.R   (run_all.R sources all three)
# Then:       build_report_docx.py  (assembles the Word report from the CSV tables + PNG figures)
#
# Design principle: EVERY number that appears in the report text comes from a table
# written by 01_data_and_tables.R (tables/*.csv); nothing is typed by hand.
#
# Data basis: the cached `targets` objects in _targets/objects/ (yield, NO3 leaching, IUNG
# SOC) plus the raw TDR csv files, soil-survey workbook and Daisy outputs under 1_input_data/.
# The raw inputs have not changed since the targets were last built (checked by file
# modification dates when this report was written).

suppressPackageStartupMessages({
  # On the machine this was written on, R 4.4 packages live in the renv cache rather than a
  # project library. Prepend that library if it exists; otherwise rely on renv::restore().
  .renv_lib <- "C:/Users/au759124/AppData/Local/R/cache/R/renv/library/vapv_agronomy_management_study-848e8773/windows/R-4.4/x86_64-w64-mingw32"
  if (dir.exists(.renv_lib)) .libPaths(c(.renv_lib, .libPaths()))
  library(dplyr); library(tidyr); library(purrr); library(tibble); library(stringr)
  library(forcats); library(lubridate); library(readr); library(readxl)
  library(ggplot2); library(patchwork); library(scales)
})

args <- commandArgs(trailingOnly = TRUE)
ROOT <- if (length(args) >= 1) args[1] else "C:/Users/au759124/OneDrive - Aarhus universitet/Documents/vapv_agronomy_management_study"
REP  <- file.path(ROOT, "4_outputs", "reports", "vapv_vs_openfield_summary")
TGT  <- file.path(ROOT, "_targets", "objects")
FIG  <- file.path(REP, "figures")
TAB  <- file.path(REP, "tables")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
dir.create(TAB, showWarnings = FALSE, recursive = TRUE)

rd <- function(name) readRDS(file.path(TGT, name))
write_tab <- function(df, name) { readr::write_csv(df, file.path(TAB, paste0(name, ".csv"))); invisible(df) }

# ---- Factor vocabulary --------------------------------------------------------
ENV_LV <- c("Open field", "VAPV")
POS_LV <- c("West", "Centre", "East")
POS6_LV <- c("Open W", "Open C", "Open E", "VAPV W", "VAPV C", "VAPV E")

env_from_loc <- function(loc) factor(if_else(loc == "BETWEEN_PV", "VAPV", "Open field"), ENV_LV)
pos_from_sub <- function(p) factor(recode(as.character(p), WEST = "West", MIDDLE = "Centre", EAST = "East"), POS_LV)

# Nest (sub-strip, N1..N30) attributes derived from the field layout: nests come in triplets
# W/C/E; triplets alternate open field / VAPV along each main strip A1..A5 (6 nests per strip).
# Cross-checked against GLOBAL_SUB_STRIP_LOCATION in the yield data (see 01_data_and_tables.R).
nest_attr <- function(nest) {
  k <- as.integer(str_extract(nest, "[0-9]+"))
  tibble(nest = nest,
         strip = paste0("A", (k - 1) %/% 6 + 1),
         env = factor(if_else(((k - 1) %/% 3) %% 2 == 0, "Open field", "VAPV"), ENV_LV),
         pos = factor(c("West", "Centre", "East")[(k - 1) %% 3 + 1], POS_LV))
}

add_nest_attr <- function(df) dplyr::bind_cols(df, dplyr::select(nest_attr(df$nest), -nest))

# ---- Statistics helpers ---------------------------------------------------------
se <- function(x) { x <- x[!is.na(x)]; if (length(x) < 2) NA_real_ else sd(x) / sqrt(length(x)) }

#' Welch t-test on log values: VAPV / Open-field ratio, expressed as % difference.
#' `d` needs columns `env` (factor Open field / VAPV) and the value column `v` (> 0).
#' Returns the geometric-mean ratio effect with 95% CI and p; n_* = number of units.
welch_ratio <- function(d, v = "v") {
  x <- d[[v]]; e <- d$env; ok <- !is.na(x) & x > 0
  o <- x[ok & e == "Open field"]; p <- x[ok & e == "VAPV"]
  out <- tibble(n_open = length(o), n_vapv = length(p),
                mean_open = mean(o), mean_vapv = mean(p),
                se_open = se(o), se_vapv = se(p),
                pct = NA_real_, pct_lcl = NA_real_, pct_ucl = NA_real_, p = NA_real_, df = NA_real_)
  if (length(o) >= 2 && length(p) >= 2) {
    r <- t.test(log(p), log(o))
    out$pct <- 100 * (exp(unname(r$estimate[1] - r$estimate[2])) - 1)
    out$pct_lcl <- 100 * (exp(r$conf.int[1]) - 1); out$pct_ucl <- 100 * (exp(r$conf.int[2]) - 1)
    out$p <- r$p.value; out$df <- unname(r$parameter)
  }
  out
}

#' Welch t-test on the raw scale: VAPV minus Open field (absolute units).
welch_diff <- function(d, v = "v") {
  x <- d[[v]]; e <- d$env; ok <- !is.na(x)
  o <- x[ok & e == "Open field"]; p <- x[ok & e == "VAPV"]
  out <- tibble(n_open = length(o), n_vapv = length(p), mean_open = mean(o), mean_vapv = mean(p),
                se_open = se(o), se_vapv = se(p), diff = mean(p) - mean(o),
                diff_lcl = NA_real_, diff_ucl = NA_real_, p = NA_real_, df = NA_real_)
  if (length(o) >= 2 && length(p) >= 2) {
    r <- t.test(p, o); out$diff_lcl <- r$conf.int[1]; out$diff_ucl <- r$conf.int[2]
    out$p <- r$p.value; out$df <- unname(r$parameter)
  }
  out
}

# ---- Palette (project's established entity colours, see 3_analysis/R/utils.R) ----
ENV_COL   <- c("Open field" = "#00A087", "VAPV" = "#E64B35")
ENV_SHAPE <- c("Open field" = 16, "VAPV" = 17)    # shape redundant with colour (CVD/greyscale safe)
INK  <- "grey20"; INK2 <- "grey40"; GRID <- "grey90"

theme_rep <- function(base_size = 9) {
  theme_bw(base_size = base_size, base_family = "Arial") +
    theme(
      strip.text = element_text(face = "bold", size = base_size, margin = margin(2, 2, 2, 2)),
      strip.background = element_rect(fill = "grey93", colour = NA),
      legend.position = "top", legend.title = element_blank(),
      legend.key.size = unit(0.8, "lines"), legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, -4, 0),
      axis.title = element_text(size = base_size, colour = INK),
      axis.text = element_text(size = base_size - 1, colour = INK),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
      panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.4),
      plot.title = element_text(size = base_size + 0.5, face = "bold", margin = margin(b = 3)),
      plot.margin = margin(3, 6, 3, 3)
    )
}

save_fig <- function(p, name, w, h) {
  ggsave(file.path(FIG, paste0(name, ".png")), p, width = w, height = h, dpi = 300,
         device = ragg::agg_png, bg = "white")
}

fmt_en_date <- function(d) paste(as.integer(format(d, "%d")), month.abb[as.integer(format(d, "%m"))], format(d, "%Y"))   # locale-independent English date
fmt_p <- function(p) ifelse(is.na(p), "n/a", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
