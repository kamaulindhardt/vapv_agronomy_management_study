# Data request: 2024 grass-clover yields per cut for the green biorefinery group
# (total LPC yield per ha calculations).
#
# Standalone script -- NOT part of the targets pipeline. Run from the project
# root so relative paths resolve:
#   Rscript --vanilla 4_outputs/data_requests/gc_2024_biorefinery/build_gc_2024_yield_table.R
#
# Output (written next to this script):
#   GC_2024_yield_per_cut_biorefinery.docx   table + figure + method notes
#   GC_2024_yield_per_cut_table1.xlsx        Table 1 (unrounded values, display-formatted) + Notes sheet
#   GC_2024_yield_per_cut_figure.png         the figure on its own
#   GC_2024_yield_per_cut_summary.csv        the table's numbers, unrounded to 3 dp
#
# Scope (see the notes printed inside the docx):
#   * Year 2024, grass-clover (CROP == "GC"), vertical PV set-up, Value4Farm
#     strip A3 (A3a = Outside PV, A3b = Between PV): the only grass-clover
#     strip carrying the full digestate x mineral x residue factorial.
#   * Fertiliser: digestate = treatments A + C; mineral = treatments B + D.
#     Pooled across residue treatment (removed/retained) and sub-strips
#     (west/middle/east) -> n = 6 plots per Position x Fertiliser x Cut.
#   * Dry-matter yield is RECOMPUTED as fresh-matter yield x DM% / 100 for
#     every plot. The workbook's DRY_WEIGHT_* columns reconcile exactly with
#     that for cuts 1-4 and for the HYPERFARM rows, but for Value4Farm cut 5
#     (14 Oct 2024, 24 rows) they are shifted one row relative to the fresh
#     weight/DM% columns (checked below).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(ggplot2)
  library(flextable)
  library(officer)
  library(openxlsx)
})

root <- normalizePath(getwd(), winslash = "/")
stopifnot("Run this script from the project root (the folder containing _targets.R)" =
            file.exists(file.path(root, "_targets.R")))

out_dir <- file.path(root, "4_outputs", "data_requests", "gc_2024_biorefinery")
xlsx    <- file.path(root, "1_input_data", "crop_yield_quality", "2024",
                     "HYPERFARM_VALUE4FARM_YIELD_DATA_2024_v3.xlsx")

# Shared palettes/theme (fertilisation_colors, theme_study2) so the figure
# matches the manuscript figures.
source(file.path(root, "3_analysis", "R", "utils.R"))
source(file.path(root, "3_analysis", "R", "plot_theme.R"))

# ---- 1. Read and select ------------------------------------------------------

raw <- read_excel(xlsx, sheet = "ALL_CROPS_COMBINED")
names(raw) <- trimws(names(raw))

gc_all <- raw |>
  filter(CROP == "GC", YEAR == 2024) |>
  mutate(
    CUT = as.integer(CUT),
    FRESH_WEIGHT_TON_HA = as.numeric(FRESH_WEIGHT_TON_HA),
    TS_PCT = as.numeric(TS_PCT),
    DRY_WEIGHT_TON_HA = as.numeric(DRY_WEIGHT_TON_HA),
    CUT_DATE = as.Date(sprintf("%s-%02d-%02d", CUT_YEAR, as.integer(CUT_MONTH), as.integer(CUT_DAY)))
  )

# Integrity check 1: fresh weight t/ha must follow from plot kg and harvested area.
stopifnot(all(abs(gc_all$FRESH_WEIGHT_PLOT_KG / gc_all$HARVESTED_PLOT_SIZE_M2 * 10 -
                    gc_all$FRESH_WEIGHT_TON_HA) < 1e-9))

# Integrity check 2: does the recorded dry weight equal fresh weight x DM%?
gc_all <- gc_all |>
  mutate(DM_YIELD_RECOMPUTED = FRESH_WEIGHT_TON_HA * TS_PCT / 100,
         DM_MISMATCH = abs(DRY_WEIGHT_TON_HA - DM_YIELD_RECOMPUTED) > 1e-6)
message("\nRows where recorded DRY_WEIGHT_TON_HA != FW x DM%/100, by project and cut:")
print(gc_all |> filter(DM_MISMATCH) |> count(PROJECT, CUT))

# Is the mismatch a one-row shift? (recorded[i] == recomputed[i - 1] within the block)
blk <- gc_all |> filter(PROJECT == "VALUE4FARM", CUT == 5)
shift_ok <- isTRUE(all.equal(blk$DRY_WEIGHT_TON_HA[-1], blk$DM_YIELD_RECOMPUTED[-nrow(blk)]))
message("Value4Farm cut 5: recorded dry weight == previous row's FW x DM%/100 -> ", shift_ok)
stopifnot("Mismatch is not confined to V4F cut 5" =
            all(gc_all$DM_MISMATCH == (gc_all$PROJECT == "VALUE4FARM" & gc_all$CUT == 5)))

gc <- gc_all |>
  filter(PROJECT == "VALUE4FARM", PV_SETUP == "VERTICAL") |>
  mutate(
    FERTILISATION = case_when(
      TREATMENT %in% c("A", "C") ~ "BIOGAS_DIGESTATE",
      TREATMENT %in% c("B", "D") ~ "MINERAL_FERTILISER"
    ),
    DM_YIELD_TON_HA = DM_YIELD_RECOMPUTED
  )

# Design checks: 2 positions x 2 fertilisers x 5 cuts x 6 plots = 120 rows.
stopifnot(nrow(gc) == 120, !anyNA(gc$FERTILISATION),
          all(table(gc$LOCATION, gc$FERTILISATION, gc$CUT) == 6),
          all(tapply(gc$CUT_DATE, gc$CUT, function(d) length(unique(d))) == 1))

# ---- 2. Summarise ------------------------------------------------------------

month_en <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
fmt_date <- function(d) paste(as.integer(format(d, "%d")), month_en[as.integer(format(d, "%m"))])

summ <- gc |>
  group_by(LOCATION, FERTILISATION, CUT) |>
  summarise(
    CUT_DATE = first(CUT_DATE),
    n        = n(),
    fm_mean  = mean(FRESH_WEIGHT_TON_HA), fm_sd = sd(FRESH_WEIGHT_TON_HA),
    dmc_mean = mean(TS_PCT),              dmc_sd = sd(TS_PCT),
    dm_mean  = mean(DM_YIELD_TON_HA),     dm_sd  = sd(DM_YIELD_TON_HA),
    .groups = "drop"
  ) |>
  mutate(
    LOCATION      = factor(LOCATION, c("BETWEEN_PV", "OUTSIDE_PV")),
    FERTILISATION = factor(FERTILISATION, c("BIOGAS_DIGESTATE", "MINERAL_FERTILISER"))
  ) |>
  arrange(LOCATION, FERTILISATION, CUT)

position_label <- c(BETWEEN_PV = "Between PV", OUTSIDE_PV = "Outside PV")
fert_label     <- c(BIOGAS_DIGESTATE = "Biogas digestate", MINERAL_FERTILISER = "Mineral fertiliser")

# Effect of the dry-weight correction, for the record (V4F cut 5 only differs).
message("\nValue4Farm cut 5 mean DM yield (t/ha): recorded vs recomputed")
print(gc |> filter(CUT == 5) |>
        group_by(LOCATION, FERTILISATION) |>
        summarise(recorded = mean(DRY_WEIGHT_TON_HA), recomputed = mean(DM_YIELD_TON_HA), .groups = "drop"))

# CSV companion (so the biorefinery group need not retype numbers from Word).
csv <- summ |>
  transmute(
    position = position_label[as.character(LOCATION)],
    fertiliser = fert_label[as.character(FERTILISATION)],
    cut = CUT, cut_date = format(CUT_DATE), n_plots = n,
    fm_yield_mean_t_ha = round(fm_mean, 3), fm_yield_sd_t_ha = round(fm_sd, 3),
    dm_content_mean_pct = round(dmc_mean, 3), dm_content_sd_pct = round(dmc_sd, 3),
    dm_yield_mean_t_ha = round(dm_mean, 3), dm_yield_sd_t_ha = round(dm_sd, 3)
  )
write.csv(csv, file.path(out_dir, "GC_2024_yield_per_cut_summary.csv"), row.names = FALSE)

# ---- 3. Figure ---------------------------------------------------------------

metric_levels <- c("Fresh matter yield (t/ha)", "Dry matter yield (t/ha)")
cut_levels    <- paste0("Cut ", 1:5, "\n", fmt_date(sort(unique(gc$CUT_DATE))))

lab_cut <- function(x) factor(x, levels = 1:5, labels = cut_levels)

bars <- summ |>
  select(LOCATION, FERTILISATION, CUT, fm_mean, fm_sd, dm_mean, dm_sd) |>
  pivot_longer(c(fm_mean, fm_sd, dm_mean, dm_sd),
               names_to = c("metric", "stat"), names_sep = "_") |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(metric = factor(recode(metric, fm = metric_levels[1], dm = metric_levels[2]), metric_levels),
         CUT_LAB = lab_cut(CUT))

pts <- gc |>
  transmute(LOCATION = factor(LOCATION, levels(summ$LOCATION)),
            FERTILISATION = factor(FERTILISATION, levels(summ$FERTILISATION)),
            CUT_LAB = lab_cut(CUT),
            `Fresh matter yield (t/ha)` = FRESH_WEIGHT_TON_HA,
            `Dry matter yield (t/ha)` = DM_YIELD_TON_HA) |>
  pivot_longer(all_of(metric_levels), names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric, metric_levels))

dodge <- position_dodge(width = 0.8)

fig <- ggplot(bars, aes(CUT_LAB, mean, fill = FERTILISATION)) +
  geom_col(position = dodge, width = 0.72) +
  geom_point(data = pts, aes(y = value, group = FERTILISATION),
             position = position_jitterdodge(jitter.width = 0.10, dodge.width = 0.8, seed = 42),
             shape = 16, size = 1.1, colour = "grey20", alpha = 0.55) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = dodge,
                width = 0.22, linewidth = 0.5, colour = "grey10") +
  facet_grid(metric ~ LOCATION, scales = "free_y", switch = "y",
             labeller = labeller(LOCATION = position_label)) +
  scale_fill_manual(values = fertilisation_colors, labels = fert_label, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
  labs(x = NULL, y = NULL) +
  theme_study2() +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90),
        panel.grid.major.x = element_blank())

fig_path <- file.path(out_dir, "GC_2024_yield_per_cut_figure.png")
fig_w <- 9.0; fig_h <- 4.5  # inches; also the size embedded in the docx (1:1, so text sizes are true)
ggsave(fig_path, fig, width = fig_w, height = fig_h, dpi = 300, device = ragg::agg_png, bg = "white")

# ---- 4. Table ----------------------------------------------------------------

tab <- summ |>
  transmute(
    Position   = position_label[as.character(LOCATION)],
    Fertiliser = fert_label[as.character(FERTILISATION)],
    Cut        = as.character(CUT),
    Date       = paste(fmt_date(CUT_DATE), "2024"),
    n          = as.character(n),
    fm_mean = sprintf("%.1f", fm_mean),  fm_sd = sprintf("%.1f", fm_sd),
    dmc_mean = sprintf("%.1f", dmc_mean), dmc_sd = sprintf("%.1f", dmc_sd),
    dm_mean = sprintf("%.2f", dm_mean),  dm_sd = sprintf("%.2f", dm_sd)
  )

font <- "Calibri"
rule_grp  <- fp_border(color = "grey35", width = 1.2)
rule_row  <- fp_border(color = "grey85", width = 0.5)
n_rows    <- nrow(tab)

ft <- flextable(tab) |>
  set_header_labels(Position = "Position", Fertiliser = "Fertiliser", Cut = "Cut", Date = "Cut date", n = "n (plots)",
                    fm_mean = "Mean", fm_sd = "SD", dmc_mean = "Mean", dmc_sd = "SD", dm_mean = "Mean", dm_sd = "SD") |>
  add_header_row(values = c("", "Fresh matter yield\n(t FM/ha)", "Dry matter content\n(%)", "Dry matter yield\n(t DM/ha)"),
                 colwidths = c(5, 2, 2, 2)) |>
  merge_v(j = 1:5, part = "header") |>
  font(fontname = font, part = "all") |>
  fontsize(size = 9.5, part = "all") |>
  bold(part = "header") |>
  bg(part = "header", bg = "#E9E9E9") |>
  align(j = 3:11, align = "center", part = "all") |>
  valign(valign = "center", part = "all") |>
  padding(padding.top = 2.5, padding.bottom = 2.5, part = "all") |>
  border_remove() |>
  hline(border = rule_row, part = "body") |>
  hline(i = seq(5, n_rows - 1, by = 5), border = rule_grp, part = "body") |>
  hline_top(border = rule_grp, part = "header") |>
  hline_bottom(border = rule_grp, part = "header") |>
  hline_bottom(border = rule_grp, part = "body") |>
  bold(j = 1:2, part = "body") |>
  set_table_properties(layout = "fixed") |>
  width(j = 1:11, width = c(1.2, 1.7, 0.5, 1.1, 0.8, rep(0.79, 6)))   # sums to the 10.1 in text width

# ---- 5. Document -------------------------------------------------------------

notes <- list(
  "Source" = paste0("HYPERFARM_VALUE4FARM_YIELD_DATA_2024_v3.xlsx (sheet ALL_CROPS_COMBINED); Value4Farm grass-clover strip A3 ",
                    "(A3a = Outside PV, A3b = Between PV), vertical PV set-up; 4.5 m² harvested per plot."),
  "Pooling" = paste0("digestate = treatments A + C, mineral = treatments B + D; each is pooled across residue removed/retained and ",
                     "west/middle/east sub-strips. SD is the sample SD (n − 1) across those 6 plots, so it includes sub-strip and ",
                     "residue-treatment variation as well as plot-to-plot noise."),
  "Dry matter yield" = paste0("calculated per plot as fresh matter yield × dry matter content / 100, then averaged (so it is not the product of ",
                              "the two column means). In the source workbook the dry-weight columns for Value4Farm cut 5 are offset by one row from ",
                              "the fresh-weight and dry-matter-content columns, so dry matter yield here is recomputed rather than read from those columns."),
  "Not included" = paste0("the HYPERFARM strips (A5, B1, B3), whose 2024 field log records only potassium fertiliser (no digestate, no NPK), and ",
                          "the south-oriented PV set-up. Botanical composition (grass:clover ratio) was not recorded.")
)

txt  <- function(x, font.size = 10, ...) ftext(x, prop = fp_text(font.family = font, font.size = font.size, ...))
para <- function(..., after = 4) fpar(..., fp_p = fp_par(padding.bottom = after, line_spacing = 1.05))

doc <- read_docx() |>
  body_set_default_section(prop_section(
    page_size = page_size(width = 29.7 / 2.54, height = 21 / 2.54, orient = "landscape"),
    page_margins = page_mar(top = 0.7, bottom = 0.7, left = 0.8, right = 0.8,
                            header = 0.3, footer = 0.3, gutter = 0)
  )) |>
  body_add_fpar(para(txt("Grass-clover yield per cut, 2024", bold = TRUE, font.size = 16), after = 1)) |>
  body_add_fpar(para(txt("Value4Farm vertical agrivoltaic experiment, Foulum  |  Data for the green biorefinery group (LPC yield per ha)",
                         color = "grey30"), after = 6)) |>
  body_add_fpar(para(
    txt("Fresh and dry matter yield of the five 2024 cuts, by position relative to the PV rows (Between PV, Outside PV) and fertiliser "),
    txt("(biogas digestate, mineral fertiliser). Means and standard deviations are pooled across the residue treatments and the three "),
    txt("sub-strips, so each mean is based on n = 6 plots. Method notes follow the figure."), after = 6)) |>
  body_add_fpar(para(txt("Table 1. ", bold = TRUE),
                     txt("Grass-clover yield per cut in 2024: mean and standard deviation (SD) of n plots."), after = 3)) |>
  body_add_flextable(ft) |>
  body_add_break() |>
  body_add_fpar(para(txt("Figure 1. ", bold = TRUE),
                     txt("Fresh matter (top) and dry matter (bottom) yield per cut by position and fertiliser. Bars are means, whiskers are "),
                     txt("± 1 SD, dots are the individual plots (n = 6 per bar)."), after = 3)) |>
  body_add_img(fig_path, width = fig_w, height = fig_h) |>
  body_add_fpar(para(txt("Method notes", bold = TRUE, font.size = 11), after = 2))

for (i in seq_along(notes)) {
  doc <- body_add_fpar(doc, para(txt(paste0(names(notes)[i], ": "), bold = TRUE), txt(notes[[i]]),
                                 after = if (i < length(notes)) 2 else 0))
}

docx_path <- file.path(out_dir, "GC_2024_yield_per_cut_biorefinery.docx")
print(doc, target = docx_path)

# ---- 6. Excel (Table 1) ------------------------------------------------------
# Same layout as Table 1 in the docx, but cells hold the unrounded numbers (number
# formats reproduce the docx's displayed decimals), so nothing is lost to rounding
# when the biorefinery group calculates from them. Method notes go on a second sheet.

xl <- summ |>
  transmute(
    Position = position_label[as.character(LOCATION)],
    Fertiliser = fert_label[as.character(FERTILISATION)],
    Cut = CUT, `Cut date` = CUT_DATE, `n (plots)` = n,
    fm_mean, fm_sd, dmc_mean, dmc_sd, dm_mean, dm_sd
  )

wb <- createWorkbook()
addWorksheet(wb, "Table 1", gridLines = FALSE)
hdr <- createStyle(fontName = font, fontSize = 10, textDecoration = "bold", fgFill = "#E9E9E9",
                   halign = "center", valign = "center", wrapText = TRUE,
                   border = "TopBottom", borderColour = "grey35", borderStyle = "medium")
body <- createStyle(fontName = font, fontSize = 10, valign = "center")
grp  <- createStyle(border = "Bottom", borderColour = "grey35", borderStyle = "medium")

writeData(wb, "Table 1", "Table 1. Grass-clover yield per cut in 2024: mean and standard deviation (SD) of n plots.",
          startCol = 1, startRow = 1)
addStyle(wb, "Table 1", createStyle(fontName = font, fontSize = 10, textDecoration = "bold"), rows = 1, cols = 1)

# two-row header: id columns span both header rows; yield groups merged over Mean/SD
for (j in 1:5) {
  writeData(wb, "Table 1", names(xl)[j], startRow = 3, startCol = j)
  mergeCells(wb, "Table 1", cols = j, rows = 3:4)
}
group_labels <- c("Fresh matter yield\n(t FM/ha)", "Dry matter content\n(%)", "Dry matter yield\n(t DM/ha)")
for (g in 1:3) {
  cols <- c(6, 8, 10)[g] + 0:1
  writeData(wb, "Table 1", group_labels[g], startRow = 3, startCol = cols[1])
  mergeCells(wb, "Table 1", cols = cols, rows = 3)
}
writeData(wb, "Table 1", t(rep(c("Mean", "SD"), 3)), startRow = 4, startCol = 6, colNames = FALSE)
writeData(wb, "Table 1", xl, startRow = 5, colNames = FALSE)

first <- 5; last <- 4 + nrow(xl)
addStyle(wb, "Table 1", hdr, rows = 3:4, cols = 1:11, gridExpand = TRUE)
addStyle(wb, "Table 1", body, rows = first:last, cols = 1:11, gridExpand = TRUE)
addStyle(wb, "Table 1", createStyle(fontName = font, fontSize = 10, textDecoration = "bold"),
         rows = first:last, cols = 1:2, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Table 1", createStyle(halign = "center"), rows = first:last, cols = 3:11, gridExpand = TRUE, stack = TRUE)
# [$-409] forces English month names (as in the docx) whatever the viewer's Excel locale
addStyle(wb, "Table 1", createStyle(numFmt = "[$-409]d mmm yyyy;@"), rows = first:last, cols = 4, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Table 1", createStyle(numFmt = "0.0"),  rows = first:last, cols = 6:9, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Table 1", createStyle(numFmt = "0.00"), rows = first:last, cols = 10:11, gridExpand = TRUE, stack = TRUE)
addStyle(wb, "Table 1", grp, rows = seq(first + 4, last, by = 5), cols = 1:11, gridExpand = TRUE, stack = TRUE)
setColWidths(wb, "Table 1", cols = 1:11, widths = c(14, 20, 6, 13, 10, rep(10, 6)))
setRowHeights(wb, "Table 1", rows = 3, heights = 30)
freezePane(wb, "Table 1", firstActiveRow = 5)
pageSetup(wb, "Table 1", orientation = "landscape", paperSize = 9, fitToWidth = 1, fitToHeight = 1)

addWorksheet(wb, "Notes", gridLines = FALSE)
writeData(wb, "Notes", data.frame(Topic = names(notes), Note = unlist(notes, use.names = FALSE)))
addStyle(wb, "Notes", createStyle(fontName = font, fontSize = 10, textDecoration = "bold", valign = "top"),
         rows = 2:(1 + length(notes)), cols = 1, gridExpand = TRUE)
addStyle(wb, "Notes", createStyle(fontName = font, fontSize = 10, wrapText = TRUE, valign = "top"),
         rows = 2:(1 + length(notes)), cols = 2, gridExpand = TRUE)
addStyle(wb, "Notes", createStyle(fontName = font, fontSize = 10, textDecoration = "bold", fgFill = "#E9E9E9"),
         rows = 1, cols = 1:2, gridExpand = TRUE)
setColWidths(wb, "Notes", cols = 1:2, widths = c(18, 110))
# openxlsx does not autofit wrapped rows: ~105 characters fit per line at this width.
setRowHeights(wb, "Notes", rows = 2:(1 + length(notes)),
              heights = 13.5 * ceiling(nchar(unlist(notes, use.names = FALSE)) / 105) + 3)
pageSetup(wb, "Notes", orientation = "landscape", paperSize = 9, fitToWidth = 1, fitToHeight = 0)

xlsx_path <- file.path(out_dir, "GC_2024_yield_per_cut_table1.xlsx")
saveWorkbook(wb, xlsx_path, overwrite = TRUE)

# openxlsx 4.2.7.1 leaves each sheet's relationships (and [Content_Types].xml) pointing at
# xl/drawings parts it never writes. Excel ignores that, but strict readers (Python's
# openpyxl/pandas) refuse the file with a KeyError. Drop every internal relationship and
# content-type override whose target part is not in the package, then re-zip.
strip_dangling_refs <- function(path) {
  exdir <- tempfile("xlsx_fix_"); dir.create(exdir)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  unzip(path, exdir = exdir)
  parts <- unzip(path, list = TRUE)$Name
  read_part <- function(p) rawToChar(readBin(file.path(exdir, p), "raw", file.size(file.path(exdir, p))))
  write_part <- function(p, x) writeBin(charToRaw(x), file.path(exdir, p))
  resolve <- function(base, target) {
    full <- if (startsWith(target, "/")) sub("^/", "", target) else if (base == "") target else paste(base, target, sep = "/")
    out <- character()
    for (s in strsplit(full, "/", fixed = TRUE)[[1]]) {
      if (s == "..") out <- head(out, -1) else if (!s %in% c(".", "")) out <- c(out, s)
    }
    paste(out, collapse = "/")
  }
  n_removed <- 0L
  drop_tags <- function(xml, pattern, is_dangling) {
    tags <- regmatches(xml, gregexpr(pattern, xml, perl = TRUE))[[1]]
    for (tag in tags[vapply(tags, is_dangling, logical(1))]) {
      xml <- sub(tag, "", xml, fixed = TRUE); n_removed <<- n_removed + 1L
    }
    xml
  }
  for (rels in grep("\\.rels$", parts, value = TRUE)) {
    base <- dirname(dirname(rels)); if (base == ".") base <- ""
    xml <- read_part(rels)
    new <- drop_tags(xml, "<Relationship [^>]*>", function(tag) {
      !grepl("TargetMode=\"External\"", tag, fixed = TRUE) &&
        !resolve(base, sub(".*Target=\"([^\"]*)\".*", "\\1", tag)) %in% parts
    })
    if (!identical(new, xml)) write_part(rels, new)
  }
  ct  <- read_part("[Content_Types].xml")
  new <- drop_tags(ct, "<Override [^>]*>", function(tag) !sub("^/", "", sub(".*PartName=\"([^\"]*)\".*", "\\1", tag)) %in% parts)
  if (!identical(new, ct)) write_part("[Content_Types].xml", new)

  if (n_removed > 0L) {
    fixed <- tempfile(fileext = ".xlsx")
    zip::zip(fixed, files = parts, root = exdir, mode = "mirror")
    file.copy(fixed, path, overwrite = TRUE); unlink(fixed)
  }
  n_removed
}
n_fixed <- strip_dangling_refs(xlsx_path)
message("xlsx: removed ", n_fixed, " dangling drawing reference(s)")

message("\nWrote:\n  ", docx_path, "\n  ", xlsx_path, "\n  ", fig_path, "\n  ",
        file.path(out_dir, "GC_2024_yield_per_cut_summary.csv"))
