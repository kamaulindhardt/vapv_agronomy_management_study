# Builds every dataset and table used in the VAPV vs open-field report.
# Writes tables/*.csv; figure data objects are kept in the R session for 02_figures.R.
# Requires 00_setup.R to have been sourced.

qc <- list()   # data-quality log -> tables/tbl_qc.csv
add_qc <- function(domain, item, n, handling) {
  qc[[length(qc) + 1]] <<- tibble(domain = domain, item = item, n = n, handling = handling)
}

# =============================================================================
# 1. CROP PRODUCTIVITY
# =============================================================================
y_all <- rd("study2_yield_analysis_ready")

y <- y_all |>
  filter(PRODUCT_SIMPLE %in% c("GRAIN", "BIOMASS")) |>             # primary harvestable product only
  mutate(env = env_from_loc(as.character(LOCATION)),
         pos = pos_from_sub(SUB_STRIP_LOCATION),
         nest = as.character(SUB_STRIP),
         year = as.integer(as.character(YEAR)),
         plot_id = paste(SUB_STRIP, ROW, COLUMN))

# Cross-check the layout rule in nest_attr() against the yield dataset's own labels
chk <- y |> distinct(nest, env, pos) |> left_join(nest_attr(unique(y$nest)), by = "nest", suffix = c("", ".rule"))
stopifnot(all(chk$env == chk$env.rule), all(chk$pos == chk$pos.rule))

add_qc("Yield", "Winter wheat 2025 plots with no yield recorded (blank template in the raw workbook)",
       sum(y$CROP == "WW" & y$year == 2025), "Excluded; winter wheat 2025 is absent from all yield results")
add_qc("Yield", "Ryegrass 2025 duplicate placeholder rows without data",
       sum(y$CROP == "RG" & is.na(y$DRY_WEIGHT_TON_HA)), "Dropped (one real record per plot remains)")
add_qc("Yield", "Grass-clover 2025 plots without cut 1 (strip A1, nests N1-N6)",
       sum(y$CROP == "GC" & y$year == 2025 & is.na(y$DRY_WEIGHT_TON_HA)),
       "Plots excluded from annual totals (incomplete season); strip A4 only for GC 2025")

# Annual plot totals: grass-clover = sum of all cuts in the year (complete-case plots only)
exp_cuts <- tribble(~CROP, ~year, ~n_exp, "GC", 2023L, 3L, "GC", 2024L, 5L, "GC", 2025L, 5L)
yy <- y |>
  filter(!is.na(DRY_WEIGHT_TON_HA)) |>
  group_by(CROP, year, env, pos, nest, plot_id, TREATMENT, FERTILISATION, RESIDUE) |>
  summarise(n_cuts = n(), dm = sum(DRY_WEIGHT_TON_HA), .groups = "drop") |>
  left_join(exp_cuts, by = c("CROP", "year")) |>
  mutate(n_exp = coalesce(n_exp, 1L)) |>
  filter(n_cuts == n_exp) |>
  mutate(cy = paste(CROP, year))
CY_LV <- c("WW 2023", "WW 2024", "SY 2024", "GC 2023", "GC 2024", "GC 2025", "SB 2025", "RG 2025")
yy$cy <- factor(yy$cy, CY_LV)
stopifnot(!any(is.na(yy$cy)))

CROP_LABEL <- c(WW = "Winter wheat", SY = "Soybean", GC = "Grass-clover", SB = "Spring barley", RG = "Ryegrass (undersown)")
CROP_MEAS  <- c(WW = "Grain", SY = "Grain", GC = "Biomass, sum of cuts", SB = "Green biomass", RG = "Biomass")

# ---- Table: crop-year (plot-level descriptives + nest-level inference) ----
cy_desc <- yy |> group_by(cy, CROP, year, env) |>
  summarise(n_plot = n(), n_nest = n_distinct(nest), mean = mean(dm), se = se(dm), .groups = "drop")
nest_cy <- yy |> group_by(cy, env, nest) |> summarise(v = mean(dm), .groups = "drop")
cy_inf <- nest_cy |> group_by(cy) |> group_modify(~ welch_ratio(.x)) |> ungroup() |>
  select(cy, n_nest_open = n_open, n_nest_vapv = n_vapv, pct, pct_lcl, pct_ucl, p, df)
tbl_yield_cy <- cy_desc |>
  pivot_wider(names_from = env, values_from = c(n_plot, n_nest, mean, se), names_sep = "_") |>
  rename(n_plot_open = `n_plot_Open field`, n_plot_vapv = n_plot_VAPV,
         mean_open = `mean_Open field`, mean_vapv = mean_VAPV,
         se_open = `se_Open field`, se_vapv = se_VAPV) |>
  select(-starts_with("n_nest_")) |>
  left_join(cy_inf, by = "cy") |>
  mutate(diff_abs = mean_vapv - mean_open, pct_means = 100 * (mean_vapv / mean_open - 1)) |>
  arrange(cy)
write_tab(tbl_yield_cy, "tbl_yield_cropyear")

# ---- Table: position (substrip) level ----
tbl_yield_pos <- yy |> group_by(cy, CROP, year, env, pos) |>
  summarise(n_plot = n(), mean = mean(dm), se = se(dm), .groups = "drop") |> arrange(cy, env, pos)
write_tab(tbl_yield_pos, "tbl_yield_position")
tbl_yield_pos_delta <- yy |> group_by(cy, pos) |> group_modify(~ welch_ratio(.x |> mutate(v = dm))) |> ungroup() |>
  mutate(pct_means = 100 * (mean_vapv / mean_open - 1))
write_tab(tbl_yield_pos_delta, "tbl_yield_position_delta")

# ---- Relative yield (RY): nest mean / open-field geometric-mean nest value of the same crop-year ----
ny <- yy |> group_by(cy, CROP, year, env, nest, pos) |> summarise(m = mean(dm), .groups = "drop")
ref <- ny |> filter(env == "Open field") |> group_by(cy) |> summarise(ref = exp(mean(log(m))), .groups = "drop")
ny <- ny |> left_join(ref, by = "cy") |> mutate(lry = log(m / ref))
nest_crop <- ny |> group_by(CROP, nest, env, pos) |> summarise(v = exp(mean(lry)), .groups = "drop")
yield_crop_inf <- nest_crop |> group_by(CROP) |> group_modify(~ welch_ratio(.x)) |> ungroup() |>
  mutate(label = unname(CROP_LABEL[CROP]))
nest_all <- ny |> group_by(nest, env, pos) |> summarise(v = exp(mean(lry)), n_cy = n(), .groups = "drop")
yield_all_inf <- welch_ratio(nest_all) |> mutate(CROP = "ALL", label = "All crops (pooled)")
tbl_yield_overall <- bind_rows(yield_crop_inf, yield_all_inf)
write_tab(tbl_yield_overall, "tbl_yield_overall")

# ---- Tissue N (2024 season only) ----
qn <- y_all |>
  filter(PRODUCT_SIMPLE %in% c("GRAIN", "BIOMASS"), !is.na(N_TOTAL)) |>
  mutate(env = env_from_loc(as.character(LOCATION)), pos = pos_from_sub(SUB_STRIP_LOCATION),
         nest = as.character(SUB_STRIP), plot_id = paste(SUB_STRIP, ROW, COLUMN)) |>
  group_by(CROP, env, pos, nest, plot_id) |> summarise(n_pct = mean(N_TOTAL), .groups = "drop")
qn_plot <- qn |> group_by(CROP, env) |> summarise(n_plot = n(), mean = mean(n_pct), se = se(n_pct), .groups = "drop")
qn_nest <- qn |> group_by(CROP, env, nest) |> summarise(v = mean(n_pct), .groups = "drop")
qn_inf <- qn_nest |> group_by(CROP) |> group_modify(~ welch_diff(.x)) |> ungroup() |>
  select(CROP, n_nest_open = n_open, n_nest_vapv = n_vapv, diff, diff_lcl, diff_ucl, p)
tbl_tissue_n <- qn_plot |> pivot_wider(names_from = env, values_from = c(n_plot, mean, se), names_sep = "_") |>
  rename(n_open = `n_plot_Open field`, n_vapv = n_plot_VAPV, mean_open = `mean_Open field`, mean_vapv = mean_VAPV,
         se_open = `se_Open field`, se_vapv = se_VAPV) |>
  left_join(qn_inf, by = "CROP")
write_tab(tbl_tissue_n, "tbl_tissue_n")

# =============================================================================
# 2. NITRATE LEACHING (Drainage-Linear method, suction cups at 1 m, Daisy drainage)
# =============================================================================
nl <- rd("no3_leaching_result")
swl <- rd("soil_water_long")
cup_nest <- swl |> distinct(suction_cup, sub_strip)

ROT_LABEL <- c(A2 = "Strip A2 (winter wheat)", A3 = "Strip A3 (grass-clover / barley / ryegrass)")
tot <- nl$total |>
  left_join(cup_nest, by = "suction_cup") |>
  mutate(env = env_from_loc(location), pos = pos_from_sub(sub_strip_location), nest = sub_strip,
         conc_flow = 100 * N_LI_kgNha / drainage_mm)          # mg N L-1 (kg N/ha per mm drainage x 100)
stopifnot(!any(is.na(tot$nest)))
win <- range(nl$daily$date)
write_tab(rd("no3_dl_vs_daisy_validation") |> transmute(strip = daisy_col, n_days = n, r2, nse = NSE, pbias = PBIAS), "tbl_dl_vs_daisy")

tbl_no3_strip <- tot |> group_by(strip, env) |>
  summarise(n_cup = n(), n_nest = n_distinct(nest), mean = mean(N_LI_kgNha), se = se(N_LI_kgNha),
            median = median(N_LI_kgNha), drainage_mm = mean(drainage_mm),
            conc = mean(conc_flow), .groups = "drop")
no3_nest <- tot |> group_by(strip, env, nest) |> summarise(v = mean(N_LI_kgNha), .groups = "drop")
no3_inf <- no3_nest |> group_by(strip) |> group_modify(~ welch_ratio(.x)) |> ungroup() |>
  select(strip, nn_open = n_open, nn_vapv = n_vapv, pct, pct_lcl, pct_ucl, p, df)
# Both strips together: nest-level linear model on log(N) with strip as a fixed factor (exact t intervals)
m_no3 <- lm(log(v) ~ env + strip, data = no3_nest)
ci <- confint(m_no3)["envVAPV", ]
no3_both <- tibble(strip = "Both", nn_open = sum(no3_nest$env == "Open field"), nn_vapv = sum(no3_nest$env == "VAPV"),
                   pct = 100 * (exp(coef(m_no3)["envVAPV"]) - 1), pct_lcl = 100 * (exp(ci[1]) - 1), pct_ucl = 100 * (exp(ci[2]) - 1),
                   p = summary(m_no3)$coefficients["envVAPV", 4], df = m_no3$df.residual)
tbl_no3_both_desc <- tot |> group_by(env) |>
  summarise(n_cup = n(), n_nest = n_distinct(nest), mean = mean(N_LI_kgNha), se = se(N_LI_kgNha),
            median = median(N_LI_kgNha), drainage_mm = mean(drainage_mm), conc = mean(conc_flow), .groups = "drop") |>
  mutate(strip = "Both")
tbl_no3_strip <- bind_rows(tbl_no3_strip, tbl_no3_both_desc) |>
  pivot_wider(names_from = env, values_from = c(n_cup, n_nest, mean, se, median, drainage_mm, conc), names_sep = "_") |>
  rename_with(~ str_replace_all(.x, c("Open field" = "open", "VAPV" = "vapv")), -strip) |>
  left_join(bind_rows(no3_inf, no3_both), by = "strip")
write_tab(tbl_no3_strip, "tbl_no3_strip")

# By position (substrip)
tbl_no3_pos <- tot |> group_by(strip, env, pos) |>
  summarise(n_cup = n(), mean = mean(N_LI_kgNha), se = se(N_LI_kgNha), .groups = "drop")
write_tab(tbl_no3_pos, "tbl_no3_position")
no3_pos_delta <- tot |> group_by(strip, pos) |> group_modify(~ welch_ratio(.x |> mutate(v = N_LI_kgNha))) |> ungroup() |>
  mutate(pct_means = 100 * (mean_vapv / mean_open - 1))
write_tab(no3_pos_delta, "tbl_no3_position_delta")

# By drainage season
SEASONS <- c("W1" = "Winter 2024/25 (13 Sep-30 Apr)", "GS" = "Growing season 2025 (1 May-30 Sep)", "W2" = "Winter 2025/26 (1 Oct-1 Apr)")
daily <- nl$daily |>
  left_join(cup_nest, by = "suction_cup") |>
  mutate(env = env_from_loc(location), pos = pos_from_sub(sub_strip_location), nest = sub_strip,
         season = case_when(date <= as.Date("2025-04-30") ~ "W1", date <= as.Date("2025-09-30") ~ "GS", TRUE ~ "W2"),
         season = factor(season, names(SEASONS)))
cup_season <- daily |> group_by(strip, env, pos, nest, suction_cup, season) |>
  summarise(N = sum(N_LI), drain = sum(drainage_mm), .groups = "drop")
drain_tab <- cup_season |> group_by(strip, season) |> summarise(drain = mean(drain), .groups = "drop")
tbl_no3_season <- cup_season |> group_by(strip, env, season) |>
  summarise(n_cup = n(), mean = mean(N), se = se(N), .groups = "drop")
no3_season_inf <- cup_season |> group_by(strip, season, nest, env) |> summarise(v = mean(N), .groups = "drop") |>
  group_by(strip, season) |> group_modify(~ welch_diff(.x)) |> ungroup() |>
  select(strip, season, diff, diff_lcl, diff_ucl)
tbl_no3_season <- tbl_no3_season |>
  pivot_wider(names_from = env, values_from = c(n_cup, mean, se), names_sep = "_") |>
  rename(n_open = `n_cup_Open field`, n_vapv = n_cup_VAPV, mean_open = `mean_Open field`, mean_vapv = mean_VAPV,
         se_open = `se_Open field`, se_vapv = se_VAPV) |>
  left_join(no3_season_inf, by = c("strip", "season")) |>
  left_join(drain_tab, by = c("strip", "season")) |>
  mutate(season_label = SEASONS[as.character(season)])
write_tab(tbl_no3_season, "tbl_no3_season")

# Cumulative curves (mean +- SE across cups)
cum_cup <- daily |> arrange(suction_cup, date) |> group_by(suction_cup) |>
  mutate(cum = cumsum(N_LI)) |> ungroup()
no3_cum <- cum_cup |> group_by(strip, env, date) |>
  summarise(mean = mean(cum), se = se(cum), n = n(), .groups = "drop")
no3_cum_pos <- cum_cup |> group_by(strip, env, pos, date) |> summarise(mean = mean(cum), .groups = "drop")
write_tab(no3_cum, "tbl_no3_cumulative_series")

# Suction-cup NO3-N concentrations (mg N L-1) and the 11.3 mg N L-1 drinking-water threshold (50 mg NO3 L-1)
add_qc("NO3 leaching", "Suction-cup samples with no concentration value", sum(is.na(swl$no3_mg_l)),
       "Not used in concentration summaries; the DL method interpolates linearly between valid samples")
add_qc("NO3 leaching", "Suction-cup samples flagged below detection limit", sum(swl$below_dl, na.rm = TRUE),
       "Kept at the value stored by the pipeline (not set to zero)")
conc <- swl |> filter(!is.na(no3_mg_l)) |>
  mutate(env = env_from_loc(location), strip = str_extract(main_strip, "A[0-9]"))
tbl_no3_conc <- conc |> group_by(strip, env) |>
  summarise(n = n(), mean = mean(no3_mg_l), median = median(no3_mg_l), p90 = quantile(no3_mg_l, 0.9),
            share_above_11_3 = 100 * mean(no3_mg_l > 11.3), .groups = "drop")
write_tab(tbl_no3_conc, "tbl_no3_concentration")

# =============================================================================
# 3. SOIL WATER (TDR volumetric water content, 20 cm and 50 cm probes)
# =============================================================================
fs <- list.files(file.path(ROOT, "1_input_data", "tdr_data"), pattern = "\\.csv$", full.names = TRUE)
tdr_raw <- map_dfr(fs, function(f) {
  d <- suppressMessages(read_csv2(f, col_types = cols(.default = "c"), show_col_types = FALSE)); d$file <- basename(f); d })
num <- function(x) suppressWarnings(as.numeric(str_replace(x, ",", ".")))
tdr <- tdr_raw |>
  transmute(file, fdate = dmy(str_extract(file, "[0-9]{6}")), nest = Sub_strip, plot = Plot_no,
            probe = num(Probe_Length_m), v = num(Soil_Water_Content_pct), year_meta = Year)
add_qc("Soil water (TDR)", "Records missing a measurement-year field (date recovered from file name)",
       sum(is.na(tdr$year_meta)), "Date taken from the ddmmyy in the file name for all records")
add_qc("Soil water (TDR)", "Records with no water-content value", sum(is.na(tdr$v)), "Dropped")
tdr <- tdr |> filter(!is.na(v), probe %in% c(0.2, 0.5)) |>
  mutate(depth = factor(if_else(probe == 0.2, "20 cm probe", "50 cm probe"), c("20 cm probe", "50 cm probe")),
         key = paste(fdate, nest, plot, probe))
dup_keys <- tdr |> count(key) |> filter(n > 1)
add_qc("Soil water (TDR)", "Date x nest x plot x probe combinations recorded in more than one file (crop-labelled copies)",
       nrow(dup_keys), "Averaged to one value per combination; sensitivity check using first record only is in tbl_tdr_overall")
tdr1 <- tdr |> group_by(fdate, nest, plot, depth) |> summarise(v = mean(v), .groups = "drop")
tdr1_first <- tdr |> group_by(key) |> slice(1) |> ungroup() |>
  group_by(fdate, nest, plot, depth) |> summarise(v = mean(v), .groups = "drop")
add_qc("Soil water (TDR)", "Water-content values above 45 vol-% (near saturation for this sandy soil)",
       sum(tdr1$v > 45), "Retained (physically possible when wet); flagged only")

tdr_nd_fn <- function(d) {
  d |> group_by(fdate, nest, depth) |> summarise(v = mean(v), .groups = "drop") |>
    add_nest_attr() |>
    mutate(year = year(fdate))
}
tdr_nd <- tdr_nd_fn(tdr1)
tdr_nd_first <- tdr_nd_fn(tdr1_first)

# Paired difference per date x strip x depth: mean of VAPV nests minus the open-field nest of the same strip
pair_fn <- function(nd) {
  open <- nd |> filter(env == "Open field") |> select(fdate, strip, depth, open = v)
  nd |> filter(env == "VAPV") |> left_join(open, by = c("fdate", "strip", "depth")) |> filter(!is.na(open)) |>
    mutate(diff = v - open, ratio = 100 * v / open, year = year(fdate))
}
pairs <- pair_fn(tdr_nd)
pairs_first <- pair_fn(tdr_nd_first)
date_diff <- function(p) p |> group_by(depth, year, fdate) |>
  summarise(diff = mean(diff), ratio = mean(ratio), open = mean(open), vapv = mean(v), .groups = "drop")
dd <- date_diff(pairs); dd_first <- date_diff(pairs_first)
write_tab(dd, "tbl_tdr_date_series")

date_summary <- function(dd) {
  f <- function(d) {
    tt <- t.test(d$diff)
    tibble(n_date = nrow(d), mean_open = mean(d$open), mean_vapv = mean(d$vapv), diff = mean(d$diff),
           lcl = tt$conf.int[1], ucl = tt$conf.int[2], n_pos = sum(d$diff > 0), share_pos = 100 * mean(d$diff > 0),
           min_date = min(d$fdate), max_date = max(d$fdate))
  }
  bind_rows(dd |> group_by(depth, year) |> group_modify(~ f(.x)) |> ungroup() |> mutate(year = as.character(year)),
            dd |> group_by(depth) |> group_modify(~ f(.x)) |> ungroup() |> mutate(year = "All"))
}
tbl_tdr_date <- date_summary(dd) |> left_join(
  date_summary(dd_first) |> select(depth, year, diff_first = diff), by = c("depth", "year"))
write_tab(tbl_tdr_date, "tbl_tdr_overall")

# Nest-level (conservative) contrast: mean over all dates per nest, 3 open vs 9 VAPV nests
tdr_nest <- tdr_nd |> group_by(depth, nest, strip, env, pos) |> summarise(v = mean(v), n_date = n(), .groups = "drop")
tbl_tdr_nest <- tdr_nest |> group_by(depth) |> group_modify(~ bind_cols(welch_diff(.x), welch_ratio(.x) |> select(pct, pct_lcl, pct_ucl))) |> ungroup()
write_tab(tbl_tdr_nest, "tbl_tdr_nest_level")

# By position: VAPV W/C/E nests vs open-field centre nest of the same strip
pos_date <- pairs |> group_by(depth, pos, fdate) |>
  summarise(diff = mean(diff), ratio = mean(ratio), v = mean(v), open = mean(open), .groups = "drop")
tbl_tdr_pos <- pos_date |> group_by(depth, pos) |>
  summarise(n_date = n(), mean_vapv = mean(v), mean_open = mean(open), diff_mean = mean(diff), diff_se = se(diff),
            diff_lcl = t.test(diff)$conf.int[1], diff_ucl = t.test(diff)$conf.int[2],
            share_pos = 100 * mean(diff > 0), ratio = mean(ratio), .groups = "drop")
write_tab(tbl_tdr_pos, "tbl_tdr_position")
tdr_pos_levels <- tdr_nd |> group_by(depth, env, pos) |>
  summarise(n_nest = n_distinct(nest), mean = mean(v), se = se(v), .groups = "drop")
write_tab(tdr_pos_levels, "tbl_tdr_position_levels")

# Annual (per growing season) VWC means by env, for context
tbl_tdr_season_lvl <- tdr_nd |> group_by(depth, year, env) |> summarise(n_nest = n_distinct(nest), mean = mean(v), .groups = "drop")
write_tab(tbl_tdr_season_lvl, "tbl_tdr_year_levels")

# =============================================================================
# 4. SOIL CARBON
# =============================================================================
soc <- rd("iung_soil_analysis_ready") |>
  mutate(env = env_from_loc(LOCATION), pos = pos_from_sub(SUB_STRIP_LOCATION),
         round = factor(recode(SURVEY_ROUND, `2024_baseline` = "2024 survey", `2025_T1` = "2025 T1", `2025_T2` = "2025 T2"),
                        c("2024 survey", "2025 T1", "2025 T2")),
         soc = SOC_PCT, cn = C_PCT / N_PCT)
chk <- soc |> distinct(nest, env, pos) |> left_join(nest_attr(unique(soc$nest)), by = "nest", suffix = c("", ".rule"))
stopifnot(all(chk$env == chk$env.rule), all(chk$pos == chk$pos.rule))

tbl_soc_round <- soc |> group_by(round, env) |>
  summarise(n_sample = n(), n_nest = n_distinct(nest), mean = mean(soc), se = se(soc), .groups = "drop")
soc_nest <- soc |> group_by(round, env, nest, pos) |> summarise(v = mean(soc), .groups = "drop")
soc_inf <- soc_nest |> group_by(round) |> group_modify(~ bind_cols(welch_diff(.x), welch_ratio(.x) |> select(pct, pct_lcl, pct_ucl))) |> ungroup() |>
  select(round, n_nest_open = n_open, n_nest_vapv = n_vapv, diff, diff_lcl, diff_ucl, pct, pct_lcl, pct_ucl, p)
# pooled over rounds: nest means over all rounds
soc_nest_all <- soc |> group_by(env, nest, pos) |> summarise(v = mean(soc), .groups = "drop")
soc_all_inf <- bind_cols(welch_diff(soc_nest_all), welch_ratio(soc_nest_all) |> select(pct, pct_lcl, pct_ucl)) |>
  transmute(round = "All rounds (nest means)", n_nest_open = n_open, n_nest_vapv = n_vapv, diff, diff_lcl, diff_ucl, pct, pct_lcl, pct_ucl, p)
tbl_soc_round <- tbl_soc_round |>
  pivot_wider(names_from = env, values_from = c(n_sample, n_nest, mean, se), names_sep = "_") |>
  rename(n_open = `n_sample_Open field`, n_vapv = n_sample_VAPV, mean_open = `mean_Open field`, mean_vapv = mean_VAPV,
         se_open = `se_Open field`, se_vapv = se_VAPV) |>
  select(-starts_with("n_nest_")) |>
  mutate(round = as.character(round)) |>
  left_join(soc_inf |> mutate(round = as.character(round)), by = "round") |>
  bind_rows(soc_all_inf |> mutate(n_open = sum(soc$env == "Open field"), n_vapv = sum(soc$env == "VAPV"),
                                  mean_open = mean(soc$soc[soc$env == "Open field"]), mean_vapv = mean(soc$soc[soc$env == "VAPV"]),
                                  se_open = se(soc$soc[soc$env == "Open field"]), se_vapv = se(soc$soc[soc$env == "VAPV"])))
write_tab(tbl_soc_round, "tbl_soc_round")

tbl_soc_pos <- soc |> group_by(round, env, pos) |>
  summarise(n_sample = n(), mean = mean(soc), se = se(soc), .groups = "drop")
write_tab(tbl_soc_pos, "tbl_soc_position")
soc_pos_delta <- soc |> group_by(pos) |> group_modify(~ welch_diff(.x |> mutate(v = soc))) |> ungroup()
# Gap between VAPV and open field by round (difference of nest means) and its growth since the 2024 survey (nest-level)
nest_round <- soc |> group_by(round, env, nest) |> summarise(v = mean(soc), .groups = "drop")
nest_wide <- nest_round |> filter(round %in% c("2024 survey", "2025 T2")) |>
  pivot_wider(names_from = round, values_from = v) |> filter(!is.na(`2024 survey`), !is.na(`2025 T2`)) |>
  mutate(chg = `2025 T2` - `2024 survey`)
did <- welch_diff(nest_wide |> mutate(v = chg))
tbl_soc_did <- did |> transmute(n_nest_open = n_open, n_nest_vapv = n_vapv,
                                chg_open = mean_open, chg_vapv = mean_vapv, did = diff, did_lcl = diff_lcl, did_ucl = diff_ucl, p = p)
write_tab(tbl_soc_did, "tbl_soc_did")
add_qc("Soil carbon (IUNG)", "Samples in the IUNG dataset", nrow(soc),
       "Every sample code is a distinct physical point; rounds are NOT repeated measures at the same spot")
add_qc("Soil carbon (IUNG)", "Samples with fertiliser identity resolved", sum(!is.na(soc$FERTILISATION)),
       "Fertiliser / residue effects on SOC not analysed (too sparse, confounded with survey round)")

# Baseline soil survey (Aarhus University lab, winter 2024-25): total C by depth, 30 nests
ss <- read_excel(file.path(ROOT, "1_input_data", "soil_survey", "SOIL_SURVEY_DATA_COMBINED.xlsx"), sheet = "COMBINED_CLEAN")
names(ss) <- c("AE_id", "kunde", "fill", "profil", "substrip", "plot", "depth", "lat", "lon", "clay", "silt", "coarse_silt",
               "fine_sand", "coarse_sand", "humus", "caco3", "jb", "total_c")
base <- ss |> filter(str_detect(substrip, "^N[0-9]+$"), depth %in% c("0-30", "30-60", "60-")) |>
  mutate(across(c(clay, total_c), num), nest = substrip) |>
  add_nest_attr() |>
  mutate(depth = factor(recode(depth, `0-30` = "0-30 cm", `30-60` = "30-60 cm", `60-` = ">60 cm"), c("0-30 cm", "30-60 cm", ">60 cm")))
# The survey covers 20 vertical-PV-trial nests (West and East positions only; no centre nests) plus
# 10 "S" nests belonging to the south-oriented PV setup, which are outside this report's scope.
stopifnot(n_distinct(base$nest) == 20, !any(base$pos == "Centre"))
tbl_base_c <- base |> group_by(depth) |> group_modify(~ bind_cols(
  welch_diff(.x |> mutate(v = total_c)),
  welch_ratio(.x |> mutate(v = total_c)) |> select(pct, pct_lcl, pct_ucl))) |> ungroup()
tbl_base_c_pos <- base |> group_by(depth, env, pos) |> summarise(n = n(), mean = mean(total_c), se = se(total_c), .groups = "drop")
tbl_base_clay <- base |> group_by(depth, env) |> summarise(n = n(), clay = mean(clay), total_c = mean(total_c), .groups = "drop")
write_tab(tbl_base_c, "tbl_baseline_totalC")
write_tab(tbl_base_c_pos, "tbl_baseline_totalC_position")
write_tab(tbl_base_clay, "tbl_baseline_texture")

# =============================================================================
# 5. HEADLINE TABLE
# =============================================================================
evidence <- function(lcl, ucl) case_when(is.na(lcl) ~ "n/a", lcl > 0 ~ "VAPV higher (CI excludes 0)", ucl < 0 ~ "VAPV lower (CI excludes 0)",
                                          TRUE ~ "No clear difference (CI includes 0)")
hl_yield <- tbl_yield_overall |> transmute(
  domain = "Crop productivity", metric = if_else(CROP == "ALL", "Relative yield, all crops", paste0("Yield, ", tolower(label))),
  unit = "% of open field", open = NA_real_, vapv = NA_real_, delta = NA_real_,
  pct, lcl = pct_lcl, ucl = pct_ucl, p, n_open = n_open, n_vapv = n_vapv, unit_n = "nests")
hl_no3 <- tbl_no3_strip |> transmute(
  domain = "Nitrate leaching", metric = case_when(strip == "A2" ~ "NO3-N leached, strip A2 (winter wheat)", strip == "A3" ~ "NO3-N leached, strip A3 (grass-clover / barley)", TRUE ~ "NO3-N leached, both strips"),
  unit = "kg N/ha", open = mean_open, vapv = mean_vapv, delta = mean_vapv - mean_open, pct, lcl = pct_lcl, ucl = pct_ucl, p,
  n_open = nn_open, n_vapv = nn_vapv, unit_n = "nests")
hl_tdr <- tbl_tdr_nest |> transmute(
  domain = "Soil water", metric = paste0("Soil water content, ", depth), unit = "vol-%", open = mean_open, vapv = mean_vapv,
  delta = diff, pct, lcl = pct_lcl, ucl = pct_ucl, p, n_open, n_vapv, unit_n = "nests")
hl_soc <- tbl_soc_round |> filter(round == "All rounds (nest means)") |> transmute(
  domain = "Soil carbon", metric = "SOC, IUNG (all rounds)", unit = "% SOC", open = mean_open, vapv = mean_vapv, delta = diff,
  pct, lcl = pct_lcl, ucl = pct_ucl, p, n_open = n_nest_open, n_vapv = n_nest_vapv, unit_n = "nests")
hl_base <- tbl_base_c |> filter(depth == "0-30 cm") |> transmute(
  domain = "Soil carbon", metric = "Total C 0-30 cm, baseline survey", unit = "% C", open = mean_open, vapv = mean_vapv, delta = diff,
  pct, lcl = pct_lcl, ucl = pct_ucl, p, n_open, n_vapv, unit_n = "nests")
tbl_headline <- bind_rows(hl_yield, hl_no3, hl_tdr, hl_soc, hl_base) |>
  mutate(evidence = evidence(lcl, ucl))
write_tab(tbl_headline, "tbl_headline")

# =============================================================================
# 6. DATA COVERAGE + QC LOG
# =============================================================================
cov_rows <- list(
  tibble(domain = "Crop productivity", data = "Dry-matter yield of primary product (grain; biomass for grass-clover, barley, ryegrass)",
         period = "2023-2025 seasons", n = sprintf("%d plot-yields (%d crop-years)", nrow(yy), n_distinct(yy$cy)),
         unit = "Plot (1 of 4 treatment plots per nest)", note = "Winter wheat 2025 not yet entered; grass-clover 2023 = 3 cuts only"),
  tibble(domain = "Crop productivity", data = "Total N in harvested product (tissue N, %)", period = "2024 only",
         n = sprintf("%d plot values", nrow(qn)), unit = "Plot", note = "Winter wheat, soybean, grass-clover only"),
  tibble(domain = "Nitrate leaching", data = "Suction-cup NO3-N at 1 m + Daisy drainage (Drainage-Linear method)",
         period = sprintf("%s to %s", fmt_en_date(win[1]), fmt_en_date(win[2])),
         n = sprintf("%d cups, %d sampling dates", n_distinct(tot$suction_cup), n_distinct(swl$date)),
         unit = "Cup (4 per nest; 12 nests)", note = "Only strips A2 and A3; one drainage series per strip (same for VAPV and open field)"),
  tibble(domain = "Soil water", data = "TDR volumetric water content (20 and 50 cm probes)",
         period = sprintf("%d paired sampling dates, Mar-Aug 2024 and May-Sep 2025", n_distinct(dd$fdate)),
         n = sprintf("%d nest-date values, %d nests", nrow(tdr_nd), n_distinct(tdr_nd$nest)),
         unit = "Nest (2-4 plots each)", note = "Open field = centre-position nests only (3 nests); no winter data"),
  tibble(domain = "Soil carbon", data = "Soil organic carbon, IUNG laboratory (3 survey rounds)", period = "2024 survey; 2025 T1 and T2",
         n = sprintf("%d samples, %d nests", nrow(soc), n_distinct(soc$nest)), unit = "Sample (distinct points per round)",
         note = "Different physical points each round; no sampling dates in dataset"),
  tibble(domain = "Soil carbon", data = "Baseline soil survey, total C by depth (Aarhus University lab)", period = "Winter 2024-25",
         n = sprintf("%d nests x 3 depths", n_distinct(base$nest)), unit = "Nest",
         note = "West and East nests only (no centre); single survey, no repeat")
)
write_tab(bind_rows(cov_rows), "tbl_data_coverage")
write_tab(bind_rows(qc), "tbl_qc")

cat("\n==== DONE: tables written to", TAB, "====\n")
