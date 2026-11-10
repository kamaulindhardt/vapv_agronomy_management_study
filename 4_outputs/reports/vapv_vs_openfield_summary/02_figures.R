# Builds the six report figures (PNG, 300 dpi) from the objects created in 01_data_and_tables.R.
# Colour = environment (project palette: open field teal, VAPV red); shape repeats the colour
# distinction (circle vs triangle) so no figure relies on colour alone. Individual observations are
# shown as faint points behind every mean +- SE.

scale_env <- list(scale_colour_manual(values = ENV_COL, name = NULL),
                  scale_shape_manual(values = ENV_SHAPE, name = NULL))
fill_env <- scale_fill_manual(values = ENV_COL, name = NULL)
mon_lab <- function(x) paste0(month.abb[as.integer(format(x, "%m"))], "\n", format(x, "%Y"))
daymon_lab <- function(x) paste0(as.integer(format(x, "%d")), " ", month.abb[as.integer(format(x, "%m"))])
DODGE <- 0.6
jd <- function(jw = 0.08) position_jitterdodge(jitter.width = jw, jitter.height = 0, dodge.width = DODGE, seed = 11)
pd <- position_dodge(width = DODGE)

# ------------------------------------------------------------------ Figure 1: yield by crop-year
cy_label <- c("WW 2023" = "Winter wheat 2023", "WW 2024" = "Winter wheat 2024", "SY 2024" = "Soybean 2024",
              "GC 2023" = "Grass-clover 2023\n(3 cuts)", "GC 2024" = "Grass-clover 2024\n(5 cuts)",
              "GC 2025" = "Grass-clover 2025\n(5 cuts)", "SB 2025" = "Spring barley 2025\n(green biomass)",
              "RG 2025" = "Ryegrass 2025\n(undersown)", "ALL" = "All crops\n(pooled)")
cy9 <- c(CY_LV, "ALL")

pA <- ggplot(yy, aes(x = cy, y = dm, colour = env, shape = env, group = env)) +
  geom_point(position = jd(0.08), alpha = 0.35, size = 1.1) +
  geom_pointrange(data = cy_desc, aes(y = mean, ymin = mean - se, ymax = mean + se), position = pd,
                  size = 0.5, linewidth = 0.7) +
  scale_x_discrete(limits = rev(cy9), labels = cy_label) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_env + coord_flip() +
  labs(x = NULL, y = "Dry-matter yield (Mg/ha)", title = "Yield by crop and year") +
  theme_rep()

dB <- bind_rows(
  tbl_yield_cy |> transmute(cy = as.character(cy), pct, lcl = pct_lcl, ucl = pct_ucl),
  tbl_yield_overall |> filter(CROP == "ALL") |> transmute(cy = "ALL", pct, lcl = pct_lcl, ucl = pct_ucl)) |>
  mutate(is_all = cy == "ALL")
pB <- ggplot(dB, aes(x = cy, y = pct, ymin = lcl, ymax = ucl)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.4) +
  geom_pointrange(aes(colour = is_all, shape = is_all), size = 0.5, linewidth = 0.7) +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey35"), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 18, `FALSE` = 16), guide = "none") +
  scale_x_discrete(limits = rev(cy9), labels = NULL) +
  scale_y_continuous(breaks = seq(-60, 80, 20)) +
  coord_flip() +
  labs(x = NULL, y = "VAPV vs open field (%, 95% CI)", title = "Difference") +
  theme_rep() + theme(axis.ticks.y = element_blank())

fig1 <- (pA + pB + plot_layout(widths = c(1.55, 1), guides = "collect")) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "top", plot.tag = element_text(face = "bold", size = 10))
save_fig(fig1, "fig1_yield", 7.0, 5.0)

# ------------------------------------------------------------------ Figure 2: yield by position (substrip)
dpos <- tbl_yield_pos_delta |> mutate(cy = factor(cy, CY_LV), pos = factor(pos, POS_LV),
                                      lab = sprintf("%+.0f%%", pct_means))
fig2 <- ggplot(yy, aes(x = pos, y = dm, colour = env, shape = env, group = env)) +
  geom_point(position = jd(0.1), alpha = 0.35, size = 1) +
  geom_pointrange(data = tbl_yield_pos |> mutate(cy = factor(cy, CY_LV)),
                  aes(y = mean, ymin = mean - se, ymax = mean + se), position = pd, size = 0.4, linewidth = 0.6) +
  geom_text(data = dpos, aes(x = pos, y = Inf, label = lab), inherit.aes = FALSE, vjust = 1.4, size = 2.5, colour = INK2) +
  facet_wrap(~ cy, nrow = 2, scales = "free_y",
             labeller = labeller(cy = function(x) c("WW 2023" = "Winter wheat 2023", "WW 2024" = "Winter wheat 2024", "SY 2024" = "Soybean 2024",
                                                    "GC 2023" = "Grass-clover 2023 (3 cuts)", "GC 2024" = "Grass-clover 2024 (5 cuts)",
                                                    "GC 2025" = "Grass-clover 2025 (5 cuts)", "SB 2025" = "Spring barley 2025",
                                                    "RG 2025" = "Ryegrass 2025")[x])) +
  scale_env +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(x = "Position across the strip", y = "Dry-matter yield (Mg/ha)") +
  theme_rep(8.5) + theme(strip.text = element_text(size = 7.5, face = "bold"))
save_fig(fig2, "fig2_yield_position", 7.0, 4.9)

# ------------------------------------------------------------------ Figure 3: nitrate leaching
strip_lab <- c(A2 = "Strip A2: winter wheat", A3 = "Strip A3: grass-clover / barley / ryegrass")
events <- tribble(~strip, ~date, ~label,
                  "A2", "2024-09-23", "Wheat sown", "A2", "2025-08-12", "Wheat harvested",
                  "A3", "2025-04-10", "Barley sown", "A3", "2025-06-25", "Barley harvested") |>
  mutate(date = as.Date(date))
season_txt <- tibble(date = as.Date(c("2024-12-20", "2025-07-15", "2026-01-05")), lab = c("Winter 24/25", "Grow. season", "Winter 25/26"))

p3a <- ggplot(no3_cum, aes(x = date, y = mean, colour = env, fill = env)) +
  geom_vline(xintercept = as.Date(c("2025-04-30", "2025-09-30")), linetype = "dotted", colour = "grey55") +
  geom_vline(data = events, aes(xintercept = date), colour = "grey70", linewidth = 0.4) +
  geom_text(data = events, aes(x = date + 6, y = 0, label = label), inherit.aes = FALSE, angle = 90, hjust = -0.1, vjust = 0.5,
            size = 2.3, colour = INK2) +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se), alpha = 0.2, colour = NA) +
  geom_line(linewidth = 0.8) +
  geom_text(data = season_txt, aes(x = date, y = Inf, label = lab), inherit.aes = FALSE, vjust = 1.5, size = 2.4, colour = INK2) +
  facet_wrap(~ strip, scales = "free_y", labeller = labeller(strip = strip_lab)) +
  scale_colour_manual(values = ENV_COL, name = NULL) + fill_env + guides(fill = "none") +
  scale_x_date(date_breaks = "4 months", labels = mon_lab, expand = expansion(mult = 0.01)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Cumulative NO3-N leached (kg N/ha)", title = "Cumulative leaching (mean ± SE of cups)") +
  theme_rep()

p3b <- ggplot(tot, aes(x = pos, y = N_LI_kgNha, colour = env, shape = env, group = env)) +
  geom_point(position = jd(0.1), alpha = 0.4, size = 1.1) +
  geom_pointrange(data = tbl_no3_pos |> mutate(pos = factor(pos, POS_LV), env = factor(env, ENV_LV)),
                  aes(y = mean, ymin = mean - se, ymax = mean + se), position = pd, size = 0.45, linewidth = 0.65) +
  facet_wrap(~ strip, scales = "free_y") + scale_env +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Position across the strip", y = "Total NO3-N leached (kg N/ha)", title = "Total by position (13 Sep 2024 - 1 Apr 2026)") +
  theme_rep()

p3c <- ggplot(tbl_no3_season |> filter(season != "GS") |>
                pivot_longer(c(mean_open, mean_vapv, se_open, se_vapv), names_to = c(".value", "env"), names_sep = "_") |>
                mutate(env = factor(if_else(env == "open", "Open field", "VAPV"), ENV_LV),
                       season = factor(season, c("W1", "W2"), c("Winter\n2024/25", "Winter\n2025/26"))),
              aes(x = season, y = mean, colour = env, shape = env, group = env)) +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se), position = pd, size = 0.5, linewidth = 0.7) +
  facet_wrap(~ strip, scales = "free_y") + scale_env +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "NO3-N leached (kg N/ha)", title = "By drainage season (mean ± SE)") +
  theme_rep()

fig3 <- p3a / ((p3b + theme(legend.position = "none")) | (p3c + theme(legend.position = "none"))) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 10))
save_fig(fig3, "fig3_no3", 7.0, 6.6)

# ------------------------------------------------------------------ Figure 4: soil water (TDR)
ts <- dd |> pivot_longer(c(open, vapv), names_to = "env", values_to = "vwc") |>
  mutate(env = factor(if_else(env == "open", "Open field", "VAPV"), ENV_LV))
ts <- ts |> arrange(depth, year, env, fdate) |> group_by(depth, year, env) |>
  mutate(seg = paste(env, cumsum(c(TRUE, diff(fdate) > 25)))) |> ungroup()   # break lines across gaps > 25 days
p4a <- ggplot(ts, aes(x = fdate, y = vwc, colour = env, shape = env, group = seg)) +
  geom_line(linewidth = 0.5) + geom_point(size = 1.3) +
  facet_grid(depth ~ year, scales = "free_x", space = "free_x") + scale_env +
  scale_x_date(date_breaks = "1 month", labels = daymon_lab) +
  labs(x = NULL, y = "Soil water content (vol-%)", title = "Soil water content (mean over strips, plots and nests)") +
  theme_rep() + theme(axis.text.x = element_text(angle = 0, size = 6.5))

mean_d <- tbl_tdr_date |> filter(year != "All") |> mutate(year = as.integer(year))
p4b <- ggplot(dd, aes(x = fdate, y = diff)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.4) +
  geom_hline(data = mean_d, aes(yintercept = diff), linetype = "dashed", colour = ENV_COL[["VAPV"]], linewidth = 0.5) +
  geom_segment(aes(xend = fdate, yend = 0), colour = "grey55", linewidth = 0.4) +
  geom_point(colour = ENV_COL[["VAPV"]], shape = 17, size = 1.5) +
  geom_text(data = mean_d, aes(x = min_date, y = Inf, label = sprintf("mean %+.1f pp; VAPV wetter on %d of %d dates", round(diff, 1) + 0, n_pos, n_date)),
            inherit.aes = FALSE, hjust = 0, vjust = 1.5, size = 2.4, colour = INK2) +
  facet_grid(depth ~ year, scales = "free_x", space = "free_x") +
  scale_x_date(date_breaks = "1 month", labels = daymon_lab) +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.22))) +
  labs(x = NULL, y = "VAPV minus open field (pp)", title = "Paired difference per date (VAPV nests minus open-field nest of the same strip)") +
  theme_rep() + theme(axis.text.x = element_text(size = 6.5))

p4c <- ggplot(tbl_tdr_pos |> mutate(pos = factor(pos, POS_LV)), aes(x = pos, y = diff_mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ENV_COL[["Open field"]], linewidth = 0.6) +
  geom_pointrange(aes(ymin = diff_lcl, ymax = diff_ucl), colour = ENV_COL[["VAPV"]], shape = 17, size = 0.55, linewidth = 0.7) +
  facet_wrap(~ depth) +
  labs(x = "VAPV position across the strip", y = "VAPV minus open-field centre (pp)",
       title = "By VAPV position (mean over dates, 95% CI; dashed = open-field centre nest)") +
  theme_rep()

fig4 <- p4a / p4b / p4c + plot_layout(heights = c(1.15, 1, 0.85)) +
  plot_annotation(tag_levels = "A") & theme(legend.position = "top", plot.tag = element_text(face = "bold", size = 10))
save_fig(fig4, "fig4_soilwater", 7.0, 8.4)

# ------------------------------------------------------------------ Figure 5: soil carbon
p5a <- ggplot(soc, aes(x = round, y = soc, colour = env, shape = env, group = env)) +
  geom_point(position = jd(0.1), alpha = 0.35, size = 1.1) +
  geom_pointrange(data = tbl_soc_round |> filter(!str_detect(round, "^All")) |>
                    pivot_longer(c(mean_open, mean_vapv, se_open, se_vapv), names_to = c(".value", "env"), names_sep = "_") |>
                    mutate(env = factor(if_else(env == "open", "Open field", "VAPV"), ENV_LV), round = factor(round, levels(soc$round))),
                  aes(y = mean, ymin = mean - se, ymax = mean + se), position = pd, size = 0.5, linewidth = 0.7) +
  scale_env + labs(x = NULL, y = "Soil organic carbon (% of soil)", title = "SOC by survey round (IUNG)") + theme_rep()

p5c <- ggplot(base, aes(x = depth, y = total_c, colour = env, shape = env, group = env)) +
  geom_point(position = jd(0.1), alpha = 0.4, size = 1.1) +
  geom_pointrange(data = base |> group_by(depth, env) |> summarise(m = mean(total_c), s = se(total_c), .groups = "drop"),
                  aes(y = m, ymin = m - s, ymax = m + s), position = pd, size = 0.5, linewidth = 0.7) +
  scale_env + labs(x = "Soil depth", y = "Total carbon (% of soil)", title = "Baseline survey: total C by depth") + theme_rep()

p5b <- ggplot(soc, aes(x = pos, y = soc, colour = env, shape = env, group = env)) +
  geom_point(position = jd(0.1), alpha = 0.35, size = 1) +
  geom_pointrange(data = tbl_soc_pos |> mutate(pos = factor(pos, POS_LV), env = factor(env, ENV_LV), round = factor(round, levels(soc$round))),
                  aes(y = mean, ymin = mean - se, ymax = mean + se), position = pd, size = 0.45, linewidth = 0.65) +
  facet_wrap(~ round) + scale_env +
  labs(x = "Position across the strip", y = "Soil organic carbon (% of soil)", title = "SOC by position, within each survey round") + theme_rep()

fig5 <- ((p5a + theme(legend.position = "top")) | (p5c + theme(legend.position = "none"))) /
  (p5b + theme(legend.position = "none")) + plot_layout(heights = c(1, 0.9)) +
  plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 10))
save_fig(fig5, "fig5_soc", 7.0, 6.0)

# ------------------------------------------------------------------ Figure 6: substrip synthesis
rel_cell <- function(df, outcome) df |> group_by(env, pos) |>
  summarise(n = n(), mean = mean(rel), se = se(rel), .groups = "drop") |> mutate(outcome = outcome)
r_y <- yy |> group_by(cy) |> mutate(rel = 100 * dm / mean(dm[env == "Open field"])) |> ungroup() |> rel_cell("Crop yield\n(8 crop-years)")
r_n <- tot |> group_by(strip) |> mutate(rel = 100 * N_LI_kgNha / mean(N_LI_kgNha[env == "Open field"])) |> ungroup() |> rel_cell("NO3-N leaching\n(strips A2 and A3)")
r_t <- pairs |> group_by(env, pos, nest) |> summarise(rel = mean(ratio), .groups = "drop") |> rel_cell("Soil water content\n(TDR, both probes)")
r_t <- bind_rows(r_t, tibble(env = factor("Open field", ENV_LV), pos = factor("Centre", POS_LV), n = 3L, mean = 100, se = NA_real_,
                             outcome = "Soil water content\n(TDR, both probes)"))
r_s <- soc |> group_by(round) |> mutate(rel = 100 * soc / mean(soc[env == "Open field"])) |> ungroup() |> rel_cell("Soil organic carbon\n(IUNG, 3 rounds)")
syn <- bind_rows(r_y, r_n, r_t, r_s) |>
  mutate(x = factor(paste(if_else(env == "VAPV", "VAPV", "Open"), c(West = "W", Centre = "C", East = "E")[as.character(pos)]), POS6_LV),
         outcome = factor(outcome, unique(outcome)))
write_tab(syn |> select(outcome, env, pos, n, mean, se) |> mutate(outcome = str_replace_all(outcome, "\n", " ")), "tbl_synthesis_relative")

fig6 <- ggplot(syn, aes(x = x, y = mean, colour = env, shape = env)) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = 3.5, colour = "grey75", linewidth = 0.4) +
  geom_pointrange(aes(ymin = mean - se, ymax = mean + se), size = 0.6, linewidth = 0.8) +
  facet_wrap(~ outcome, scales = "free_y", nrow = 2) + scale_env +
  labs(x = NULL, y = "Percent of the open-field mean (100 = open field)") +
  theme_rep(9)
save_fig(fig6, "fig6_synthesis", 7.0, 5.4)

cat("\n==== DONE: figures written to", FIG, "====\n")
