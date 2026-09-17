**Figure 2. Nitrate leaching under vertical agrivoltaics (VAPV), crop rotation and agronomic
management.** Based on n = 47 suction cups (Drainage-Linear method, linear interpolation; Vogeler
et al. 2020) -- every panel should be read with this modest sample size in mind; wide confidence
intervals reflect the data, not a modelling defect. West/centre/east row position uses the SAME
colour mapping in every panel that shows it (B and C).
(A) Cumulative NO₃–N leaching through time, open field vs VAPV, faceted by rotation
(WW–SY–WW, historically labelled strip A2; GC–GC–SB, strip A3) -- establishes the broad
temporal and rotational pattern. Shaded ribbon = mean ± 1 SE across suction cups -- a descriptive
summary, NOT a 95% CI (unlike the model-derived panels C-E below). Thin grey vertical lines mark
sowing and harvest of each rotation's namesake crop (winter wheat for WW–SY–WW; spring barley for
GC–GC–SB), read directly from the crop_calendar target -- not inferred or invented dates.
(B) Cumulative NO₃–N leaching through time by the 6 substrip positions (open field/VAPV x
west/centre/east row position) -- "zooms in" on the spatial mechanism (A) cannot show; deliberately
kept out of (A) to preserve a system -> substrip -> management -> synthesis visual hierarchy.
(C) Model-adjusted FINAL cumulative leaching by substrip position (estimated marginal means ± 95%
CI) -- the statistical summary of the spatial gradient (B) traces through time. Points are coloured
by ROW POSITION (matching panel B's legend exactly, not repeated here) rather than by environment:
open field vs VAPV is already communicated by the x-axis grouping and header text, so colour is used
to visually connect this panel to (B) instead. Model: fit_no3_substrip_model(), Gamma(log),
N_LI_kgNha ~ GLOBAL_SUB_STRIP_LOCATION + crop, no dispersion/spatial term (no ROW/COLUMN grid exists
for suction-cup locations).
(D) Model-adjusted leaching under the 4 fertiliser x residue combinations, stratified by CROP PHASE
(winter wheat / ryegrass) rather than by rotation: `crop` records which crop was growing on a
monitored strip at the time of measurement, not the strip's multi-year rotation identity (WW–SY–WW
or GC–GC–SB, shown in panel A) -- only 2 crop phases were ever monitored, so "by rotation" would
have mislabelled this panel. Cell counts allow this stratification (~5-6 cups per crop phase x
fertiliser x residue cell, confirmed empirically). Model: fit_no3_management_model(), Gamma(log),
N_LI_kgNha ~ crop * fertilisation * residue (full 3-way interaction, converged).
(E) Forest plot of model-derived percentage effects with 95% CI: VAPV environment, fertiliser source,
residue management (the 3 main effects, above the blank spacer row), and the supported fertiliser x
residue, VAPV x fertiliser and VAPV x residue interactions (below it). Model:
fit_no3_model_manuscript(), Gamma(log), N_LI_kgNha ~ location + fertilisation + residue + crop +
location:fertilisation + location:residue + fertilisation:residue -- the same log-link
percentage-response convention as Figure 1B/E. Dashed line at 0% = no effect; no significance stars
-- a CI crossing zero already conveys the inferential result.
Reference level for every contrast: open field (OUTSIDE_PV); mineral fertiliser; residue removed.

Percentage-effect definitions (all computed on the model's log-link scale as a ratio, then expressed
as a percentage -- not a ratio of raw arithmetic means; panel E):
  VAPV vs open field (%)   = 100 x (VAPV - Open field) / Open field
  Digestate vs mineral (%) = 100 x (Digestate - Mineral) / Mineral
  Retained vs removed (%) = 100 x (Retained - Removed) / Removed
