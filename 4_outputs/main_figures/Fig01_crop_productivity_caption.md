**Figure 1. Crop productivity, tissue N and agronomic management response to vertical agrivoltaics
(VAPV).** For each crop's primary harvestable product (WW/SY: grain; GC/RG/SB: biomass). Crop is
encoded redundantly by colour AND point/line shape wherever a per-crop estimate is shown (A, B, D, E).
Panel A shows estimated marginal means (EMMs) with 95% confidence intervals from the fitted model --
NOT raw arithmetic means. Panels B-F show model-derived CONTRASTS (differences or percentage
responses) with 95% confidence intervals; a dashed reference line at 0 (0% for a percentage response)
marks no effect. No significance stars are used anywhere -- whether a CI crosses the zero/no-effect
line already conveys the inferential result.

(A) Yield (Mg DM ha⁻¹; EMM ± 95% CI) by row position (west/centre/east), shown separately for
open field and VAPV (a blank spacer plus a dashed line divides the two; header text labels which
half is which).
(B) Overall VAPV effect on yield: 100 x (VAPV − open field) / open field, i.e. the model-derived
percentage response (computed as a ratio on the model's log-link scale, then expressed as %) with
95% CI, one row per crop. The inferential counterpart of (A) from the same model
(fit_yield_pooled_model).
(C) Spatial VAPV effect on tissue N (percentage points, VAPV minus open field), by row
position (west/centre/east). POOLED ACROSS CROP, not shown per crop: the underlying model
(fit_quality_substrip_model) is `N_TOTAL ~ GLOBAL_SUB_STRIP_LOCATION + CROP`, main effects only --
an 18-cell row-position x crop interaction did not converge on these 204 rows (see that function's
docstring) and was not forced. Without that interaction, a crop-specific version of this contrast
would just repeat the same pooled number for every crop, so it is not shown. (C) and (D) are
complementary marginal views of the same VAPV-on-quality question: (C) varies row position and pools
crop; (D) varies crop and pools row position -- neither model supports both at once.
(D) Overall VAPV effect on tissue N: absolute difference (percentage POINTS, VAPV minus open field,
not a percentage response) with 95% CI, one row per crop, pooled across row position -- an absolute
difference is reported here (rather than (B)'s relative response) because the underlying model
(fit_quality_pooled_model) is gaussian (identity link, so no ratio scale applies) and because tissue N
already sits on a comparable 1.5-6.5% scale across crops, unlike yield's very different per-crop
magnitudes in (B). Tissue N data are 2024 season only, and only for the 3 crops with any N_TOTAL data
(winter wheat, soybean, grass-clover -- ryegrass and spring barley have none); this applies to (C) too.
(E) Management effects on yield, per crop: percentage response to fertiliser source (digestate vs
mineral) and residue management (retained vs removed), from fit_yield_management_model()'s
FERTILISATION:CROP and RESIDUE:CROP terms -- same % response definition as (B).
(F) VAPV x management interactions: does VAPV change the (pooled, across-crop) management response?
A genuine DIFFERENCE-IN-DIFFERENCES / interaction contrast, not a main effect: how much the fertiliser
(digestate vs mineral) and residue (retained vs removed) effects on yield differ between VAPV and
open field, expressed as a percentage (a ratio-of-ratios on the log scale). 0% = the management effect
is the same size under VAPV and open field; a CI crossing zero means limited evidence that VAPV
modifies that management response. Pooled across crop rather than per-crop: a per-crop version of
this 3-way interaction (location x management x crop) was not fit -- with only 2 x 2 x 5 = 20 cells
over ~444 rows, low-replication crops (soybean: 24 rows, spring barley: 30 rows) would average under
2 rows per cell, well below what the interaction could reliably estimate. Point/line colour is a
single neutral dark tone here (not the crop palette), since these are management-level, not
crop-level, contrasts.

Models: yield models (A/B/E/F) are Gamma(log) with row/column dispersion
(dispformula = ~ ROW + COLUMN); tissue-N models (C/D) are gaussian with no dispersion/spatial term
(too sparse for one). Reference level for every VAPV contrast is open field (OUTSIDE_PV); for
fertiliser, mineral fertiliser; for residue, removed.

Percentage-response definitions (all computed on the model's log-link scale as a ratio, then
expressed as a percentage -- not a ratio of raw arithmetic means):
  Yield response to VAPV (%)   = 100 x (VAPV - Open field) / Open field           [panel B]
  Digestate vs mineral (%)     = 100 x (Digestate - Mineral) / Mineral            [panel E]
  Retained vs removed (%)     = 100 x (Retained - Removed) / Removed             [panel E]
  VAPV x management (%)        = [management response | VAPV] - [management response | open field],
                                  as a ratio-of-ratios expressed the same way              [panel F]
