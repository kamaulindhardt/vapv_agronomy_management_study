**Figure 3. Soil organic carbon (SOC) under vertical agrivoltaics (VAPV) -- partial-data template.**
(A) Model-adjusted SOC (%) by growing environment (VAPV vs open field) and row position
(west/middle/east); faint points are individual samples.
(B) Model-adjusted SOC (%) by environment and survey round (2024 baseline; 2025 T1/T2).
IMPORTANT: survey rounds sample DIFFERENT physical points each time (no location was resampled at
the same spot) -- panel B shows SOC across survey rounds, not a paired change (Delta SOC) at any
location.
(C) Forest plot of the model's estimable effects (percentage points of SOC).
Fertiliser x residue SOC comparisons are NOT shown: fertiliser source resolves for only 40/114
samples (fully confounded with survey round) and residue management is not resolvable at this
sampling grain (as for the TDR soil-water outcome) -- see
3_analysis/R/functions_soil_chemistry.R for the coverage audit.
Model: gaussian GLMM, SOC_PCT ~ LOCATION + SUB_STRIP_LOCATION + SURVEY_ROUND +
LOCATION:SURVEY_ROUND, no random/spatial term (nest is fully crossed with the fixed effects).
Error bars are 95% confidence intervals; points are estimated marginal means, not raw means.
This figure will be rebuilt once repeated same-point SOC measurements and a resolved
fertiliser x residue design are available from IUNG.
