# Generic statistical model factory for Study 2 -- the pipeline's answer to
# the progress report's Objective 1: "Develop and deploy a generic
# statistical model for analysing outcome metrics based on explanatory
# variables such as location within the vertical agrivoltaics, fertilisation
# and crop residue treatments, including crop type, year and spatial
# autocorrelation structure."
#
# Grounded directly in the real model-fitting pattern already used and
# reported on (CROP_YIELD_ANALYSIS_2025.Rmd STEP 7-9: glmmtmb_loc /
# glmmtmb_fert, `DRY_WEIGHT_TON_HA ~ GLOBAL_SUB_STRIP_LOCATION +
# FERTILISATION + CROP_PRODUCT + YEAR, dispformula = ~ ROW + COLUMN` -- the
# model behind the draft report's "position effect: chi2 = 14.0, df = 5, p =
# 0.015" result) but GENERALISED over outcome variable and EXTENDED to
# include RESIDUE, which the source notebook derives as a column but never
# includes in a fitted formula (confirmed absent from every glmmTMB/lmer call
# in CROP_YIELD_ANALYSIS_2025.Rmd) -- closing exactly the gap the researcher's
# own draft results document flags: residue management "has not yet been
# included in the formal yield or quality statistical models."
#
# Whether RESIDUE (and its interactions) belongs in every outcome's final
# model, and which spatial_structure is the statistically correct one, are
# methods decisions for the researcher and their supervisor/statistician --
# this factory makes both easy to change (arguments, not hardcoded), it does
# not pretend to settle them.

#' Fit a generalized linear mixed model for one outcome variable, with a
#' pluggable spatial-structure treatment matching the three approaches
#' actually explored in the source notebooks:
#'
#'   "row_col_disp" (default) -- ROW + COLUMN as DISPERSION-submodel
#'      predictors, i.e. `dispformula = ~ ROW + COLUMN`. This is the pattern
#'      used in glmmtmb_loc / glmmtmb_fert, the models that survived to the
#'      draft report's headline numbers.
#'   "row_col_re" -- ROW and COLUMN as crossed random effects,
#'      `(1 | ROW) + (1 | COLUMN)`. Matches lmm_1's attempt; the source
#'      notebook's own comments flag this as prone to a singular-fit
#'      convergence warning for these data.
#'   "none" -- no explicit spatial term (fixed effects only).
#'
#' @param data an analysis-ready data frame (e.g. study2_yield_analysis_ready)
#' @param outcome name (string) of the response column
#' @param fixed_effects character vector of fixed-effect column names
#' @param interactions optional character vector of interaction terms, e.g.
#'   c("FERTILISATION:RESIDUE")
#' @param random_effects optional character vector of additional random-effect
#'   terms, e.g. "(1 | CROP/MAIN_STRIP)"
#' @param family a family object or family-generating call, e.g.
#'   Gamma(link = "log") for strictly-positive continuous outcomes,
#'   gaussian() otherwise
#' @param ... further arguments passed to glmmTMB::glmmTMB()
fit_glmm_outcome <- function(data,
                              outcome,
                              fixed_effects,
                              interactions = NULL,
                              random_effects = NULL,
                              spatial_structure = c("row_col_disp", "row_col_re", "none"),
                              family = stats::gaussian(),
                              weights = NULL,
                              ...) {
  spatial_structure <- match.arg(spatial_structure)

  missing_cols <- setdiff(c(outcome, fixed_effects), names(data))
  if (length(missing_cols) > 0) {
    stop("fit_glmm_outcome(): columns not found in data: ", paste(missing_cols, collapse = ", "))
  }

  dispformula  <- ~1
  extra_random <- NULL

  if (spatial_structure %in% c("row_col_disp", "row_col_re")) {
    if (!all(c("ROW", "COLUMN") %in% names(data))) {
      stop("spatial_structure = '", spatial_structure, "' requires ROW and COLUMN columns in data.")
    }
    if (spatial_structure == "row_col_disp") {
      dispformula <- ~ ROW + COLUMN
    } else {
      extra_random <- c("(1 | ROW)", "(1 | COLUMN)")
    }
  }

  rhs_terms <- c(fixed_effects, interactions, random_effects, extra_random)
  form <- stats::reformulate(rhs_terms, response = outcome)

  fit_args <- list(formula = form, dispformula = dispformula, family = family, data = data, ...)
  if (!is.null(weights)) fit_args$weights <- data[[weights]]

  fit <- do.call(glmmTMB::glmmTMB, fit_args)

  attr(fit, "spatial_structure") <- spatial_structure
  attr(fit, "outcome") <- outcome
  fit
}

#' DHARMa simulation-based residual diagnostics for a fitted glmmTMB model:
#' returns the simulated-residuals object plus its formal goodness-of-fit
#' tests (uniformity/KS, dispersion, outliers), matching the diagnostic
#' pattern reported alongside glmmtmb_loc (e.g. "KS p < 0.001... dispersion
#' test was not significant (p = 0.24)").
diagnose_glmm <- function(model, n_sim = 1000, seed = 1) {
  sim <- DHARMa::simulateResiduals(model, n = n_sim, seed = seed)
  list(
    sim = sim,
    tests = list(
      uniformity = DHARMa::testUniformity(sim, plot = FALSE),
      dispersion = DHARMa::testDispersion(sim, plot = FALSE),
      outliers   = DHARMa::testOutliers(sim, plot = FALSE)
    )
  )
}

#' Type II Wald chi-square ANOVA for a fitted glmmTMB model (car::Anova),
#' the significance-testing step used throughout STEP 8 of the source
#' notebook (e.g. the reported "GLOBAL_SUB_STRIP_LOCATION 14.0362 5 0.01538").
anova_glmm <- function(model, type = "II") {
  car::Anova(model, type = type)
}

#' Estimated marginal means (+ pairwise Tukey contrasts) for a fitted glmmTMB
#' model, matching STEP 9's post-hoc pattern (emmeans + pairwise comparisons
#' across treatments and PV zones).
summarize_glmm_emmeans <- function(model, spec, type = "response") {
  emm <- emmeans::emmeans(model, spec, type = type)
  list(
    emmeans = emm,
    pairwise = emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
  )
}

#' Render a Type II ANOVA table as a formatted gt table, generalised from the
#' outcome-specific make_anova_gt_table()/make_model_overview_table() helpers
#' in CROP_YIELD_ANALYSIS_2025.Rmd / BIOSTATISTICS_MOD_FOR_AG_SCIENCE.Rmd into
#' one function parameterised over outcome_label rather than hand-written per
#' outcome.
make_anova_gt_table <- function(model, outcome_label, digits = 3) {
  aov_tbl <- as.data.frame(anova_glmm(model))
  aov_tbl <- tibble::rownames_to_column(aov_tbl, var = "Term")

  aov_tbl |>
    gt::gt() |>
    gt::tab_header(
      title = paste("Type II ANOVA:", outcome_label),
      subtitle = paste0(
        "spatial structure = ", attr(model, "spatial_structure") %||% "unknown"
      )
    ) |>
    gt::fmt_number(columns = dplyr::where(is.numeric), decimals = digits) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = `Pr(>Chisq)` < 0.05)
    )
}

# ---- Per-outcome model specifications ----------------------------------------
# Thin wrappers around fit_glmm_outcome() that pin down each outcome's fixed
# effects, family and spatial structure explicitly (rather than threading
# complex list-columns through tar_map()) -- `_targets.R` maps over these by
# name via tarchetypes::tar_map(). Each one is independently testable.

#' Crop dry-matter yield: matches glmmtmb_loc/glmmtmb_fert's specification
#' (CROP_YIELD_ANALYSIS_2025.Rmd STEP 7) exactly for the main effects, EXTENDED
#' with RESIDUE and a FERTILISATION:RESIDUE interaction -- the Study 2
#' Objective 1 gap this pipeline exists to close.
fit_yield_dry_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "DRY_WEIGHT_TON_HA",
    fixed_effects = c("GLOBAL_SUB_STRIP_LOCATION", "FERTILISATION", "RESIDUE", "CROP_PRODUCT", "YEAR"),
    interactions = "FERTILISATION:RESIDUE",
    spatial_structure = "row_col_disp",
    family = Gamma(link = "log")
  )
}

#' Crop nitrogen content (N_TOTAL, %) -- quality outcome, same explanatory
#' structure as yield so the two are directly comparable, EXCEPT `YEAR`:
#' N_TOTAL is currently only populated for the 2024 season (282/606 rows have
#' a value, all of them 2024 -- confirmed empirically), so `YEAR` has a
#' single effective level once glmmTMB drops the NA rows and cannot enter a
#' contrast matrix ("contrasts can be applied only to factors with 2 or more
#' levels"). Drop it here and re-add it once 2023/2025 quality data are
#' digitised, rather than have the factory silently swallow the error.
fit_quality_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "N_TOTAL",
    fixed_effects = c("GLOBAL_SUB_STRIP_LOCATION", "FERTILISATION", "RESIDUE", "CROP_PRODUCT"),
    interactions = "FERTILISATION:RESIDUE",
    spatial_structure = "row_col_disp",
    family = stats::gaussian()
  )
}

#' Cumulative NO3-N leaching per suction cup (DL method, linear
#' interpolation). No ROW/COLUMN grid exists for suction-cup locations (they
#' follow the suction-cup installation plan, not the yield-plot grid), so
#' spatial_structure = "none". No random effect: the monitored suction cups
#' sit on only 2 field strips ("A2", the WW-SY-WW rotation; "A3", the
#' GC-GC-SB rotation -- confirmed empirically, not just a modelling
#' simplification), and `crop` is itself determined by which of those two
#' strips a cup is on -- so `(1 | strip)` aliases against the `crop` fixed
#' effect exactly as `(1 | SUB_STRIP)` did against `LOCATION` in the TDR
#' model above (same "NaN at starting parameter values" symptom, same root
#' cause). Two strips is also too few groups to usefully estimate a variance
#' component regardless. This outcome had NO formal statistical model at all
#' before this pipeline (only descriptive means +/- SE), so this is new
#' analysis, not a port.
fit_no3_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "N_LI_kgNha",
    fixed_effects = c("location", "fertilisation", "residue", "crop"),
    interactions = "fertilisation:residue",
    spatial_structure = "none",
    family = Gamma(link = "log")
  )
}

#' TDR soil water content (%). RESIDUE is not available at the TDR-nest level
#' (see derive_tdr_covariates() in functions_tdr.R) so it is deliberately
#' excluded rather than silently imputed. Like NO3 leaching, this outcome had
#' no formal statistical test before this pipeline (only descriptive plots)
#' -- the researcher's own draft results document flags this gap explicitly.
#'
#' No random effect by default: LOCATION is itself a per-nest (per-SUB_STRIP)
#' property here (every probe at a given nest shares the same LOCATION --
#' confirmed empirically: fitting with `(1 | SUB_STRIP)` produced "negative
#' log-likelihood is NaN at starting parameter values", the classic glmmTMB
#' symptom of a fixed effect aliased against its own random-intercept
#' grouping). This mirrors the source notebook's own note about SUB_STRIP
#' being unusable as a random effect for an analogous reason (crop
#' confounding) -- the right repeated-measures structure to use here (e.g.
#' SUB_STRIP:PLOT_NO, or a date-level term) is a methods question for the
#' researcher/supervisor, not one this factory should silently resolve.
#' Gamma(link="log") is used rather than gaussian() since water content is
#' strictly positive and this was also numerically more stable in practice.
fit_soil_water_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "SOIL_WATER_CONTENT",
    fixed_effects = c("LOCATION", "TREATMENT", "CROP"),
    spatial_structure = "none",
    family = Gamma(link = "log")
  )
}

#' Fit + diagnose + test + summarise one outcome model in a single call, and
#' return only portable results (data frames / numbers), never the live
#' glmmTMB fit object itself.
#'
#' This matters for `targets`: glmmTMB objects embed a TMB C++ pointer that
#' is not guaranteed to survive being written to an .rds file by one target
#' and read back by another in a fresh process (which is exactly what
#' `tar_make()` does between targets by default). Doing everything that
#' needs the live model -- DHARMa::simulateResiduals(), which calls
#' simulate() on the model -- inside one target sidesteps that risk
#' entirely; only plain data frames and summary statistics cross target
#' boundaries.
#'
#' @param data analysis-ready data frame
#' @param model_fn a fit_*_model() wrapper (e.g. fit_yield_dry_model)
#' @param emm_fn the matching emm_*_model() wrapper (e.g. emm_yield_dry_model)
run_outcome_model <- function(data, model_fn, emm_fn, outcome_label) {
  result <- tryCatch({
    fit <- model_fn(data)
    diag <- diagnose_glmm(fit)
    emm <- emm_fn(fit)

    list(
      outcome_label = outcome_label,
      formula = paste(deparse(stats::formula(fit)), collapse = " "),
      dispformula = deparse(fit$modelInfo$allForm$dispformula),
      spatial_structure = attr(fit, "spatial_structure"),
      n_obs = stats::nobs(fit),
      AIC = stats::AIC(fit),
      converged = isTRUE(fit$sdr$pdHess),
      anova = as.data.frame(anova_glmm(fit)),
      dharma_tests = list(
        uniformity_p = diag$tests$uniformity$p.value,
        dispersion_p = diag$tests$dispersion$p.value,
        outliers_p   = diag$tests$outliers$p.value
      ),
      emmeans_df  = as.data.frame(emm$emmeans),
      contrasts_df = as.data.frame(emm$pairwise),
      error = NA_character_
    )
  }, error = function(e) {
    message("run_outcome_model('", outcome_label, "') failed to fit -- returning a ",
            "diagnostic-only result rather than failing the whole pipeline. Error: ", e$message)
    list(
      outcome_label = outcome_label, formula = NA_character_, dispformula = NA_character_,
      spatial_structure = NA_character_, n_obs = NA_integer_, AIC = NA_real_, converged = FALSE,
      anova = data.frame(Note = paste("Model fit failed:", e$message)),
      dharma_tests = list(uniformity_p = NA_real_, dispersion_p = NA_real_, outliers_p = NA_real_),
      emmeans_df = data.frame(), contrasts_df = data.frame(),
      error = e$message
    )
  })
  result
}

#' Render a run_outcome_model() result's ANOVA table as a formatted gt table.
make_anova_gt_table_from_result <- function(result, digits = 3) {
  aov_tbl <- tibble::rownames_to_column(result$anova, var = "Term")

  aov_tbl |>
    gt::gt() |>
    gt::tab_header(
      title = paste("Type II ANOVA:", result$outcome_label),
      subtitle = paste0("spatial structure = ", result$spatial_structure %||% "unknown",
                        " | AIC = ", round(result$AIC, 1),
                        " | converged = ", result$converged)
    ) |>
    gt::fmt_number(columns = dplyr::where(is.numeric), decimals = digits)
}

#' One-row-per-outcome overview table from a named list of
#' run_outcome_model() results.
make_model_overview_table <- function(results) {
  rows <- lapply(names(results), function(nm) {
    r <- results[[nm]]
    tibble::tibble(
      outcome = r$outcome_label %||% nm,
      formula = r$formula,
      spatial_structure = r$spatial_structure %||% NA_character_,
      n_obs = r$n_obs,
      AIC = r$AIC,
      converged = r$converged,
      dispersion_p = r$dharma_tests$dispersion_p,
      uniformity_p = r$dharma_tests$uniformity_p
    )
  })
  overview <- dplyr::bind_rows(rows)

  overview |>
    gt::gt() |>
    gt::tab_header(title = "Study 2 model overview") |>
    gt::fmt_number(columns = c(AIC, dispersion_p, uniformity_p), decimals = 3)
}

#' Estimated marginal means for each outcome's primary treatment contrast
#' (STEP 9 equivalent) -- paired with the fit_*_model() wrappers above via
#' the same tar_map() outcome table in _targets.R.
emm_yield_dry_model  <- function(fit) summarize_glmm_emmeans(fit, ~ FERTILISATION * RESIDUE)
emm_quality_model    <- function(fit) summarize_glmm_emmeans(fit, ~ FERTILISATION * RESIDUE)
emm_no3_model        <- function(fit) summarize_glmm_emmeans(fit, ~ fertilisation * residue)
emm_soil_water_model <- function(fit) summarize_glmm_emmeans(fit, ~ LOCATION * TREATMENT)

#' Generic estimated-marginal-means effect plot: point + asymptotic 95% CI
#' per combination of two grouping factors, from a run_outcome_model()
#' result's `emmeans_df`. New, non-preliminary figure -- deliberately
#' distinguishes plotted MODEL output from raw-data figures (Midway (2020)
#' Principle #7, "data and models are different things": the subtitle states
#' this is the fitted model's estimated marginal means, not raw
#' observations, and callers should cite the corresponding fit_*_model() for
#' full model details).
#'
#' @param emmeans_df a run_outcome_model() result's `$emmeans_df`
#' @param x_var,colour_var the two grouping column names present in
#'   emmeans_df (e.g. "FERTILISATION"/"RESIDUE" for yield;
#'   "LOCATION"/"TREATMENT" for soil water) -- column names and casing differ
#'   by outcome (see each outcome's emm_*_model() spec), so these are
#'   required arguments rather than hardcoded.
#' @param colour_values optional named colour vector keyed on colour_var's
#'   levels (e.g. utils.R's fertilisation_colors, for consistency with the
#'   preliminary figures); NULL uses ggplot2's own discrete scale.
#' @param y_lab y-axis label (outcome name + units)
#'
#' Column-name note: `emmeans::emmeans(..., type = "response")` names its
#' point-estimate/CI columns differently depending on the model family/link
#' -- "response"/"asymp.LCL"/"asymp.UCL" for the Gamma(link="log") outcomes
#' (yield_dry, no3_leaching, soil_water_vwc; asymptotic df = Inf), but
#' "emmean"/"lower.CL"/"upper.CL" for the gaussian() outcome (quality_n;
#' finite df, no back-transform). Detected here rather than hardcoded so
#' this one function works for every outcome's fit_*_model().
plot_emmeans_effect <- function(emmeans_df, x_var, colour_var,
                                 colour_values = NULL, y_lab = "Estimated marginal mean") {
  x_sym      <- rlang::sym(x_var)
  colour_sym <- rlang::sym(colour_var)

  est_col <- if ("response" %in% names(emmeans_df)) "response" else "emmean"
  lcl_col <- if ("asymp.LCL" %in% names(emmeans_df)) "asymp.LCL" else "lower.CL"
  ucl_col <- if ("asymp.UCL" %in% names(emmeans_df)) "asymp.UCL" else "upper.CL"
  est_sym <- rlang::sym(est_col)
  lcl_sym <- rlang::sym(lcl_col)
  ucl_sym <- rlang::sym(ucl_col)

  p <- ggplot2::ggplot(
    emmeans_df,
    ggplot2::aes(x = !!x_sym, y = !!est_sym, colour = !!colour_sym, group = !!colour_sym)
  ) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = !!lcl_sym, ymax = !!ucl_sym),
                           width = 0.15, position = ggplot2::position_dodge(width = 0.3),
                           linewidth = 0.8) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.3)) +
    ggplot2::labs(
      title = "Estimated marginal means (fitted model)",
      subtitle = "Point = model estimate, error bars = asymptotic 95% CI -- not raw data",
      x = x_var, y = y_lab, colour = colour_var
    ) +
    theme_study2()

  if (!is.null(colour_values)) p <- p + ggplot2::scale_colour_manual(values = colour_values)
  p
}

# ---- Candidate manuscript models ---------------------------------------------
# The models above (fit_yield_dry_model, fit_quality_model, fit_no3_model,
# fit_soil_water_model) are the pipeline's published/reported model objects
# (model_result_* targets) -- left untouched here per the manuscript figure
# brief's instruction not to silently overwrite them. The functions below are
# clearly-named CANDIDATES that add the biologically pre-specified
# PV_LOCATION:FERTILISATION / PV_LOCATION:RESIDUE interaction terms the
# manuscript's central question (does agronomic management perform
# differently under VAPV?) actually requires -- GLOBAL_SUB_STRIP_LOCATION
# (6 levels: PV zone x WEST/MIDDLE/EAST) is too fine-grained to cross with
# FERTILISATION x RESIDUE without emptying cells, so these use the coarser
# 2-level LOCATION/location factor (VAPV vs open field) instead. They are
# used only by the manuscript figure functions (fig01_*, fig02_*, ...), not
# wired into the `outcome_specs` tar_map() factory, so they never replace the
# existing model_result_* targets.

#' Candidate manuscript yield model: same family/dispersion structure as
#' fit_yield_dry_model(), LOCATION in place of GLOBAL_SUB_STRIP_LOCATION, and
#' the full pre-specified 2-way interaction set
#' LOCATION:FERTILISATION + LOCATION:RESIDUE + FERTILISATION:RESIDUE. The
#' 3-way LOCATION:FERTILISATION:RESIDUE term is deliberately NOT included by
#' default (see fit_yield_dry_model_manuscript_3way()) -- fit that only after
#' checking cell counts/rank-deficiency for the crop(s) of interest.
fit_yield_dry_model_manuscript <- function(data) {
  fit_glmm_outcome(
    data, outcome = "DRY_WEIGHT_TON_HA",
    fixed_effects = c("LOCATION", "FERTILISATION", "RESIDUE", "CROP_PRODUCT", "YEAR"),
    interactions = c("LOCATION:FERTILISATION", "LOCATION:RESIDUE", "FERTILISATION:RESIDUE"),
    spatial_structure = "row_col_disp",
    family = Gamma(link = "log")
  )
}

#' 3-way extension of fit_yield_dry_model_manuscript() -- only fit this and
#' inspect car::Anova()/rank-deficiency warnings before using it for a
#' figure; do not assume it converges for every crop subset.
fit_yield_dry_model_manuscript_3way <- function(data) {
  fit_glmm_outcome(
    data, outcome = "DRY_WEIGHT_TON_HA",
    fixed_effects = c("LOCATION", "FERTILISATION", "RESIDUE", "CROP_PRODUCT", "YEAR"),
    interactions = c("LOCATION:FERTILISATION", "LOCATION:RESIDUE", "FERTILISATION:RESIDUE",
                      "LOCATION:FERTILISATION:RESIDUE"),
    spatial_structure = "row_col_disp",
    family = Gamma(link = "log")
  )
}

#' Figure 1 candidate models -- crop productivity/quality response to VAPV
#' ONLY (no FERTILISATION/RESIDUE at all; those move to a separate
#' management-effects figure per the researcher's explicit request). Each
#' outcome gets two variants: "substrip" (the 6-level GLOBAL_SUB_STRIP_LOCATION,
#' i.e. does the effect vary WEST/MIDDLE/EAST within each PV zone) and
#' "pooled" (the 2-level LOCATION, VAPV vs open field only) -- fit as two
#' separate models rather than one and marginalised, so each is a clean,
#' directly-interpretable model in its own right. Both include a
#' location:CROP interaction (crop-specific VAPV/substrip response), fit on
#' PRIMARY-PRODUCT rows only (PRODUCT_SIMPLE %in% c("GRAIN","BIOMASS") --
#' the crop's actual harvestable yield component: WW/SY grain, GC/RG/SB
#' biomass; excludes straw/pod/residue by-products, which belong in the
#' Figure 5 N-use/export analysis, not a productivity figure).
fit_yield_substrip_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "DRY_WEIGHT_TON_HA",
    fixed_effects = c("GLOBAL_SUB_STRIP_LOCATION", "CROP", "YEAR"),
    interactions = "GLOBAL_SUB_STRIP_LOCATION:CROP",
    spatial_structure = "row_col_disp",
    family = Gamma(link = "log")
  )
}

fit_yield_pooled_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "DRY_WEIGHT_TON_HA",
    fixed_effects = c("LOCATION", "CROP", "YEAR"),
    interactions = "LOCATION:CROP",
    spatial_structure = "row_col_disp",
    family = Gamma(link = "log")
  )
}

#' Quality (N_TOTAL, %) counterparts. No YEAR (N_TOTAL exists for 2024 only
#' -- see fit_quality_model()'s docstring); coverage is further limited to
#' WW/SY/GC (RG and SB have zero N_TOTAL observations on their primary
#' product -- confirmed empirically), so these models' CROP factor will
#' have only those levels once NA rows are dropped. `spatial_structure =
#' "none"` (not "row_col_disp") for BOTH: N_TOTAL's 204 rows are too sparse
#' for a ROW+COLUMN dispersion submodel on top of the fixed effects here --
#' confirmed empirically (row_col_disp gave a non-positive-definite Hessian
#' for the substrip variant). The GLOBAL_SUB_STRIP_LOCATION:CROP interaction
#' (6 x 3 = 18 cells over 204 rows) is dropped from the substrip model for
#' the same reason -- confirmed non-convergent when included; main effects
#' only. The pooled model's coarser 2 x 3 = 6-cell interaction DOES converge
#' and is kept.
fit_quality_substrip_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "N_TOTAL",
    fixed_effects = c("GLOBAL_SUB_STRIP_LOCATION", "CROP"),
    spatial_structure = "none",
    family = stats::gaussian()
  )
}

fit_quality_pooled_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "N_TOTAL",
    fixed_effects = c("LOCATION", "CROP"),
    interactions = "LOCATION:CROP",
    spatial_structure = "none",
    family = stats::gaussian()
  )
}

#' Figure 1 panels E/F candidate model -- crop-specific management effects
#' (fertiliser source, residue management) on yield, and whether VAPV
#' modifies the pooled (across-crop) management response. Deliberately
#' parsimonious: FERTILISATION:CROP and RESIDUE:CROP give per-crop
#' management effects (panel E); LOCATION:FERTILISATION and LOCATION:RESIDUE
#' give the POOLED (not per-crop) VAPV x management interaction (panel F).
#' A full LOCATION:FERTILISATION:CROP / LOCATION:RESIDUE:CROP three-way (a
#' per-crop version of panel F) was deliberately NOT fit: with 2 (location)
#' x 2 (fertiliser or residue) x 5 (crop) = 20 cells over the same ~444-row
#' primary-product dataset used elsewhere in Figure 1, several crops (SY:
#' 24 rows, SB: 30 rows) would average under 2 rows per cell -- reporting a
#' pooled interaction here rather than a per-crop one that would not be
#' reliably estimable.
fit_yield_management_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "DRY_WEIGHT_TON_HA",
    fixed_effects = c("LOCATION", "FERTILISATION", "RESIDUE", "CROP", "YEAR"),
    interactions = c("FERTILISATION:CROP", "RESIDUE:CROP", "LOCATION:FERTILISATION", "LOCATION:RESIDUE"),
    spatial_structure = "row_col_disp",
    family = Gamma(link = "log")
  )
}

#' Candidate manuscript NO3 leaching model: adds location:fertilisation and
#' location:residue to fit_no3_model()'s existing fertilisation:residue term.
#' Same "no random effect" justification as fit_no3_model() applies (only 2
#' field strips, `crop` fixed effect already absorbs the strip grouping) --
#' see fit_no3_model()'s docstring. With only 2 strips x 2 fertilisation x 2
#' residue x 2 location cells, the 3-way interaction is not attempted (8
#' cells minimum, likely several empty/singleton) -- flagged as
#' NOT_ESTIMABLE rather than fit and silently unstable.
fit_no3_model_manuscript <- function(data) {
  fit_glmm_outcome(
    data, outcome = "N_LI_kgNha",
    fixed_effects = c("location", "fertilisation", "residue", "crop"),
    interactions = c("location:fertilisation", "location:residue", "fertilisation:residue"),
    spatial_structure = "none",
    family = Gamma(link = "log")
  )
}

#' Candidate manuscript soil-organic-carbon model, fit on the IUNG lab
#' soil-chemistry dataset (2_preprocessing/R/functions_soil_chemistry.R).
#' SURVEY_ROUND (2024_baseline / 2025_T1 / 2025_T2) stands in for a repeated-
#' measures time factor, but see build_iung_soil_dataset()'s docstring: these
#' are DIFFERENT physical sampling points at each round, not the same point
#' resampled, so LOCATION:SURVEY_ROUND tests whether the VAPV-vs-open-field
#' SOC difference is consistent ACROSS survey rounds, not a true paired
#' change (Delta SOC) at any one location. FERTILISATION is deliberately
#' excluded: only 40/114 rows have it resolved (via the TDR nest+plot
#' lookup; see build_iung_soil_dataset()), too sparse and too confounded
#' with SURVEY_ROUND (all resolved rows are 2025 T1/T2) to add as a fixed
#' effect without confounding the round comparison. RESIDUE is never
#' resolvable at this sampling grain (same limitation as the TDR outcome)
#' and is not in the data at all. No random effect / spatial term: nest is
#' fully crossed with LOCATION x SUB_STRIP_LOCATION already (a nest's
#' physical position defines LOCATION/SUB_STRIP_LOCATION), so `(1 | nest)`
#' would alias against those fixed effects the same way SUB_STRIP aliased
#' against LOCATION in the TDR model.
fit_soc_model_manuscript <- function(data) {
  fit_glmm_outcome(
    data, outcome = "SOC_PCT",
    fixed_effects = c("LOCATION", "SUB_STRIP_LOCATION", "SURVEY_ROUND"),
    interactions = "LOCATION:SURVEY_ROUND",
    spatial_structure = "none",
    family = stats::gaussian()
  )
}

#' Figure 2 panel C candidate model -- cumulative NO3-N leaching (DL-LI) by
#' the 6-level GLOBAL_SUB_STRIP_LOCATION (PV zone x west/centre/east), the
#' substrip-specific counterpart to fit_no3_model()'s 2-level `location`.
#' `crop` fixed effect (not `fertilisation`/`residue`) matches fit_no3_model()'s
#' precedent exactly: with only 2 monitored strips (A2 = winter wheat, A3 =
#' ryegrass), crop already captures the rotation/strip grouping, and 47 cups
#' split across 6 substrip cells (~7-8/cell) does not support adding
#' fertiliser/residue on top without emptying cells. No dispersion/spatial
#' term: no ROW/COLUMN grid exists for suction-cup locations (same
#' precedent as fit_no3_model()).
fit_no3_substrip_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "N_LI_kgNha",
    fixed_effects = c("GLOBAL_SUB_STRIP_LOCATION", "crop"),
    spatial_structure = "none",
    family = Gamma(link = "log")
  )
}

#' Figure 2 panel D candidate model -- cumulative NO3-N leaching by crop
#' (rotation/strip) x fertiliser x residue, INCLUDING the full 3-way
#' interaction. Confirmed empirically to converge despite only 47 total
#' observations (crop x fertilisation x residue cells are close to balanced:
#' 5-6 cups/cell, verified directly) -- fit and used as-is rather than
#' dropped pre-emptively, but treat per-cell estimates as based on small
#' samples (reflected in wide CIs, not a defect of the model).
fit_no3_management_model <- function(data) {
  fit_glmm_outcome(
    data, outcome = "N_LI_kgNha",
    fixed_effects = c("crop", "fertilisation", "residue"),
    interactions = c("crop:fertilisation", "crop:residue", "fertilisation:residue",
                      "crop:fertilisation:residue"),
    spatial_structure = "none",
    family = Gamma(link = "log")
  )
}

#' Lightweight, non-factorial comparison helper: one-sample t-test of a
#' group's mean against a reference mean. Generalises the pattern used in
#' MICROCLIMATE_ANALYSIS_2025.Rmd STEP 6 (nest mean vs. reference-station
#' mean, by day/night and month) for outcomes where the full factorial GLMM
#' design is not appropriate (e.g. too few reference observations to treat
#' as a random-effects group).
compare_group_vs_reference_ttest <- function(group_values, reference_values, conf_level = 0.95) {
  stats::t.test(group_values, mu = mean(reference_values, na.rm = TRUE), conf.level = conf_level)
}
