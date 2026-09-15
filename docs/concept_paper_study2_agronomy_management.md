# Concept Paper — Study 2: Agronomic Management in a Vertical Agrivoltaic System

**Working title of the planned manuscript:** *Fertilisation and residue management in a vertical agrivoltaic system: productivity, nutrient cycling, and system sustainability*

**Position in the PhD project:** Paper 3 of 3 (Study 2), within the tentative PhD title *"Investigating Win-Wins of 'Solar Sharing' in an Integrated Vertical Agrivoltaic System — Microclimatic and Agronomic Management Aspects of Crop Production and Renewable Energy Generation"*.

**Author:** Magnus Kamau Katana Lindhardt (au759124), Department of Agroecology, Aarhus University
**Main supervisor:** Prof. Uffe Jørgensen · **Co-supervisors:** Johannes W. M. Pullens, Maryam Rahimi Jahangirlou

**Status:** internal planning document (not for external submission). Drafted from the Qualifying Exam progress report (6 May 2025), the draft demonstration results section, and the `targets` reproducibility pipeline in this repository (`_targets.R`, `R/`). Last updated alongside that pipeline — see `README.md` for what the pipeline currently covers.

---

## 1. Background and rationale

Vertical bifacial agrivoltaics combines two land uses — crop production and photovoltaic (PV) electricity generation — on the same field area. Compared with ground-mounted or stilt-mounted horizontal PV, a vertical, north–south-oriented bifacial array minimises shading at midday (when it matters least for total daily radiation capture), captures diffuse light from both faces, generates on a schedule shifted toward morning and evening peak demand, and — in a windy, temperate climate such as Denmark's — can act as a windbreak in the tradition of the shelterbelt hedgerows planted across Jutland since the early 20th century (Toledo & Scognamiglio, 2021; Campana et al., 2021; Fritzbøger, 2002).

Two agronomic levers sit inside this physical system and are the subject of Study 2:

1. **Fertiliser source** — mineral NPK versus biogas digestate returned from anaerobic digestion of the system's own biomass (including grass-clover from the rotation). Digestate substitution is central to the Value4Farm project's closed-loop, circular-economy ambition: biomass grown in the field is processed through a green biorefinery and biomethanation plant, and the resulting digestate is returned to the same field as fertiliser (Sieborg et al., 2024; De Notaris et al., 2018).
2. **Crop residue management** — removing aboveground residue (straw, stubble) for external biomass utilisation versus retaining it in the field.

Both levers interact with the PV-modulated microclimate (shading, wind shelter, altered evapotranspiration) in ways that are not yet well characterised for *vertical* agrivoltaic configurations specifically — most existing agrivoltaic agronomy literature concerns horizontal or stilt-mounted systems (Amaducci et al., 2018; Marrou, Guilioni, et al., 2013; Laub et al., 2022). Understanding how fertilisation and residue treatments perform *combined with* the position-dependent shading and wind-shelter effects of the vertical panels — rather than each factor in isolation — is the specific gap Study 2 addresses.

## 2. Novelty and significance

Three things distinguish this study from a conventional fertiliser-trial paper:

- **A genuinely combined factorial design** (fertiliser source × residue management × position relative to PV panels × crop × year) run inside an operating vertical agrivoltaic field trial, not a simulated or greenhouse system.
- **A concrete, closeable analytical gap.** The trial's own field design already includes a residue-removal treatment, and the underlying data already carries a `RESIDUE` column (removed vs. retained, derived from the four-letter treatment code A/B/C/D) — but residue management has, to date, never been included as a predictor in any fitted statistical model of crop yield, quality, or nitrate leaching (confirmed by inspecting every model formula in `CROP_YIELD_ANALYSIS_2025.Rmd` and `BIOSTATISTICS_MOD_FOR_AG_SCIENCE.Rmd`: every one omits `RESIDUE`). Similarly, soil water content (TDR) and NO3-N leaching have so far been reported only descriptively (means ± SE), never through a formal significance test. This concept paper's accompanying `targets` pipeline (Section 7) closes both gaps directly: a `FERTILISATION:RESIDUE` interaction term is now included in every yield/quality/leaching model, and soil water content and NO3-N leaching each get, for the first time, a formal generalized linear mixed model rather than a descriptive comparison only.
- **A reusable, generic statistical model factory** (Objective 1, below) rather than one hand-written model per outcome — directly answering the methodological objective set out in the PhD progress report, and making it straightforward to re-run the same tested treatment structure on new outcomes as more 2025/2026 data become analysis-ready.

## 3. Aim

To investigate the influence and interactions of agronomic management practices — particularly crop residue removal and fertilisation (biogas digestate vs. mineral fertiliser) — on crop productivity, quality, and key soil ecosystem services in a vertical agrivoltaic system. This study explores how agronomic practices interact with vertical agrivoltaic PV panels regarding productivity, nutrient cycling, and system sustainability.

## 4. Objectives

1. Develop and deploy a generic statistical model for analysing outcome metrics based on explanatory variables such as location within the vertical agrivoltaics, fertilisation and crop residue treatments, including crop type, year and spatial autocorrelation structure.
2. Assess how fertilisation treatments and crop residue management impact crop yield and biomass production and quality, as well as soil water balance, nutrient uptake dynamics, and carbon sequestration.
3. Evaluate impacts of fertilisation, crop residue management and their interactions with vertical PV panels on nitrate leaching.

## 5. Research questions and hypotheses

**RQ1.** How does the removal of crop residues and subsequent re-application of digestate influence short-term nitrogen leaching, nutrient cycling, and carbon balance compared with keeping residues in the field and using mineral fertiliser?

- *H1.1* — Digestate application following residue removal will reduce nitrate leaching compared with mineral fertiliser, due to slower nitrogen mineralisation and better synchrony with crop uptake.
- *H1.2* — Residue removal decreases soil carbon inputs; digestate re-application can moderately mitigate this through organic matter addition, but carbon retention is still expected to be lower than in systems with mineral fertiliser where residues are retained.

**RQ2.** What are the effects of residue removal and digestate fertilisation on productivity, yield, and quality at the rotation level and for individual crops — and are there interactions with agrivoltaic versus open-field conditions?

- *H2.1* — Across a full crop rotation, digestate fertilisation combined with residue removal enhances total productivity and nitrogen use efficiency compared with mineral fertilisation, without reducing crop quality.
- *H2.2* — Individual crop performance under digestate fertilisation is expected to further improve under agrivoltaic conditions due to moderate temperatures and increased soil moisture, leading to higher quality traits (e.g. protein content) than under open-field conditions.

**RQ3.** How do these agronomic management practices interact with agrivoltaic conditions (shading, wind shelter, moisture retention) — and what are the implications for system resilience and resource-use efficiency?

- *H3.1* — Digestate fertilisation under agrivoltaics improves water-, land- and nutrient-use efficiency, due to reduced wind speed and evaporation lowering nutrient losses relative to open-field conditions.
- *H3.2* — Agronomic strategies combining digestate fertilisation with residue removal enhance system sustainability and resilience — especially under agrivoltaic conditions — by improving nutrient retention.

## 6. Site, experimental design and treatments

Field trial at Aarhus University, AU Viborg (Foulum) (N 56°29'35.577", E 9°33'40.266"), inside the vertical bifacial PV installation established under the Value4Farm project. Plots (~1.5 m × 1.5 m) are arranged in a georeferenced grid between the PV rows (`BETWEEN_PV`) and in an adjacent open-field reference area (`OUTSIDE_PV`), with three sub-strip positions (`WEST`/`MIDDLE`/`EAST`) crossed with location to give the six-level factor `GLOBAL_SUB_STRIP_LOCATION` used throughout the analysis.

Four-year crop rotation: winter wheat → soybean → grass-clover (2 years) → spring barley with undersown ryegrass, reflecting practices relevant to Danish farming systems. Two strip groups carry distinct rotations for the nitrate-leaching analysis: **A2** (winter wheat–soybean–winter wheat) and **A3** (grass-clover–grass-clover–spring barley).

Treatments are a full factorial of:

- **Fertilisation** — biogas digestate (`BD`) vs. mineral NPK (`MF`). Mineral fertiliser rate is matched to the *NH4-N* content of the digestate (not total N), since NH4-N is the fraction readily available within the cropping season, following De Notaris et al. (2018) and Nyang'au et al. (2023). Both treatments are supplemented with potassium where relevant, and application timing/splitting is synchronised between treatments.
- **Residue management** — removed vs. retained (`RESIDUE`), derived from the four-letter treatment code (A/B → removed, C/D → retained) jointly with fertilisation (A/C → digestate, B/D → mineral).

## 7. Data, methods and the reproducibility pipeline

### 7.1 Measurements

- **Yield and biomass** — harvested-area fresh weight in the field, oven-dried subsamples for dry-matter content; total carbon and total nitrogen on dried, milled samples (quality).
- **Nitrate leaching** — biweekly soil-water sampling via ceramic suction cups at 1 m depth (48 plots), analysed for NO3-N. Cumulative leaching is estimated with the **Drainage-Linear (DL) method** of Vogeler et al. (2020): daily N flux = interpolated suction-cup concentration × Daisy-simulated daily drainage at 1 m, summed over the monitoring period, cross-validated against Daisy's own simulated NO3-N leaching.
- **Soil water** — Time-Domain Reflectometry (TDR) at 20 and 50 cm depth in selected plots, alongside Daisy-simulated soil water percolation.
- **Soil spatial covariates** — a stratified soil survey (winter 2024–25) of texture and organic matter, interpolated by ordinary kriging (spherical variogram) to provide spatial covariates for the statistical models, addressing the field variability documented in that survey.
- **Disease pressure** — septoria severity scoring on winter wheat (2025), converted to a Disease Severity Index (DSI).

### 7.2 Statistical modelling

Generalized linear mixed models (glmmTMB) are used throughout. The core explanatory structure — `outcome ~ GLOBAL_SUB_STRIP_LOCATION + FERTILISATION + RESIDUE + CROP_PRODUCT + YEAR` (plus a `FERTILISATION:RESIDUE` interaction), with `ROW`/`COLUMN` field-grid position entered through the dispersion submodel (`dispformula = ~ ROW + COLUMN`) as the spatial-autocorrelation term — is fit through **one generic factory function**, `fit_glmm_outcome()`, rather than a bespoke model per outcome. Model diagnostics use DHARMa simulation-based residual tests; significance testing uses Type II Wald χ² (car::Anova); post-hoc comparisons use estimated marginal means (emmeans) with Tukey adjustment. Where the ROW/COLUMN plot grid does not apply (suction-cup and TDR-nest locations, which follow their own installation plans), the factory instead uses a random intercept for the relevant clustering unit (strip, or TDR sub-strip).

### 7.3 Reproducibility pipeline

This concept paper is paired with a `targets`-framework (Wickham et al.) pipeline in the same repository (`_targets.R`, functions in `R/`), built to FAIR/open-science standards: every derived dataset, model, table and figure is a tracked, cacheable target with an explicit dependency graph back to the raw workbooks and Daisy model outputs, `renv` pins the exact R package versions, and the whole analysis re-runs end-to-end with `targets::tar_make()`. Concretely, the pipeline currently covers:

- Field management import (wraps `DATA/FIELD_MANAGEMENT/read_foulumngaard_mngmnt.py`) → crop calendar / strip-crop lookup
- Crop yield & quality import and cleaning (2023–2025) → the `GLOBAL_SUB_STRIP_LOCATION` / `ROW` / `COLUMN` / `FERTILISATION` / `RESIDUE` covariates
- Soil-water suction-cup import and cleaning, and the full DL nitrate-leaching computation (with Daisy cross-validation)
- TDR soil-water import and cleaning
- Soil-survey kriging (spatial covariates)
- Fertilisation-plan calculations, winter-wheat disease severity index, and a-LER (apparent Land Equivalent Ratio)
- The generic statistical model factory, mapped across all four outcomes (yield, quality, NO3-N leaching, soil water content) via `tarchetypes::tar_map()`
- An auto-rendered summary report (`reports/study2_pipeline_summary.Rmd`)

Study 1 (microclimate/DAISY modelling) and Study 3 (whole-system assessment) are **not yet ported** into this pipeline; their existing ad hoc `.Rmd`/`.R` scripts remain the working analyses for now (see `README.md`'s migration table). DAISY's own drainage and NO3-leaching outputs *are* consumed here as tracked file inputs to the leaching branch, without re-implementing the DAISY model run itself.

## 8. Preliminary results (as of this pipeline's first build)

These are the field-observed numbers already available from the 2023–2025 seasons (draft demonstration results section), which this study's manuscript will extend with the residue-inclusive models above:

- **Position effect on yield.** A mixed-effects model across grass-clover, soybean and winter wheat (2023–2024) found a statistically significant overall effect of position relative to the PV rows on dry-matter yield in the best-supported specification (χ² = 14.0, df = 5, *p* = 0.015); the position × crop interaction was not significant in the tested specifications. Winter wheat grain yield was 9% (2023) to 17% (2024) lower between the rows than in the open field; soybean kernel yield was 24–28% lower; grass-clover was materially unaffected (−4% to +7% across three years).
- **Fertiliser source.** No statistically significant effect of fertiliser source on yield in any crop tested (lowest-AIC model: χ² = 0.99, df = 1, *p* = 0.32), supporting the technical feasibility of digestate substitution without a measurable yield penalty (partially supporting H2.1 — see Section 9 for what remains to be tested with residue included).
- **Residue management.** Established in the field design but, prior to this pipeline, never included in a formal yield/quality model — an exploratory (untested) comparison of cumulative 2024 yield showed overlapping distributions between removal and retention for all three crops tested.
- **Nitrate leaching.** Cumulative NO3-N leaching (DL method, linear interpolation, Sept 2024–Dec 2025) was roughly 15-fold higher in the winter-wheat rotation (A2, 41.1 kg N ha⁻¹) than the grass-clover rotation (A3, 2.8 kg N ha⁻¹). Within the winter-wheat rotation, leaching between the PV rows (52.6–54.7 kg N ha⁻¹) was 70–90% higher than in the open field (24.5–32.6 kg N ha⁻¹); fertiliser source had little discernible effect. Volume-weighted NO3-N concentrations in winter-wheat between-row cups repeatedly exceeded the EU drinking-water threshold (11.3 mg NO3-N L⁻¹). Cross-validation of the field DL estimate against Daisy's own simulated leaching showed moderate agreement for the winter-wheat rotation (R² = 0.50) and weaker agreement for grass-clover (R² = 0.17) — a caveat carried into this pipeline's `no3_dl_vs_daisy_validation` target rather than treated as settled.
- **Soil water.** Between-row soil moisture was comparable to the open field in the near-normal 2024 season but higher than the open field during the dry spring of 2025 — a pattern consistent with reduced evaporative demand under the panels becoming most relevant when water is limiting, but not yet formally tested prior to this pipeline.

### 8.1 First output from the generic model factory (this pipeline's own first `tar_make()`)

Running the pipeline end-to-end on the complete 2023–2025 dataset (rather than the 2023–2024 subset behind the numbers above) gives, for the first time, a `RESIDUE` term and a `FERTILISATION:RESIDUE` interaction in every model, and a formal test for NO3 leaching and soil water where none existed before:

| Outcome | Location/position effect | Fertiliser effect | Residue effect | Fertiliser × Residue |
|---|---|---|---|---|
| Yield (dry matter) | χ²=74.4, df=5, **p<0.001** | χ²=8.6, df=1, **p=0.003** | χ²≈0, df=1, p=0.997 | χ²=2.2, df=1, p=0.135 |
| Quality (N_TOTAL) | χ²=693, df=5, **p<0.001** | χ²=30.3, df=1, **p<0.001** | χ²=2.4, df=1, p=0.118 | χ²=0.05, df=1, p=0.823 |
| NO3-N leaching | χ²=11.9, df=1, **p<0.001** | χ²≈0, df=1, p=0.974 | χ²=5.4, df=1, **p=0.021** | χ²=0.65, df=1, p=0.419 |
| Soil water (TDR) | χ²=7.9, df=1, **p=0.005** | χ²=0.7, df=1, p=0.391 | *(not available at TDR-nest level)* | — |

Two things stand out for RQ1/H1: residue management shows **no detectable effect on yield or quality**, but a genuine, newly-detected effect on NO3-N leaching (p=0.021) — the opposite of what a "residue retention protects yield, costs nothing environmentally" intuition might predict, and worth a closer look (direction of effect, interaction with rotation) before it goes into the manuscript. The position/location effect is significant in every outcome, reinforcing RQ3's premise that where a plot sits relative to the PV rows matters system-wide, not just for yield. Cross-validating the DL nitrate-leaching estimate against Daisy's own simulated leaching on the full dataset gives R²=0.50 for the winter-wheat rotation and R²=0.06 for grass-clover, closely matching the R²=0.50/0.17 figures above despite the different (complete vs. partial) date range -- consistent evidence that the DL estimate is much more reliable for the winter-wheat rotation than for grass-clover, where absolute leaching is small enough that noise dominates.

These are first-pass numbers from the newly-built pipeline, not yet reviewed with a supervisor or checked against the open methods questions in Section 9 -- they demonstrate that the generic model factory works end-to-end on real data, not a final result for the manuscript.

## 9. Open analyses this study still needs (tracked as pipeline TODOs)

- `N_TOTAL` quality data currently exist only for the 2024 season (282/606 rows; all non-missing rows are 2024) — the quality model therefore omits `YEAR` as a predictor. Digitise 2023/2025 quality data and re-add `YEAR` once more than one season is available.
- a-LER now runs end-to-end on the complete 2023-2025 yield dataset (previously blocked by a missing preprocessing step in the PV workbook join) and gives LER > 1 for both grain (VERTICAL ≈ 1.06, SOUTH-ORIENTED ≈ 1.10) and grass-clover (VERTICAL ≈ 1.18, SOUTH-ORIENTED ≈ 1.14) — consistent with the draft report's earlier ≈1.18/≈1.26 figures. It still uses `DATA/PV_DATA/PV_PANEL_DATA.xlsx`'s reference electricity-output assumptions (394/344/935 MWh ha⁻¹ yr⁻¹ by setup) rather than the measured 2024 inverter output (43,845 kWh tilted / 30,561 kWh vertical, Section 3.6 of the draft demonstration results) — reconciling those two electricity figures (different aggregation: per-hectare assumption vs. whole-array measured total) is the remaining step before this is a finished result.
- Extend the NO3-leaching and soil-water models to the 2025/2026 drainage season once available, and once Daisy's drainage simulation incorporates the measured between-row microclimate (radiation/wind/ET) differences rather than identical forcing for both locations.
- Decide, with supervisors, whether `spatial_structure = "row_col_disp"` (dispersion-submodel ROW+COLUMN, the option used for yield/quality here) is the appropriate final spatial treatment for publication, or the alternative crossed-random-effects / true spatial-covariance options also built into `fit_glmm_outcome()`. For NO3 leaching and TDR soil water, no random/spatial effect was usable at all: the suction cups sit on only 2 field strips and the TDR nests' `LOCATION` is itself a per-nest property, so a naive `(1 | strip)` / `(1 | SUB_STRIP)` term aliases against a fixed effect (confirmed empirically, not just a theoretical concern) — a better repeated-measures structure for those two outcomes (e.g. a date- or cup-level term) is an open methods question.
- Formally test the botanical composition (grass:clover ratio) question flagged in the draft results as unrecorded, if sampling can be added in a future season.

## 10. Expected outcomes and target journal

A manuscript quantifying, for the first time in a vertical agrivoltaic system, the combined and interactive effects of fertiliser source and residue management on crop productivity, quality, nutrient cycling and nitrate leaching, differentiated by position relative to the PV rows. Candidate outlets: *Agricultural Systems*, *European Journal of Agronomy*, or *Field Crops Research* (agrivoltaics/agronomy scope, comparable methodology to prior vertical-agrivoltaic and digestate-substitution studies cited above).

## 11. Work plan (from the PhD progress report's remaining-period plan)

- **Short term (next 6–12 months):** complete data analysis for Study 2 across the full field dataset, applying the spatial/temporal statistical models above to crop yield, nitrate leaching, and soil fertility outcomes; incorporate the winter 2024–25 soil survey as spatial covariates.
- **Long term (12–18 months):** submit the Study 2 manuscript, focused on agronomic management interactions, crop-specific responses, and environmental trade-offs under vertical agrivoltaics in Danish conditions; feed findings into the Study 3 whole-system assessment.

## 12. Data and code availability

Field data, Daisy model outputs, and analysis code are version-controlled at `github.com/kamaulindhardt/V4F_agrivoltaics` (this repository). The Study 2 pipeline described in Section 7.3 is fully reproducible via `renv::restore()` followed by `targets::tar_make()`; see the repository `README.md` for setup instructions.

## References

Amaducci, S., Yin, X., & Colauzzi, M. (2018). Agrivoltaic systems to optimise land use for electric energy production. *Applied Energy*, 220, 545–561.

Campana, P. E., et al. (2021). Optimisation of vertically mounted agrivoltaic systems. *Journal of Cleaner Production*.

De Notaris, C., et al. (2018). Nitrogen and carbon dynamics in soil following digestate application. *Agriculture, Ecosystems & Environment*.

Dupraz, C., et al. (2011). Combining solar photovoltaic panels and food crops for optimising land use: Towards new agrivoltaic schemes. *Renewable Energy*, 36(10), 2725–2732.

Fritzbøger, B. (2002). *Det danske landskabs historie* [The history of the Danish landscape].

Gomez-Casanovas, N., et al. (2023). Knowns, uncertainties, and challenges in agrivoltaic agronomy and ecology. *Nature Sustainability*.

Laub, M., et al. (2022). Contrasting yield responses at varying levels of shade suggest different suitability of crops for dual land-use systems. *European Journal of Agronomy*.

Marrou, H., Guilioni, L., et al. (2013). Microclimate under agrivoltaic systems: Is crop growth rate affected in the partial shade of solar panels? *Agricultural and Forest Meteorology*, 177, 117–132.

Nyang'au, J. O., et al. (2023). Nitrogen use efficiency of digestate compared to mineral fertiliser. *European Journal of Agronomy*.

Sieborg, M. U., et al. (2024). Green biorefinery and biomethanation as a bioenergy pathway. *Bioresource Technology*.

Toledo, C., & Scognamiglio, A. (2021). Agrivoltaic systems design and assessment: A critical review, and a descriptive model towards a sustainable landscape vision. *Sustainability*, 13(12), 6871.

Vogeler, I., Thomas, S., van der Linden, A., & Bolan, N. (2020). Nitrate leaching from suction cup data: Influence of method of drainage calculation and concentration interpolation. *Journal of Environmental Quality*, 49(2), 338–347.

Widmer, F., et al. (2024). Environmental and agronomic performance of agrivoltaic systems: A review. *Renewable and Sustainable Energy Reviews*.

*(Full reference list to be reconciled with the PhD progress report's bibliography before manuscript submission.)*
