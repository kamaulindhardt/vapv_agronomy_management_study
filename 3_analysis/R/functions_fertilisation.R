# Fertilisation planning calculations. Ported from FERTILISATION_CALCULATION.Rmd
# (already modular in the source -- near-mechanical port).

#' Build a full fertilisation plan (split digestate applications + the
#' matching mineral NPK requirement) from lab-analysed digestate nutrient
#' concentrations.
calculate_fertilisation_plan <- function(
  digestate_nh4_conc_kg_per_ton,
  digestate_p_conc_kg_per_ton,
  digestate_k_conc_kg_per_ton,
  split_applications_ton_ha,
  mineral_npk_n_percent = 21,
  mineral_npk_p_percent = 4,
  mineral_npk_k_percent = 10,
  extra_n_applied_in_yara_extran_kg_ha = NULL
) {
  split_results <- tibble::tibble(
    Split = paste0("Split_", seq_along(split_applications_ton_ha)),
    Digestate_ton_ha = split_applications_ton_ha,
    NH4_N_kg_ha = Digestate_ton_ha * digestate_nh4_conc_kg_per_ton,
    P_kg_ha = Digestate_ton_ha * digestate_p_conc_kg_per_ton,
    K_kg_ha = Digestate_ton_ha * digestate_k_conc_kg_per_ton
  )

  total_digestate_ton_ha <- sum(split_results$Digestate_ton_ha)
  total_nh4_n_kg_ha <- sum(split_results$NH4_N_kg_ha)
  total_p_kg_ha <- sum(split_results$P_kg_ha)
  total_k_kg_ha <- sum(split_results$K_kg_ha)

  total_available_n_kg_ha <- total_nh4_n_kg_ha
  extra_n_info <- NULL

  if (!is.null(extra_n_applied_in_yara_extran_kg_ha)) {
    extra_n_percent <- 33.5 # Yara Extran N%
    extra_n_kg_ha <- extra_n_applied_in_yara_extran_kg_ha * (extra_n_percent / 100)
    total_available_n_kg_ha <- total_available_n_kg_ha + extra_n_kg_ha

    extra_n_info <- tibble::tibble(
      Extra_Fertiliser_Type = "Yara Extran 33.5-0-0",
      Extra_Fertiliser_Applied_kg_ha = extra_n_applied_in_yara_extran_kg_ha,
      Extra_N_Contributed_kg_ha = extra_n_kg_ha
    )
  }

  required_mineral_n_kg_ha <- total_available_n_kg_ha / (mineral_npk_n_percent / 100)
  required_mineral_p_kg_ha <- total_p_kg_ha / (mineral_npk_p_percent / 100)
  required_mineral_k_kg_ha <- total_k_kg_ha / (mineral_npk_k_percent / 100)

  fertiliser_summary <- tibble::tibble(
    Total_Digestate_ton_ha = total_digestate_ton_ha,
    Total_Available_N_kg_ha = total_available_n_kg_ha,
    Total_P_kg_ha = total_p_kg_ha,
    Total_K_kg_ha = total_k_kg_ha,
    Required_NPK_for_N_kg_ha = required_mineral_n_kg_ha,
    Required_NPK_for_P_kg_ha = required_mineral_p_kg_ha,
    Required_NPK_for_K_kg_ha = required_mineral_k_kg_ha
  )

  list(
    Split_Results = split_results,
    Fertiliser_Summary = fertiliser_summary,
    Extra_N_Info = extra_n_info
  )
}

#' Calculate digestate application rate (ton/ha) needed to reach a target
#' NH4-N input, and the resulting P/K/S co-application.
calculate_digestate_application <- function(target_nh4_input_kg_ha,
                                             nh4_content_kg_per_ton,
                                             total_n_content_kg_per_ton,
                                             p_content_kg_per_ton,
                                             k_content_kg_per_ton,
                                             s_content_kg_per_ton) {
  if (nh4_content_kg_per_ton <= 0 || total_n_content_kg_per_ton <= 0) {
    stop("NH4-N and Total N contents must be positive values.")
  }

  digestate_ton_per_ha <- target_nh4_input_kg_ha / nh4_content_kg_per_ton

  resulting_total_n_input_kg_ha <- digestate_ton_per_ha * total_n_content_kg_per_ton
  resulting_p_input_kg_ha <- digestate_ton_per_ha * p_content_kg_per_ton
  resulting_k_input_kg_ha <- digestate_ton_per_ha * k_content_kg_per_ton
  resulting_s_input_kg_ha <- digestate_ton_per_ha * s_content_kg_per_ton

  list(
    digestate_ton_per_ha = digestate_ton_per_ha,
    resulting_total_n_input_kg_ha = resulting_total_n_input_kg_ha,
    resulting_p_input_kg_ha = resulting_p_input_kg_ha,
    resulting_k_input_kg_ha = resulting_k_input_kg_ha,
    resulting_s_input_kg_ha = resulting_s_input_kg_ha
  )
}

#' Calculate the mineral NPK 21-4-10 (default) amount required to match the
#' NH4-N already applied via digestate -- the core "comparable N input"
#' calculation underlying the Study 2 fertilisation treatments (mineral rates
#' matched to digestate NH4-N, not total N; see Study 2 methods).
calculate_npk_application <- function(digestate_nh4_content_kg_per_ton,
                                       digestate_applied_ton_per_ha,
                                       npk_n_content_percent = 21) {
  nh4_n_applied_kg_ha <- digestate_nh4_content_kg_per_ton * digestate_applied_ton_per_ha
  npk_needed_kg_ha <- nh4_n_applied_kg_ha / (npk_n_content_percent / 100)
  total_n_applied_from_npk_kg_ha <- npk_needed_kg_ha * (npk_n_content_percent / 100)

  list(
    nh4_n_applied_from_digestate_kg_ha = nh4_n_applied_kg_ha,
    npk_needed_kg_ha = npk_needed_kg_ha,
    total_n_applied_from_npk_kg_ha = total_n_applied_from_npk_kg_ha
  )
}

#' Vectorised version of calculate_npk_application() for a data frame of
#' multiple (split) digestate applications, with a summary total row appended.
#'
#' @param applications_df must contain columns digestate_applied_ton_per_ha,
#'   nh4_content_kg_per_ton, total_n_content_kg_per_ton
calculate_digestate_split_applications <- function(applications_df, npk_n_content_percent = 21) {
  required_cols <- c("digestate_applied_ton_per_ha", "nh4_content_kg_per_ton", "total_n_content_kg_per_ton")
  if (!all(required_cols %in% colnames(applications_df))) {
    stop(paste("Input dataframe must contain the following columns:", paste(required_cols, collapse = ", ")))
  }

  applications_df$nh4_n_applied_kg_ha <- applications_df$digestate_applied_ton_per_ha * applications_df$nh4_content_kg_per_ton
  applications_df$total_n_applied_kg_ha <- applications_df$digestate_applied_ton_per_ha * applications_df$total_n_content_kg_per_ton
  applications_df$npk_needed_kg_ha <- applications_df$total_n_applied_kg_ha / (npk_n_content_percent / 100)

  total_digestate_applied_ton_ha <- sum(applications_df$digestate_applied_ton_per_ha)
  total_nh4_n_applied_kg_ha <- sum(applications_df$nh4_n_applied_kg_ha)
  total_n_applied_kg_ha <- sum(applications_df$total_n_applied_kg_ha)
  total_npk_needed_kg_ha <- sum(applications_df$npk_needed_kg_ha)

  total_row <- data.frame(
    digestate_applied_ton_per_ha = total_digestate_applied_ton_ha,
    nh4_content_kg_per_ton = NA,
    total_n_content_kg_per_ton = NA,
    nh4_n_applied_kg_ha = total_nh4_n_applied_kg_ha,
    total_n_applied_kg_ha = total_n_applied_kg_ha,
    npk_needed_kg_ha = total_npk_needed_kg_ha
  )

  applications_summary_with_total <- rbind(applications_df, total_row)

  list(
    applications_summary = applications_summary_with_total,
    total_digestate_applied_ton_per_ha = total_digestate_applied_ton_ha,
    total_nh4_n_applied_kg_per_ha = total_nh4_n_applied_kg_ha,
    total_n_applied_kg_per_ha = total_n_applied_kg_ha,
    total_npk_needed_kg_per_ha = total_npk_needed_kg_ha
  )
}
