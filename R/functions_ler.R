# Apparent Land Equivalent Ratio (a-LER): combines relative crop yield and
# relative PV electricity output on a land-area basis to assess whether the
# vertical agrivoltaic system out-produces separate mono-cropping and PV
# installations (Study 2 RQ3 / system resilience and resource-use efficiency).
# Ported from a-LER_HYPERFARM.Rmd.
#
# NOTE (flagged in the Study 2 draft results section): the version of this
# analysis in the original notebook used an earlier subset of yield data, an
# external reference yield, and an electricity assumption that does not
# exactly match the measured PV figures -- it needs recalculation on the
# current, complete dataset. This port preserves the calculation logic
# exactly; recomputing it against `study2_yield_analysis_ready` and the real
# PV electricity output is a Study 2 pipeline TODO (see docs/concept paper).

#' Rename PV_PANEL_DATA.xlsx's PV_SETUP column to PV_SETUP_2 (the join key
#' process_yield_data() below uses) and relabel it to the "_BETWEEN_PV"
#' convention. Ported from a-LER_HYPERFARM.Rmd's `pv_data_preproc` step,
#' which the original notebook applies before ever calling process_yield_data()
#' -- omitting it (as an earlier port of this file did) produces a silent
#' `left_join(by = "PV_SETUP_2")` failure since the raw workbook only has
#' "PV_SETUP".
preprocess_pv_data <- function(pv_data) {
  pv_data |>
    dplyr::rename(PV_SETUP_2 = PV_SETUP) |>
    dplyr::mutate(
      PV_SETUP_2 = dplyr::case_when(
        PV_SETUP_2 == "VERTICAL" ~ "VERTICAL_BETWEEN_PV",
        PV_SETUP_2 == "SOUTH_ORIENTED" ~ "SOUTH_ORIENTED_BETWEEN_PV",
        PV_SETUP_2 == "REFERENCE" ~ "REFERENCE",
        TRUE ~ NA_character_
      )
    )
}

#' Summarise mean dry-weight yield by PV_SETUP_2 (setup x location combo) for
#' one product, and append the fixed "REFERENCE" (open-field, no PV) row.
#' @param pv_data must already be preprocess_pv_data()'d.
process_yield_data <- function(yield_data, pv_data, product_name, reference_data) {
  ready_data <- yield_data |>
    dplyr::mutate(
      PV_SETUP_2 = dplyr::case_when(
        PV_SETUP == "VERTICAL"       & LOCATION == "BETWEEN_PV" ~ "VERTICAL_BETWEEN_PV",
        PV_SETUP == "VERTICAL"       & LOCATION == "OUTSIDE_PV" ~ "VERTICAL_OUTSIDE_PV",
        PV_SETUP == "SOUTH_ORIENTED" & LOCATION == "BETWEEN_PV" ~ "SOUTH_ORIENTED_BETWEEN_PV",
        PV_SETUP == "SOUTH_ORIENTED" & LOCATION == "OUTSIDE_PV" ~ "SOUTH_ORIENTED_OUTSIDE_PV",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(PRODUCT == product_name) |>
    dplyr::group_by(PV_SETUP_2) |>
    dplyr::summarise(
      mean_dry_weight   = mean(DRY_WEIGHT_TON_HA, na.rm = TRUE),
      sd_dry_weight     = sd(DRY_WEIGHT_TON_HA, na.rm = TRUE),
      median_dry_weight = median(DRY_WEIGHT_TON_HA, na.rm = TRUE),
      count             = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::left_join(pv_data, by = "PV_SETUP_2") |>
    dplyr::select(PV_SETUP_2, mean_dry_weight, GCR, LL_HA,
                  TOTAL_AREA_OCCUPIED_M2, ELECTRICITY_KW_M2_YR,
                  ELECTRICITY_OUTPUT_MWH_HA_YR)

  dplyr::bind_rows(ready_data, reference_data)
}

#' Compute the apparent Land Equivalent Ratio: a-LER = crop_yield_component +
#' energy_output_component, where crop_yield_component adjusts the yield
#' ratio for land occupied by PV infrastructure.
calculate_a_LER <- function(combined_ler_data, use_global_mean = FALSE) {
  global_mean_dry_weight <- combined_ler_data |>
    dplyr::filter(grepl("OUTSIDE_PV", PV_SETUP_2)) |>
    dplyr::summarize(global_mean = mean(mean_dry_weight, na.rm = TRUE)) |>
    dplyr::pull(global_mean)

  combined_ler_data |>
    dplyr::mutate(
      crop_yield_ratio = dplyr::case_when(
        PV_SETUP_2 == "SOUTH_ORIENTED_BETWEEN_PV" ~ mean_dry_weight /
          ifelse(use_global_mean, global_mean_dry_weight,
                 combined_ler_data$mean_dry_weight[combined_ler_data$PV_SETUP_2 == "SOUTH_ORIENTED_OUTSIDE_PV"]),
        PV_SETUP_2 == "VERTICAL_BETWEEN_PV" ~ mean_dry_weight /
          ifelse(use_global_mean, global_mean_dry_weight,
                 combined_ler_data$mean_dry_weight[combined_ler_data$PV_SETUP_2 == "VERTICAL_OUTSIDE_PV"]),
        TRUE ~ NA_real_
      ),
      land_adjustment_factor = dplyr::case_when(
        PV_SETUP_2 %in% c("SOUTH_ORIENTED_BETWEEN_PV", "VERTICAL_BETWEEN_PV") ~
          ((TOTAL_AREA_OCCUPIED_M2 / 10000 - LL_HA) / (TOTAL_AREA_OCCUPIED_M2 / 10000)),
        TRUE ~ 1
      ),
      crop_yield_component = crop_yield_ratio * land_adjustment_factor
    ) |>
    dplyr::mutate(
      energy_output_component = ELECTRICITY_OUTPUT_MWH_HA_YR /
        combined_ler_data$ELECTRICITY_OUTPUT_MWH_HA_YR[combined_ler_data$PV_SETUP_2 == "REFERENCE"]
    ) |>
    dplyr::mutate(a_LER = crop_yield_component + energy_output_component)
}

#' Rename PV_SETUP_2 codes to display labels ("SOUTH-ORIENTED" / "VERTICAL" /
#' "REFERENCE"), dropping the _BETWEEN_PV / _OUTSIDE_PV suffix.
rename_pv_setup <- function(data) {
  data |>
    dplyr::mutate(
      PV_SETUP_2 = dplyr::case_when(
        PV_SETUP_2 == "SOUTH_ORIENTED_BETWEEN_PV" ~ "SOUTH-ORIENTED",
        PV_SETUP_2 == "VERTICAL_BETWEEN_PV" ~ "VERTICAL",
        PV_SETUP_2 == "REFERENCE" ~ "REFERENCE",
        TRUE ~ PV_SETUP_2
      )
    )
}

ler_treatment_colors <- c("REFERENCE" = "darkgreen", "SOUTH-ORIENTED" = "skyblue", "VERTICAL" = "orange")

plot_a_LER <- function(data, colors = ler_treatment_colors) {
  ggplot2::ggplot(dplyr::filter(data, !is.na(a_LER)),
                  ggplot2::aes(x = PV_SETUP_2, y = a_LER, fill = PV_SETUP_2)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Land Equivalent Ratio (LER)", x = "PV Setup", y = "LER") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

plot_crop_yield_component <- function(data, colors = ler_treatment_colors) {
  ggplot2::ggplot(dplyr::filter(data, !is.na(crop_yield_component)),
                  ggplot2::aes(x = PV_SETUP_2, y = crop_yield_component, fill = PV_SETUP_2)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Crop Yield Component of LER", x = "PV Setup", y = "Crop Yield Component") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

plot_energy_output_component <- function(data, colors = ler_treatment_colors) {
  ggplot2::ggplot(dplyr::filter(data, !is.na(energy_output_component)),
                  ggplot2::aes(x = PV_SETUP_2, y = energy_output_component, fill = PV_SETUP_2)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Energy Output Component of LER", x = "PV Setup", y = "Energy Output Component") +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Combine the three a-LER component plots side by side with patchwork.
plot_combined_a_LER <- function(data, product_type) {
  p1 <- plot_a_LER(data)
  p2 <- plot_crop_yield_component(data)
  p3 <- plot_energy_output_component(data)

  (p1 | p2 | p3) +
    patchwork::plot_annotation(
      title = paste("LER Analysis for", product_type),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 20))
    ) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "none")
}

#' Add relative crop-yield and relative energy-yield percentage columns
#' (relative to either the global mean or the PV-system-specific mean of the
#' OUTSIDE_PV reference), plus land-loss percentage.
add_relative_yield_columns <- function(data, use_global_mean = FALSE) {
  global_means <- data |>
    dplyr::filter(grepl("_OUTSIDE_PV$", PV_SETUP_2)) |>
    dplyr::group_by(Product) |>
    dplyr::summarise(global_mean_dry_weight = mean(mean_dry_weight, na.rm = TRUE), .groups = "drop")

  pv_system_means <- data |>
    dplyr::filter(grepl("_OUTSIDE_PV$", PV_SETUP_2)) |>
    dplyr::group_by(Product, PV_SETUP_2) |>
    dplyr::summarise(pv_system_mean_dry_weight = mean(mean_dry_weight, na.rm = TRUE), .groups = "drop")

  data <- data |>
    dplyr::left_join(global_means, by = "Product") |>
    dplyr::left_join(pv_system_means, by = c("Product", "PV_SETUP_2"))

  ref_mean <- function(product, setup_suffix) {
    if (use_global_mean) {
      global_means$global_mean_dry_weight[global_means$Product == product]
    } else {
      pv_system_means$pv_system_mean_dry_weight[
        pv_system_means$Product == product &
        pv_system_means$PV_SETUP_2 == paste0(setup_suffix, "_OUTSIDE_PV")
      ]
    }
  }

  data |>
    dplyr::mutate(
      relative_crop_yield_vertical = dplyr::case_when(
        PV_SETUP_2 == "VERTICAL_BETWEEN_PV" ~
          (mean_dry_weight / ref_mean(Product, "VERTICAL")) * 100,
        TRUE ~ NA_real_
      ),
      relative_crop_yield_south_oriented = dplyr::case_when(
        PV_SETUP_2 == "SOUTH_ORIENTED_BETWEEN_PV" ~
          (mean_dry_weight / ref_mean(Product, "SOUTH_ORIENTED")) * 100,
        TRUE ~ NA_real_
      ),
      relative_energy_yield_vertical = dplyr::case_when(
        PV_SETUP_2 == "VERTICAL_BETWEEN_PV" ~
          (ELECTRICITY_OUTPUT_MWH_HA_YR / data$ELECTRICITY_OUTPUT_MWH_HA_YR[data$PV_SETUP_2 == "REFERENCE"]) * 100,
        TRUE ~ NA_real_
      ),
      relative_energy_yield_south_oriented = dplyr::case_when(
        PV_SETUP_2 == "SOUTH_ORIENTED_BETWEEN_PV" ~
          (ELECTRICITY_OUTPUT_MWH_HA_YR / data$ELECTRICITY_OUTPUT_MWH_HA_YR[data$PV_SETUP_2 == "REFERENCE"]) * 100,
        TRUE ~ NA_real_
      ),
      land_loss_vertical = dplyr::case_when(
        PV_SETUP_2 == "VERTICAL_BETWEEN_PV" ~ (LL_HA / (TOTAL_AREA_OCCUPIED_M2 / 10000)) * 100,
        TRUE ~ NA_real_
      ),
      land_loss_south_oriented = dplyr::case_when(
        PV_SETUP_2 == "SOUTH_ORIENTED_BETWEEN_PV" ~ (LL_HA / (TOTAL_AREA_OCCUPIED_M2 / 10000)) * 100,
        TRUE ~ NA_real_
      )
    )
}

#' Produce a formatted kableExtra summary table (LER, relative yields, land
#' loss) for one product, restricted to the BETWEEN_PV rows.
create_clean_product_table <- function(data, product_name) {
  data |>
    dplyr::filter(Product == product_name & !grepl("_OUTSIDE_PV", PV_SETUP_2)) |>
    dplyr::mutate(
      relative_crop_yield = dplyr::case_when(
        grepl("VERTICAL", PV_SETUP_2) ~ relative_crop_yield_vertical,
        grepl("SOUTH_ORIENTED", PV_SETUP_2) ~ relative_crop_yield_south_oriented,
        TRUE ~ NA_real_
      ),
      relative_energy_yield = dplyr::case_when(
        grepl("VERTICAL", PV_SETUP_2) ~ relative_energy_yield_vertical,
        grepl("SOUTH_ORIENTED", PV_SETUP_2) ~ relative_energy_yield_south_oriented,
        TRUE ~ NA_real_
      ),
      land_loss = dplyr::case_when(
        grepl("VERTICAL", PV_SETUP_2) ~ land_loss_vertical,
        grepl("SOUTH_ORIENTED", PV_SETUP_2) ~ land_loss_south_oriented,
        TRUE ~ NA_real_
      ),
      dplyr::across(
        c(a_LER, relative_crop_yield, relative_energy_yield, land_loss,
          mean_dry_weight, ELECTRICITY_OUTPUT_MWH_HA_YR),
        ~ ifelse(is.na(.), " ", format(round(., 2), nsmall = 2))
      )
    ) |>
    dplyr::select(PV_SETUP_2, a_LER, relative_crop_yield, relative_energy_yield,
                  land_loss, mean_dry_weight, ELECTRICITY_OUTPUT_MWH_HA_YR) |>
    knitr::kable(
      format = "html",
      col.names = c("PV Setup", "LER", "Relative Crop Yield (%)", "Relative Energy Yield (%)",
                    "Land Loss (%)", "Mean Dry Weight (t/ha)", "Electricity Output (MWh/ha/yr)"),
      caption = paste("Relative Yields, a_LER, and Land Loss for", product_name)
    ) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                              full_width = FALSE, position = "center") |>
    kableExtra::row_spec(0, bold = TRUE, color = "white", background = "#4CAF50")
}
