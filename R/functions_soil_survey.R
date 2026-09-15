# Soil survey geostatistics: kriging interpolation of soil texture/organic
# matter by depth, producing spatial covariates for the Study 2 statistical
# models (the "spatial autocorrelation structure" objective). Ported from
# SOIL_SURVEY_ANALYSIS_V4F.Rmd STEP 1 (import), STEP 3 (sf/grid prep) and
# STEP 4 (kriging).

#' Read the combined soil survey workbook and derive streamlined depth bands.
#' Column names after clean_names(replace = mu_to_u) on the "COMBINED_CLEAN"
#' sheet: profil, substrip, plot, depth_cm, lat_north, lon_east, ler_2_um
#' (clay, <2um), silt/fine-sand and coarse-sand fractions, humus, total_c,
#' kan_udfyldes (source-of-sample marker), ca_co3, jb (Danish soil class).
load_soil_survey_xlsx <- function(path = "DATA/SOIL_SURVEY/SOIL_SURVEY_DATA_COMBINED.xlsx",
                                   sheet = "COMBINED_CLEAN") {
  readxl::read_excel(path, sheet = sheet) |>
    janitor::clean_names(replace = janitor:::mu_to_u) |>
    dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "na"))) |>
    dplyr::mutate(source = dplyr::case_when(
      grepl("Profil", kan_udfyldes, ignore.case = TRUE) ~ "Pit",
      TRUE ~ "Auger"
    )) |>
    dplyr::select(-kan_udfyldes) |>
    dplyr::mutate(
      depth_cm_streamlined = dplyr::case_when(
        depth_cm %in% c("0-25", "0-30") ~ "0-30",
        depth_cm %in% c("25-50", "30-60") ~ "30-60",
        depth_cm %in% c("50-90", "50-100", "60-", "90-", "100-") ~ "60-100+",
        TRUE ~ NA_character_
      ),
      # "+" is not a valid factor-level character for downstream file paths
      depth_cm_streamlined = gsub("\\+", "", depth_cm_streamlined)
    )
}

#' Derive the normalised (to 100%) clay/silt/sand texture fractions used for
#' kriging. Ported verbatim from SOIL_SURVEY_ANALYSIS_V4F.Rmd STEP 2/3 --
#' note the source's own (Danish-classification, not USDA) grouping: "silt"
#' here is coarse-silt (20-63um) PLUS fine-sand (63-200um) combined, and
#' "sand" is coarse-sand (200-2000um) only. This is intentional (repeated
#' identically at every depth split in the source notebook), not an error,
#' and is preserved rather than re-derived against a USDA convention.
derive_soil_texture_fractions <- function(soil_data) {
  soil_data |>
    dplyr::mutate(
      clay = ler_2_um,
      silt = grovsilt_20_63_um + finsand_63_200_um,
      sand = grovsand_200_2000_um
    ) |>
    tidyr::drop_na(clay, silt, sand) |>
    dplyr::mutate(total = clay + silt + sand) |>
    dplyr::mutate(
      clay = clay / total * 100,
      silt = silt / total * 100,
      sand = sand / total * 100
    ) |>
    dplyr::select(-total)
}

#' Convert the soil survey table to an sf point layer (UTM zone 32N,
#' EPSG:25832 -- matches the Foulum field coordinate system), keeping only
#' geolocated rows and dropping the 90/100 cm pit-profile duplicate depth
#' (samples judged not representative of the profile after streamlining
#' depth categories to "60-100").
#'
#' Requires lon_east/lat_north coordinate columns in the workbook (present
#' after load_soil_survey_xlsx()'s clean_names() step).
soil_survey_to_sf <- function(soil_data) {
  soil_sf <- soil_data |>
    dplyr::filter(!is.na(lat_north), !is.na(lon_east)) |>
    derive_soil_texture_fractions() |>
    sf::st_as_sf(coords = c("lon_east", "lat_north"), crs = 25832)

  soil_sf |>
    dplyr::filter(
      !(profil == "1" & depth_cm == "90-"),
      !(profil == "2" & depth_cm == "100-")
    )
}

#' Build a regular 1 m-resolution grid of cell centers over a buffered
#' (default 10 m) bounding box of the sample points.
build_kriging_grid <- function(soil_sf, grid_resolution = 1, buffer_m = 10) {
  soil_buffered <- sf::st_bbox(soil_sf) |>
    sf::st_as_sfc() |>
    sf::st_transform(sf::st_crs(soil_sf)) |>
    sf::st_buffer(dist = buffer_m)

  grid_sf <- sf::st_make_grid(soil_buffered, cellsize = grid_resolution, what = "centers") |>
    sf::st_as_sf()
  sf::st_crs(grid_sf) <- sf::st_crs(soil_sf)
  grid_sf
}

#' Ordinary kriging of one variable at one depth, using a manually specified
#' spherical variogram (psill = sample variance, range = half the maximum
#' inter-point distance, nugget = 10% of sample variance). Returns NULL
#' (with a message, not an error) when the variable lacks variability or
#' kriging otherwise fails, so a batch loop over many variable x depth
#' combinations can continue past one failure.
krige_manual <- function(data, variable, grid) {
  formula <- as.formula(paste(variable, "~ 1"))
  values <- data[[variable]]

  if (length(unique(values)) < 5 || var(values, na.rm = TRUE) < 1e-6) return(NULL)

  vgm_mod <- gstat::vgm(
    psill  = var(values, na.rm = TRUE),
    model  = "Sph",
    range  = max(dist(sf::st_coordinates(data))) / 2,
    nugget = 0.1 * var(values, na.rm = TRUE)
  )

  tryCatch({
    gstat::krige(formula, data, grid, model = vgm_mod)
  }, error = function(e) {
    message("Kriging failed for ", variable, ": ", e$message)
    NULL
  })
}

#' Loop krige_manual() over every combination of soil variable and depth
#' band, returning a named list of sf kriging results (list names:
#' "<depth>_<variable>", e.g. "0-30_sand").
krige_all_depths_variables <- function(soil_sf, grid_sf,
                                        variables = c("sand", "silt", "clay", "humus", "total_c")) {
  depths <- unique(soil_sf$depth_cm_streamlined)
  results <- list()
  for (d in depths) {
    df <- dplyr::filter(soil_sf, depth_cm_streamlined == d)
    for (v in variables) {
      kr <- krige_manual(df, v, grid_sf)
      if (!is.null(kr)) results[[paste(d, v, sep = "_")]] <- kr
    }
  }
  results
}

#' End-to-end soil-survey kriging pipeline: workbook -> per-depth spatial
#' covariate rasters. This is the function `_targets.R` calls to build the
#' Study 2 spatial covariates.
#'
#' @return named list of sf kriging result objects (see krige_all_depths_variables)
krige_soil_covariates <- function(soil_survey_xlsx = "DATA/SOIL_SURVEY/SOIL_SURVEY_DATA_COMBINED.xlsx") {
  soil_data <- load_soil_survey_xlsx(soil_survey_xlsx)
  soil_sf   <- soil_survey_to_sf(soil_data)
  grid_sf   <- build_kriging_grid(soil_sf)
  krige_all_depths_variables(soil_sf, grid_sf)
}

#' Plot one depth/variable's kriging result, with sample points overlaid.
plot_depth_kriging <- function(results, soil_sf, depth_tag, variable = "sand",
                                palette_color = "Reds") {
  key <- paste0(depth_tag, "_", variable)
  kr_obj <- results[[key]]
  if (is.null(kr_obj)) {
    message("No kriging result for ", key)
    return(invisible(NULL))
  }

  kr_df <- data.frame(
    x = sf::st_coordinates(kr_obj)[, 1],
    y = sf::st_coordinates(kr_obj)[, 2],
    z = kr_obj$var1.pred
  )
  kr_rast <- terra::rast(kr_df, type = "xyz", crs = sf::st_crs(kr_obj)$wkt)

  pts_sf <- soil_sf[soil_sf$depth_cm_streamlined == depth_tag, ]

  terra::plot(kr_rast,
              main = paste0(tools::toTitleCase(variable), " content (", depth_tag, " cm)"),
              col = terrain.colors(30), xlab = "Easting", ylab = "Northing")
  graphics::points(sf::st_coordinates(pts_sf), pch = 20, col = "black", cex = 1.2)
  invisible(kr_rast)
}
