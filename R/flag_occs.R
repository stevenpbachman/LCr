#' Flags GBIF occurrence data based on common coordinate issues
#'
#' @param gbif_occs (data frame) GBIF occurrence file in DWCA format
#' @param native_ranges (data frame) Optional native ranges from `get_native_range()`
#'
#' @return A list containing:
#'   - flagged_data: original data with error flags
#'   - summary: summary of error counts
#'
#' @export
flag_occs <- function(gbif_occs, native_ranges = NULL, buffer = 1000) {
  # Create a copy of original data with flags initialized as FALSE
  flagged_occs <- gbif_occs %>%
    dplyr::mutate(
      flag_no_coords = FALSE,
      flag_cc_capitals = FALSE,
      flag_cc_centroids = FALSE,
      flag_cc_institutions = FALSE,
      flag_cc_equal = FALSE,
      flag_cc_gbif = FALSE,
      flag_cc_zeros = FALSE,
      flag_high_uncertainty = FALSE,
      flag_outside_native = FALSE
    )

  # Flag records without coordinates
  flagged_occs$flag_no_coords <- is.na(flagged_occs$decimalLongitude) |
    is.na(flagged_occs$decimalLatitude)

  # Only process records with coordinates for other flags
  occs_with_coords <- dplyr::filter(flagged_occs, !flag_no_coords)

  # Run CoordinateCleaner tests - IMPORTANT: Invert values since
  # cc_ functions return TRUE for VALID records and FALSE for PROBLEMATIC ones
  cc_test_capitals <- !CoordinateCleaner::cc_cap(
    occs_with_coords,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    value = "flagged"
  )

  cc_test_centroids <- !CoordinateCleaner::cc_cen(
    occs_with_coords,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    value = "flagged"
  )

  cc_test_institutions <- !CoordinateCleaner::cc_inst(
    occs_with_coords,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    value = "flagged"
  )

  cc_test_equal <- !CoordinateCleaner::cc_equ(
    occs_with_coords,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    value = "flagged"
  )

  cc_test_gbif <- !CoordinateCleaner::cc_gbif(
    occs_with_coords,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    value = "flagged"
  )

  cc_test_zeros <- !CoordinateCleaner::cc_zero(
    occs_with_coords,
    lat = "decimalLatitude",
    lon = "decimalLongitude",
    value = "flagged"
  )

  # Update flags in the original dataset
  for (i in seq_len(nrow(occs_with_coords))) {
    # Use a unique ID column for matching, defaulting to row number if necessary
    id_col <- if("gbifID" %in% colnames(flagged_occs)) "gbifID" else ".row_id"

    if(id_col == ".row_id" && !".row_id" %in% colnames(flagged_occs)) {
      flagged_occs$.row_id <- 1:nrow(flagged_occs)
      occs_with_coords$.row_id <- which(!flagged_occs$flag_no_coords)
    }

    row_id <- which(flagged_occs[[id_col]] == occs_with_coords[[id_col]][i])

    if (length(row_id) > 0) {
      flagged_occs$flag_cc_capitals[row_id] <- cc_test_capitals[i]
      flagged_occs$flag_cc_centroids[row_id] <- cc_test_centroids[i]
      flagged_occs$flag_cc_institutions[row_id] <- cc_test_institutions[i]
      flagged_occs$flag_cc_equal[row_id] <- cc_test_equal[i]
      flagged_occs$flag_cc_gbif[row_id] <- cc_test_gbif[i]
      flagged_occs$flag_cc_zeros[row_id] <- cc_test_zeros[i]
    }
  }

  # Flag high uncertainty coordinates
  flagged_occs$flag_high_uncertainty <-
    !is.na(flagged_occs$coordinateUncertaintyInMeters) &
    flagged_occs$coordinateUncertaintyInMeters > 100000

  # Check against native ranges if provided
  if (!is.null(native_ranges)) {
    # Temporary turn off s2 processing to avoid geometry errors
    sf::sf_use_s2(FALSE)

    # Only process records with valid coordinates
    valid_coords <- flagged_occs %>%
      dplyr::filter(!flag_no_coords) %>%
      sf::st_as_sf(
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = sf::st_crs("EPSG:4326")
      )

    valid_coords_projected <- valid_coords %>%
      sf::st_transform(crs = "+proj=eqearth +datum=WGS84")

    tdwg_level3_projected <- LCr::tdwg_level3 %>%
      sf::st_transform(crs = "+proj=eqearth +datum=WGS84") %>%
      dplyr::select(LEVEL3_COD)


    # Join with TDWG polygons
    valid_coords_tdwg <- valid_coords_projected %>%
      sf::st_join(tdwg_level3_projected %>%
                    sf::st_buffer(dist = buffer))

    # Extract the LEVEL3_COD values
    id_col <- if("gbifID" %in% colnames(flagged_occs)) "gbifID" else ".row_id"

    # Set up progress bar
    cli::cli_progress_bar(
      name = "Checking native range status",
      total = nrow(valid_coords_tdwg),
      format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
    )

    for (i in seq_len(nrow(valid_coords_tdwg))) {
      row_id <- which(flagged_occs[[id_col]] == valid_coords_tdwg[[id_col]][i])

      if (length(row_id) > 0) {
        # Check if species occurs in this region
        species_id <- valid_coords_tdwg$wcvp_ipni_id[i]
        region_code <- valid_coords_tdwg$LEVEL3_COD[i]

        if (!is.na(region_code) && !is.na(species_id)) {
          is_native <- any(native_ranges$wcvp_ipni_id == species_id &
                             native_ranges$LEVEL3_COD == region_code)

          flagged_occs$flag_outside_native[row_id] <- !is_native
        } else {
          # Region code or species ID is NA, so we can't determine if it's native
          flagged_occs$flag_outside_native[row_id] <- TRUE
        }
      }

      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  }
  print(paste0("native range check complete"))

  # Generate a summary of flags
  flag_summary <- data.frame(
    flag = c("Missing coordinates",
             "Near country capitals",
             "Near country centroids",
             "Near biodiversity institutions",
             "Equal coordinates",
             "GBIF headquarters",
             "Zero coordinates",
             "High coordinate uncertainty",
             "Outside native range"),
    count = c(
      sum(flagged_occs$flag_no_coords),
      sum(flagged_occs$flag_cc_capitals),
      sum(flagged_occs$flag_cc_centroids),
      sum(flagged_occs$flag_cc_institutions),
      sum(flagged_occs$flag_cc_equal),
      sum(flagged_occs$flag_cc_gbif),
      sum(flagged_occs$flag_cc_zeros),
      sum(flagged_occs$flag_high_uncertainty),
      sum(flagged_occs$flag_outside_native)
    ),
    percentage = c(
      round(100 * sum(flagged_occs$flag_no_coords) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_cc_capitals) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_cc_centroids) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_cc_institutions) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_cc_equal) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_cc_gbif) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_cc_zeros) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_high_uncertainty) / nrow(flagged_occs), 2),
      round(100 * sum(flagged_occs$flag_outside_native) / nrow(flagged_occs), 2)
    )
  )

  # Clean up temporary row ID if created
  if(".row_id" %in% colnames(flagged_occs)) {
    flagged_occs$.row_id <- NULL
  }

  return(list(
    flagged_data = flagged_occs,
    summary = flag_summary
  ))
}
