
#' Convert cleaned gbif DWCA structure to IUCN SIS format
#'
#' @param occs_clean (character) Cleaned GBIF occurrence file in DWCA format. See
#' occs_clean() function.
#' @param first_name (character) First name of the assessor
#' @param second_name (character) Second name of the assessor
#' @param institution (character) Institution/affiliation of the assessor
#'
#' @return An dataframe of occurence points aligned with IUCN Red List spatial data standards
#' @export

make_sis_occs <- function(occs_clean, first_name = "", second_name = "", institution = ""){

  # Validate inputs
  validate_sis_inputs(occs_clean, first_name, second_name, institution)

  sis_points <- dplyr::select(occs_clean,
                              species,
                              recordedBy,
                              recordNumber,
                              day,
                              month,
                              year,
                              decimalLatitude,
                              decimalLongitude,
                              locality,
                              coordinateUncertaintyInMeters,
                              habitat,
                              basisOfRecord,
                              elevation,
                              catalogNumber,
                              speciesKey,
                              gbifID
  )

  sis_points <- dplyr::rename(sis_points,
                              basisofrec = basisOfRecord,
                              sci_name = species,
                              catalog_no = catalogNumber,
                              coorduncert = coordinateUncertaintyInMeters,
                              event_year = year,
                              min_elev = elevation,
                              recordedby = recordedBy,
                              recordno = recordNumber,
                              dist_comm = habitat,
                              dec_lat = decimalLatitude,
                              dec_long = decimalLongitude
  )

  sis_points <- dplyr::mutate(sis_points,
                              internal_taxon_id = speciesKey,
                              source = paste0("https://www.gbif.org/occurrence/", gbifID),
                              yrcompiled = format(Sys.Date(), "%Y"),
                              citation = institution,
                              compiler = paste0(first_name, " ", second_name),
                              data_sens = "0",
                              sens_comm = "",
                              presence = "1",
                              origin = "1",
                              seasonal = "1",
                              spatialref = "WGS84",
                              basisofrec = dplyr::recode(
                                basisofrec,
                                "FOSSIL_SPECIMEN" = "FossilSpecimen",
                                "HUMAN_OBSERVATION" = "HumanObservation",
                                "LIVING_SPECIMEN" = "LivingSpecimen",
                                "MACHINE_OBSERVATION" = "MachineObservation",
                                "PRESERVED_SPECIMEN" = "PreservedSpecimen",
                                "UNKNOWN" = "Unknown",
                                "OCCURRENCE" = "HumanObservation",
                                "LITERATURE" = "",
                                "OBSERVATION" = "HumanObservation",
                                "MATERIAL_ENTITY" = "",
                                "MATERIAL_SAMPLE" = "PreservedSpecimen",
                                "MATERIAL_CITATION" = "PreservedSpecimen"
                              ),
                              latitude = dec_lat,
                              longitude = dec_long
  )

  sis_points <- dplyr::select(sis_points, -speciesKey,-gbifID)

  # Validate final basisofrec values
  validate_basisofrec(sis_points)

  return(sis_points)
}

validate_sis_inputs <- function(occs_clean, first_name, second_name, institution) {
  cli::cli_progress_step("Validating inputs")

  errors <- list()
  warnings <- list()

  # --- Basic structure checks ---
  if (!is.data.frame(occs_clean)) {
    errors <- append(errors, list(c(
      "x" = "Input validation failed",
      "i" = cli::format_inline("{.arg occs_clean} must be a data frame, not {.cls {class(occs_clean)}}")
    )))
  } else if (nrow(occs_clean) == 0) {
    errors <- append(errors, list(c(
      "x" = "Input validation failed",
      "i" = "{.arg occs_clean} data frame is empty"
    )))
  }

  # --- Required columns ---
  required_cols <- c("species", "recordedBy", "recordNumber", "day", "month", "year",
                     "decimalLatitude", "decimalLongitude", "locality",
                     "coordinateUncertaintyInMeters", "habitat", "basisOfRecord",
                     "elevation", "catalogNumber", "speciesKey", "gbifID")

  missing_cols <- setdiff(required_cols, names(occs_clean))
  if (length(missing_cols) > 0) {
    errors <- append(errors, list(c(
      "x" = "Missing required columns",
      "i" = cli::format_inline("Missing: {.val {missing_cols}}")
    )))
  }

  # --- Parameter type checks ---
  if (!is.character(first_name)) {
    errors <- append(errors, list(c(
      "x" = "Invalid parameter type",
      "i" = cli::format_inline("{.arg first_name} must be a character string, not {.cls {class(first_name)}}")
    )))
  }
  if (!is.character(second_name)) {
    errors <- append(errors, list(c(
      "x" = "Invalid parameter type",
      "i" = cli::format_inline("{.arg second_name} must be a character string, not {.cls {class(second_name)}}")
    )))
  }
  if (!is.character(institution)) {
    errors <- append(errors, list(c(
      "x" = "Invalid parameter type",
      "i" = cli::format_inline("{.arg institution} must be a character string, not {.cls {class(institution)}}")
    )))
  }

  if (length(errors) > 0) {
    cli::cli_abort(setNames(errors, paste0("error_", seq_along(errors))))
  }

  # --- Column types ---
  if (!is.character(occs_clean$species)) {
    errors <- append(errors, list(c(
      "x" = "Invalid column type",
      "i" = cli::format_inline("{.field species} must be character, not {.cls {class(occs_clean$species)}}")
    )))
  }

  if (!is.character(occs_clean$basisOfRecord)) {
    errors <- append(errors, list(c(
      "x" = "Invalid column type",
      "i" = cli::format_inline("{.field basisOfRecord} must be character, not {.cls {class(occs_clean$basisOfRecord)}}")
    )))
  }

  numeric_cols <- c("day", "month", "year", "decimalLatitude", "decimalLongitude",
                    "coordinateUncertaintyInMeters", "elevation")

  for (col in numeric_cols) {
    if (!is.numeric(occs_clean[[col]])) {
      errors <- append(errors, list(c(
        "x" = "Invalid column type",
        "i" = cli::format_inline("{.field {col}} must be numeric, not {.cls {class(occs_clean[[col]])}}")
      )))
    }
  }

  if (length(errors) > 0) {
    cli::cli_abort(setNames(errors, paste0("error_", seq_along(errors))))
  }

  # --- Data validations ---
  data_errors <- list()

  # Helper for collecting GBIF IDs
  gbif_ids <- occs_clean$gbifID

  # Day validation
  if (any(na.omit(occs_clean$day) < 1 | na.omit(occs_clean$day) > 31)) {
    bad_rows <- which(occs_clean$day < 1 | occs_clean$day > 31)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Found invalid day {.val {unique(occs_clean$day[bad_rows])}}: see GBIF ID(s): {.val {gbif_ids[bad_rows]}}")
    ))
  }

  if (any(na.omit(occs_clean$month) < 1 | na.omit(occs_clean$month) > 12)) {
    bad_rows <- which(occs_clean$month < 1 | occs_clean$month > 12)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Found invalid month {.val {unique(occs_clean$month[bad_rows])}}: see GBIF ID(s): {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Year validation
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (any(na.omit(occs_clean$year) < 1700 | na.omit(occs_clean$year) > current_year)) {
    bad_rows <- which(occs_clean$year < 1700 | occs_clean$year > current_year)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Found invalid year: {.val {unique(occs_clean$year[bad_rows])}} see GBIF ID(s): {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Latitude
  if (any(na.omit(occs_clean$decimalLatitude) < -90 | na.omit(occs_clean$decimalLatitude) > 90)) {
    bad_rows <- which(occs_clean$decimalLatitude < -90 | occs_clean$decimalLatitude > 90)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Invalid latitudes: {.val {unique(occs_clean$decimalLatitude[bad_rows])}} see GBIF ID(s): {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Longitude
  if (any(na.omit(occs_clean$decimalLongitude) < -180 | na.omit(occs_clean$decimalLongitude) > 180)) {
    bad_rows <- which(occs_clean$decimalLongitude < -180 | occs_clean$decimalLongitude > 180)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Invalid longitudes: {.val {unique(occs_clean$decimalLongitude[bad_rows])}} see GBIF ID(s): {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Coordinate uncertainty
  if (any(na.omit(occs_clean$coordinateUncertaintyInMeters) < 0)) {
    bad_rows <- which(occs_clean$coordinateUncertaintyInMeters < 0)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Found negative values: {.val {unique(occs_clean$coordinateUncertaintyInMeters[bad_rows])}} see GBIF ID(s): {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Missing coordinates
  if (all(is.na(occs_clean$decimalLatitude) & is.na(occs_clean$decimalLongitude))) {
    data_errors <- append(data_errors, list(
      "i" = "Both latitude and longitude are NA for all rows"
    ))
  }

  # Empty or missing species
  if (any(is.na(occs_clean$species) | occs_clean$species == "")) {
    bad_rows <- which(is.na(occs_clean$species) | occs_clean$species == "")
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("{length(bad_rows)} records have empty species names; GBIF IDs: {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Binomial species check
  species_words <- strsplit(trimws(occs_clean$species), "\\s+")
  word_counts <- sapply(species_words, length)
  if (any(word_counts != 2, na.rm = TRUE)) {
    bad_rows <- which(word_counts != 2)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Species must be in 'Genus species' format. Problematic entries: {.val {unique(occs_clean$species[bad_rows])}}; GBIF IDs: {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Elevation
  if (any(na.omit(occs_clean$elevation) < -430 | na.omit(occs_clean$elevation) > 8849)) {
    bad_rows <- which(occs_clean$elevation < -430 | occs_clean$elevation > 8849)
    data_errors <- append(data_errors, list(
      "i" = cli::format_inline("Elevation out of range (-430 to 8849 m): {.val {unique(occs_clean$elevation[bad_rows])}}; GBIF IDs: {.val {gbif_ids[bad_rows]}}")
    ))
  }

  # Report data errors
  if (length(data_errors) > 0) {
    cli::cli_h1("Validation Summary")
    cli::cli_alert_danger("Found {.val {length(data_errors)}} issue{?s}:")
    #cli::cli_abort(setNames(data_errors, paste0("data_error_", seq_along(data_errors))))
    cli::cli_abort(setNames(unlist(data_errors), paste0("data_error_", seq_along(data_errors))))
  }

  cli::cli_progress_done()
  cli::cli_alert_success("Input validation completed successfully")
}

# Validation function for final basisofrec values
validate_basisofrec <- function(sis_points) {

  cli::cli_status("Validating basisOfRecord values")
  on.exit(cli::cli_status_clear(), add = TRUE)

  valid_basisofrec <- c(
    "PreservedSpecimen", "FossilSpecimen", "LivingSpecimen",
    "HumanObservation", "MachineObservation", "StillImage",
    "MovingImage", "SoundRecording", ""
  )

  # Identify rows with invalid basisofrec values
  invalid_rows <- !sis_points$basisofrec %in% valid_basisofrec

  # Collect errors
  data_errors <- character(0)

  if (any(invalid_rows)) {
    invalid_points <- sis_points[invalid_rows, c("source", "basisofrec")]

    data_errors <- paste0(
      "• Invalid basisOfRecord value {.val ", invalid_points$basisofrec,
      "} for source {.val ", invalid_points$source, "}"
    )
  }

  if (length(data_errors) > 0) {
    cli::cli_alert_danger("Found {.val {length(data_errors)}} invalid basisOfRecord value{?s}:")
    cli::cli_abort(setNames(unlist(data_errors), paste0("basisofrec_error_", seq_along(data_errors))))
  }

  cli::cli_alert_success("basisOfRecord validation completed successfully")
}



