#' Ensure a data frame has all required fields, filled with typed NAs if missing
#'
#' @param df (data frame) The occurrence data to check
#' @param required_fields (character) Vector of field names that must be present
#' @param numeric_fields (character) Subset of `required_fields` that should be
#'   numeric (filled with `NA_real_` rather than `NA_character_` if missing)
#'
#' @return (data frame) `df` with any missing fields added, and columns
#'   reordered to match `required_fields`
#' @keywords internal
ensure_required_fields <- function(df, required_fields, numeric_fields) {
  missing_fields <- setdiff(required_fields, names(df))

  if (length(missing_fields) > 0) {
    cli::cli_alert_warning("Adding missing fields not returned by GBIF: {.val {missing_fields}}")
    for (f in missing_fields) {
      df[[f]] <- if (f %in% numeric_fields) NA_real_ else NA_character_
    }
  }

  dplyr::select(df, dplyr::all_of(required_fields))
}

#' Download or search georeferenced occurrence data using GBIF name identifier
#'
#' @param keys_df (data frame) Derived from [`get_name_keys()`] function. Must include at least GBIF_usageKey to obtain GBIF occurrences
#' @param mode (character) Either \code{"search"} for a quick exploratory search
#'   or \code{"download"} for a full citable download. Default is \code{"search"}.
#'
#' @return (list) 'points' = the GBIF occurrence points, 'citation' = the GBIF
#'   citation (only available in \code{"download"} mode, \code{NULL} in search mode)
#' @export
#' @details In \code{"download"} mode, initiates a full occurrence download in DWCA
#'   format using [rgbif::occ_download()], which generates a citable DOI.
#'   In \code{"search"} mode, uses [rgbif::occ_search()] for a faster exploratory
#'   search without DOI generation. Search mode is recommended for initial data
#'   exploration; use download mode for final assessments.
get_gbif_occs <- function(keys_df, mode = "search") {

  if (!mode %in% c("search", "download")) {
    cli::cli_abort("{.arg mode} must be either {.val search} or {.val download}.")
  }

  # Define consistent fields for both modes
  required_fields <- c(
    "gbifID", "taxonKey", "speciesKey", "species",
    "recordedBy", "recordNumber", "day", "month", "year",
    "decimalLatitude", "decimalLongitude", "locality",
    "coordinateUncertaintyInMeters", "habitat", "basisOfRecord",
    "elevation", "catalogNumber", "countryCode"
  )

  numeric_fields <- c("coordinateUncertaintyInMeters", "elevation",
                       "decimalLatitude", "decimalLongitude", "day", "month", "year")

  if (mode == "search") {
    cli::cli_alert_info("Running in search mode - no DOI will be generated. Use {.val download} mode for final assessments.")

    the_points <- purrr::map_dfr(keys_df$GBIF_usageKey, function(key) {
      rgbif::occ_search(
        taxonKey = key,
        occurrenceStatus = "PRESENT",
        fields = required_fields,
        limit = 100000
      )$data
    })

    the_points <- ensure_required_fields(the_points, required_fields, numeric_fields)

    the_points$gbifID <- as.character(the_points$gbifID)
    the_points <- dplyr::left_join(the_points,
                                   keys_df,
                                   by = c("speciesKey" = "GBIF_usageKey"))

    cli::cli_alert_success("{nrow(the_points)} record{?s} retrieved in search mode.")
    return(list("citation" = NULL, "points" = the_points))

  } else {
    cli::cli_alert_info("Running in download mode - a citable DOI will be generated.")

    gbif_download <- rgbif::occ_download(
      rgbif::pred_in("taxonKey", keys_df$GBIF_usageKey),
      rgbif::pred("occurrenceStatus", "present"),
      format = "DWCA"
    )
    rgbif::occ_download_wait(gbif_download)
    temporary_folder <- tempdir()
    downloaded <- rgbif::occ_download_get(gbif_download, path = temporary_folder)
    the_points <- rgbif::occ_download_import(downloaded, path = temporary_folder)
    the_points <- dplyr::select(the_points, dplyr::any_of(required_fields))

    the_points <- ensure_required_fields(the_points, required_fields, numeric_fields)

    the_points$gbifID <- as.character(bit64::as.integer64(the_points$gbifID))
    the_points <- dplyr::left_join(the_points,
                                   keys_df,
                                   by = c("speciesKey" = "GBIF_usageKey"))

    meta <- rgbif::occ_download_meta(gbif_download)
    cite <- get_gbif_ref(meta)

    cli::cli_alert_success("{nrow(the_points)} record{?s} downloaded.")
    return(list("citation" = cite, "points" = the_points))
  }
}
