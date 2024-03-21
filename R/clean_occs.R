#' Cleans a GBIF occurrence download using CoordinateCleaner and native
#' range filter.
#'
#' @param gbif_occs (data frame) GBIF occurrence file in DWCA format. If [`get_gbif_occs()`] was used
#' to get GBIF occurrences file, note that the output is a list, so you need to e.g. my_occs$points
#' @param native_ranges (data frame) Native ranges prepared using [`get_native_range()`]
#'
#' @return a dataframe with cleaned occurrence data
#' @export

clean_occs <- function(gbif_occs, native_ranges = NULL){

  occs_ll <- tidyr::drop_na(gbif_occs, "decimalLongitude",
                            "decimalLatitude")

  # coordinate cleaner defaults
  cleaned_occs <-
    CoordinateCleaner::clean_coordinates(
      occs_ll,
      lat = "decimalLatitude",
      lon = "decimalLongitude",
      species = "scientificName",
      tests = c("capitals", "centroids", "institutions",
                "equal", "gbif", "zeros"),
      centroids_detail = 'country',
      value = "clean"
    )

  # remove highly uncertain coords
  cleaned_occs <-
    dplyr::filter(cleaned_occs, coordinateUncertaintyInMeters <= 100000 | is.na(coordinateUncertaintyInMeters) )

  # clean using POWO native range
  if (!is.null(native_ranges)) {

    distributions <- native_ranges

    # reduce distributions to id and L3 code
    distributions_IDs <- dplyr::select(distributions, POWO_ID | LEVEL3_COD)

    # getting invalid spherical geometry error - suggest turn off the s2 processing
    # https://stackoverflow.com/questions/68808238/how-to-fix-spherical-geometry-errors-caused-by-conversion-from-geos-to-s2
    sf::sf_use_s2(FALSE)

    # join species distributions with TDWG polygons
    cleaned_occs <-
      cleaned_occs %>%
      sf::st_as_sf(
        coords = c("decimalLongitude", "decimalLatitude"),
        crs = sf::st_crs("EPSG:4326")
      ) %>%
      sf::st_join(LCr::tdwg_level3 %>% dplyr::select(LEVEL3_COD)) %>%
      dplyr::inner_join(
        distributions_IDs,
        by = c("wcvp_ipni_id" = "POWO_ID", "LEVEL3_COD"),
        keep = TRUE
      )

    # Use st_coordinates to extract latitude and longitude
    coordinates <- sf::st_coordinates(cleaned_occs)

    # Extract latitude and longitude columns
    latitude <- coordinates[, "Y"]
    longitude <- coordinates[, "X"]

    cleaned_occs <- dplyr::mutate(cleaned_occs,
                                  decimalLongitude = longitude,
                                  decimalLatitude = latitude)
    cleaned_occs <- sf::st_drop_geometry(cleaned_occs)

  }

  return(cleaned_occs)

}
