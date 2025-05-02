
#' Generate the elevation upper and lower bounds for the allfields.csv file
#'
#' @param occs
#'
#' @return Returns upper and lower elevation based on elevation field in GBIF occurrence file.
#' @export

make_elevation <- function(occs) {
  elevation_stats <- occs %>%
    dplyr::group_by(internal_taxon_id) %>%
    dplyr::summarise(
      ElevationLower.limit = round(quantile(min_elev, 0.05, na.rm = TRUE)),
      ElevationUpper.limit = round(quantile(min_elev, 0.95, na.rm = TRUE)),
      #n = n(),
      .groups = "drop"
    )

  return(elevation_stats)
}
