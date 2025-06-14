
#' Generate the allfields.csv file
#'
#' @param unique_id (character) Unique identifier.
#'
#' @return Returns an SIS compliant data frame
#' @export

sis_allfields <- function(unique_id, occs = NULL) {

  # Create base tibble
  combined_table <- purrr::map_dfr(unique_id, function(id) {
    tibble(
      internal_taxon_id = id,
      CurrentTrendDataDerivation.value = "Suspected",
      AreaRestricted.isRestricted = "No",
      AreaRestricted.justification = "This species does not have a restricted AOO or number of locations, based on occurrence data from GBIF (see references for DOI).",
      SevereFragmentation.isFragmented = "No",
      #NotUtilized.isNotUtilized = "TRUE",
      NoThreats.noThreats = "TRUE",
      ThreatsUnknown.value = "FALSE",
      GenerationLength.range = ""
    )
  })

  # If occs is provided, calculate and merge elevation data
  if (!is.null(occs)) {
    elevation_stats <- make_elevation(occs)

    combined_table <- combined_table %>%
      left_join(elevation_stats, by = "internal_taxon_id")
  }

  # If occs is provided, calculate EOO and AOO
  if (!is.null(occs)) {
    eoo_aoo_stats <- make_eoo_aoo(occs)

    combined_table <- combined_table %>%
      left_join(eoo_aoo_stats, by = "internal_taxon_id")
  } else{

    combined_table$AOO.range = ""
    combined_table$AOO.justification = ""
    combined_table$EOO.range = ""
    combined_table$EOO.justification = ""
  }

  return(combined_table)
}

