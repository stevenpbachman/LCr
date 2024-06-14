
#' Generate the allfields.csv file
#'
#' @param unique_id (character) Unique identifier.
#'
#' @return Returns an SIS compliant data frame
#' @export

sis_allfields <- function(unique_id) {
  combined_table <- purrr::map_dfr(unique_id, function(id) {
    tibble(
      internal_taxon_id = id,
      CurrentTrendDataDerivation.value = "Suspected",
      AreaRestricted.isRestricted = "0",
      AreaRestricted.justification = "This species does not have a restricted AOO or number of locations, based on occurrence data from GBIF (see references for DOI).",
      AOO.range = "",
      AOO.justification = "",
      EOO.range = "",
      EOO.justification = "",
      SevereFragmentation.isFragmented = "No",
      NotUtilized.isNotUtilized = "TRUE",
      NoThreats.noThreats = "TRUE",
      ThreatsUnknown.value = "FALSE",
      GenerationLength.range = ""
    )
  })

  return(combined_table)
}
