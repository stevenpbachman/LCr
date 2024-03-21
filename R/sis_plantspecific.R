#' Generate the plantspecific.csv file
#'
#' @param unique_id (character) Unique identifier.
#'
#' @return An SIS compliant csv file
#' @export

sis_plantspecific = function(unique_id) {
  combined_table <- purrr::map_dfr(unique_id, function(id) {
    tibble::tibble(
      internal_taxon_id = id,
      PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName = "Tree - size unknown",
      PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = "T"
    )
  })

  return(combined_table)
}
