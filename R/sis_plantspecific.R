#' Generate the plantspecific.csv file
#'
#' @param unique_id (character) Unique identifier
#' @param kingdom (character) Defaults to `plantae`, but can be `fungi`
#'
#' @return An SIS compliant csv file
#' @export

sis_plantspecific = function(unique_id, kingdom = "plantae") {

  if (kingdom == "plantae"){
  combined_table <- purrr::map_dfr(unique_id, function(id) {
    tibble::tibble(
      internal_taxon_id = id,
      PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName = "Tree - size unknown",
      PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = "T"
    )
  })

  return(combined_table)
  }
  if (kingdom == "fungi") {
    combined_table <- purrr::map_dfr(unique_id, function(id) {
      tibble::tibble(
        internal_taxon_id = id,
        PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName = "Fungus",
        PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = "M"
      )
    })
  }
}
