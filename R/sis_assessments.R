
#' Generate the assessments.csv file
#'
#' @param unique_id (character) Unique identifier.
#'
#' @return Returns an SIS compliant data frame
#' @export

sis_assessments = function(unique_id) {
  rationale_str = paste(
    "This species has a very wide distribution,",
    "large population,",
    "is not currently experiencing any major threats",
    "and no significant future threats have been identified.",
    "This species is therefore assessed as Least Concern."
  )

  combined_table <- purrr::map_dfr(unique_id, function(id) {
    tibble::tibble(
      internal_taxon_id = id,
      RedListRationale.value = rationale_str,
      MapStatus.status = "Done",
      RedListAssessmentDate.value = format(Sys.Date(), "%d/%m/%Y"),
      RedListCriteria.critVersion	= "3.1",
      RedListCriteria.manualCategory	= "LC",
      PopulationTrend.value	= "Stable",
      System.value	= "Terrestrial",
      Language.value	= "English",
      RangeDocumentation.narrative	= " ",
      PopulationDocumentation.narrative	= " ",
      HabitatDocumentation.narrative	= " ",
      ThreatsDocumentation.value	= " ",
      usetradedocumentation.value = " ",
      RedlistCriteria.isManual	= "TRUE",
      BiogeographicRealm.realm = ""
    )
  })

  return(combined_table)
}
