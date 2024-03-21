
#' Generate the habitats.csv file
#'
#' @param unique_id (character) Unique identifier.
#'
#' @return Returns an SIS compliant csv file
#' @export

sis_habitats = function(unique_id) {

  combined_table <- purrr::map_dfr(unique_id, function(id) {
  habitats <- tibble::tibble(internal_taxon_id=id,
                             GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsName="Unknown",
                             GeneralHabitats.GeneralHabitatsSubfield.GeneralHabitatsLookup="18",
                             GeneralHabitats.GeneralHabitatsSubfield.suitability = "Suitable",
                             GeneralHabitats.GeneralHabitatsSubfield.majorImportance	= "",
                             GeneralHabitats.GeneralHabitatsSubfield.season = ""
  )
  })

  return(combined_table)
}
