
#' Generate the assessments.csv file
#'
#' @param unique_id (character) Unique identifier.
#'
#' @return Returns an SIS compliant data frame
#' @export

sis_assessments = function(unique_id, native_ranges, wcvp_ipni_id) {
  rationale_str = paste(
    "This species has a very wide distribution,",
    "large population,",
    "is not currently experiencing any major threats",
    "and no significant future threats have been identified.",
    "This species is therefore assessed as Least Concern."
  )

  # Generate distribution text for each WCVP IPNI ID
  distribution_data <- lapply(wcvp_ipni_id, function(id) {
    # Call powo_text and extract the iucn_dist_text element
    powo_result <- powo_text(id)
    return(powo_result$iucn_dist_text)
  })

  # Generate habitat text for each WCVP IPNI ID
  habitat_data <- lapply(wcvp_ipni_id, function(id) {
    # Call powo_text and extract the iucn_dist_text element
    powo_result <- powo_text(id)
    return(powo_result$iucn_habit_text)
  })



  combined_table <- purrr::map2_dfr(unique_id, seq_along(unique_id), function(id, index) {

    dist_text <- distribution_data[[index]]
    hab_text <- habitat_data[[index]]

    tibble::tibble(
      internal_taxon_id = id,
      RedListRationale.value = rationale_str,
      MapStatus.status = "Done",
      MapStatus.narrative = "Occurrence data sourced from GBIF, cleaned using standard protocols and native range filter using Plants of the World Online.",
      RedListAssessmentDate.value = format(Sys.Date(), "%d/%m/%Y"),
      RedListCriteria.critVersion	= "3.1",
      RedListCriteria.manualCategory	= "LC",
      PopulationTrend.value	= "Stable",
      System.value	= "Terrestrial",
      Language.value	= "English",
      RangeDocumentation.narrative	= dist_text,
      PopulationDocumentation.narrative	= "This taxon has a widespread distribution across multiple countries; as such the global population is suspected to be large.",
      HabitatDocumentation.narrative	= hab_text,
      ThreatsDocumentation.value	= "This taxon may face threats associated with land use change in some areas of its wide distribution; however, no major threats have been identified that are likely to affect its extinction risk in a short time",
      usetradedocumentation.value = "This taxon is not known to be utilised",
      RedlistCriteria.isManual	= "TRUE",
      BiogeographicRealm.realm = ""
    )
  })

  # If native_ranges and TDWG_realms are provided, calculate realms and merge
  if (!is.null(native_ranges)) {
    realm_data <- make_biorealms(native_ranges)

    combined_table <- combined_table %>%
      left_join(realm_data, by = "internal_taxon_id") %>%
      mutate(BiogeographicRealm.realm = coalesce(BiogeographicRealm.realm.y, BiogeographicRealm.realm.x)) %>%
      select(-BiogeographicRealm.realm.x, -BiogeographicRealm.realm.y)
  }

  return(combined_table)
}
