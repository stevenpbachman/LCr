#' Generate the plantspecific.csv file
#'
#' @param unique_id (character) Unique identifier
#' @param wcvp_ipni_id (character) Vector of WCVP IPNI IDs
#' @param kingdom (character) Defaults to `plantae`, but can be `fungi`
#'
#' @return An SIS compliant csv file
#' @export

sis_plantspecific = function(unique_id, wcvp_ipni_id, kingdom = "plantae") {

  if (kingdom == "plantae") {
    # Generate habit text for each WCVP IPNI ID
    habit_data <- lapply(wcvp_ipni_id, function(id) {
      # Call powo_text and extract the habit_text element
      powo_result <- powo_text(id, occs = NULL, unique_id)
      return(powo_result$habit_text)
    })

    # Process each unique ID and create rows for all matching growth forms
    combined_table <- purrr::map2_dfr(unique_id, seq_along(unique_id), function(id, index) {
      # Get the habit text for this ID
      hab_text <- habit_data[[index]]

      # If we have valid habit text and a mapping table, look for matches
      if (!is.null(LCr::lifeform_mapping_POWO_RL) && !is.null(hab_text) && hab_text != "") {
        # Find all matching rows in the crosswalk table
        matches <- LCr::lifeform_mapping_POWO_RL[LCr::lifeform_mapping_POWO_RL$lifeform_description == hab_text, ]

        if (nrow(matches) > 0) {
          # Create one row for each matching growth form

          # Filter out rows with NA in either description or code
          matches <- matches[!is.na(matches$description) & !is.na(matches$code), ]

          if (nrow(matches) > 0) {
            return(purrr::map_dfr(1:nrow(matches), function(match_idx) {
              tibble::tibble(
                internal_taxon_id = id,
                PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName = matches$description[match_idx],
                PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = matches$code[match_idx]
              )
            }))
          }

          # return(purrr::map_dfr(1:nrow(matches), function(match_idx) {
          #   tibble::tibble(
          #     internal_taxon_id = id,
          #     PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName = matches$description[match_idx],
          #     PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = matches$code[match_idx]
          #   )
          # }))


        }
      }

      # Default if no matches found or no valid habit text
      tibble::tibble(
        internal_taxon_id = id,
        PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName = "",
        PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = "",
        powo_habit_text = hab_text
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

    return(combined_table)
  }
}
