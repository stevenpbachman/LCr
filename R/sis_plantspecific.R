
#' Generate the plantspecific.csv file
#'
#' @param unique_id (character) Unique identifier.
#' @param wcvp_ipni_id (character or NULL) Vector of WCVP IPNI IDs used to
#'   retrieve growth form data via POWO. Only required when
#'   \code{kingdom = "plantae"}; pass \code{NULL} (the default) for fungi.
#' @param kingdom (character) Kingdom of the taxa being assessed. One of
#'   \code{"plantae"} (default) or \code{"fungi"}. Controls which growth form
#'   logic is applied.
#'
#' @return An SIS compliant data frame
#' @export
sis_plantspecific <- function(unique_id, wcvp_ipni_id = NULL, kingdom = "plantae") {

  kingdom <- tolower(kingdom)

  if (kingdom == "plantae") {

    if (is.null(wcvp_ipni_id)) {
      cli::cli_abort(
        "{.arg wcvp_ipni_id} must be provided when {.arg kingdom} is {.val plantae}."
      )
    }

    cli::cli_inform("Retrieving growth form data via POWO for {length(wcvp_ipni_id)} {?taxon/taxa}.")

    # Generate habit text for each WCVP IPNI ID
    habit_data <- lapply(wcvp_ipni_id, function(id) {
      powo_result <- powo_text(id, occs = NULL, unique_id)
      return(powo_result$habit_text)
    })

    # Process each unique ID and create rows for all matching growth forms
    combined_table <- purrr::map2_dfr(
      unique_id, seq_along(unique_id),
      function(id, index) {
        hab_text <- habit_data[[index]]

        if (!is.null(LCr::lifeform_mapping_POWO_RL) && !is.null(hab_text) && hab_text != "") {
          matches <- LCr::lifeform_mapping_POWO_RL[
            LCr::lifeform_mapping_POWO_RL$lifeform_description == hab_text, ]

          if (nrow(matches) > 0) {
            return(purrr::map_dfr(seq_len(nrow(matches)), function(match_idx) {
              tibble::tibble(
                internal_taxon_id = id,
                PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName   = matches$description[match_idx],
                PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = matches$code[match_idx]
              )
            }))
          }
        }

        # Default if no matches found or no valid habit text
        cli::cli_warn("No growth form match found for taxon {.val {id}} — leaving blank.")
        tibble::tibble(
          internal_taxon_id = id,
          PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName   = "",
          PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = ""
        )
      }
    )

    combined_table <- combined_table %>%
      dplyr::filter(
        !is.na(PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName),
        !is.na(PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup)
      )

    return(combined_table)
  }

  if (kingdom == "fungi") {
    cli::cli_inform("Assigning standard fungal growth form to {length(unique_id)} {?taxon/taxa}.")
    combined_table <- purrr::map_dfr(unique_id, function(id) {
      tibble::tibble(
        internal_taxon_id = id,
        PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName   = "Fungus",
        PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup = "M"
      )
    })
    return(combined_table)
  }

  cli::cli_abort(
    "{.arg kingdom} must be one of {.val plantae} or {.val fungi}, not {.val {kingdom}}."
  )
}
