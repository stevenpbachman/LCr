
#' Generate the assessments.csv file
#'
#' @param unique_id (character) Unique identifier.
#' @param native_ranges Data frame of native ranges used to calculate
#'   biogeographic realms. Pass \code{NULL} to skip realm calculation.
#' @param wcvp_ipni_id (character or NULL) WCVP IPNI identifiers used to
#'   generate distribution and habitat narrative text via POWO. Pass
#'   \code{NULL} for taxa without WCVP coverage (e.g. fungi), in which case
#'   \code{RangeDocumentation.narrative} and
#'   \code{HabitatDocumentation.narrative} will be left blank.
#' @param occs Occurrence data passed to \code{powo_text()}.
#'
#' @return Returns an SIS compliant data frame
#' @export
sis_assessments <- function(unique_id, native_ranges, wcvp_ipni_id = NULL, occs) {

  rationale_str <- paste(
    "This species has a very wide distribution,",
    "large population,",
    "is not currently experiencing any major threats",
    "and no significant future threats have been identified.",
    "This species is therefore assessed as Least Concern."
  )

  # --- Distribution and habitat narratives ---
  # Only generated when WCVP IPNI IDs are available (e.g. plants).
  # For taxa without WCVP coverage (e.g. fungi), these are left blank.
  if (!is.null(wcvp_ipni_id)) {
    cli::cli_inform("Generating distribution and habitat narratives via POWO.")
    powo_results    <- purrr::map2(wcvp_ipni_id, unique_id, ~ powo_text(.x, occs, .y))
    distribution_data <- purrr::map(powo_results, "iucn_dist_text")
    habitat_data      <- purrr::map(powo_results, "iucn_habit_text")
  } else {
    cli::cli_inform("No WCVP IPNI IDs provided — leaving distribution and habitat narratives blank.")
    distribution_data <- vector("list", length(unique_id))  # list of NULLs
    habitat_data      <- vector("list", length(unique_id))
  }

  # --- Build core assessment table ---
  combined_table <- purrr::map2_dfr(
    unique_id, seq_along(unique_id),
    function(id, index) {
      tibble::tibble(
        internal_taxon_id                  = id,
        RedListRationale.value             = rationale_str,
        MapStatus.status                   = "Done",
        MapStatus.narrative                = "Occurrence data sourced from GBIF, cleaned using standard protocols and native range filter using Plants of the World Online.",
        RedListAssessmentDate.value        = format(Sys.Date(), "%d/%m/%Y"),
        RedListCriteria.critVersion        = "3.1",
        RedListCriteria.manualCategory     = "LC",
        PopulationTrend.value              = "Stable",
        System.value                       = "Terrestrial",
        Language.value                     = "English",
        RangeDocumentation.narrative       = distribution_data[[index]] %||% "",
        PopulationDocumentation.narrative  = "This taxon has a widespread distribution across multiple countries; as such the global population is suspected to be large.",
        HabitatDocumentation.narrative     = habitat_data[[index]] %||% "",
        ThreatsDocumentation.value         = "This taxon may face threats associated with land use change in some areas of its wide distribution; however, no major threats have been identified that are likely to affect its extinction risk in a short time",
        RedlistCriteria.isManual           = "TRUE",
        BiogeographicRealm.realm           = ""
      )
    }
  )

  # ifelse(is.null(x), "", x)  # << alternative to %||% — the null-coalescing operator

  # --- Biogeographic realms (optional) ---
  if (!is.null(native_ranges)) {
    cli::cli_inform("Calculating biogeographic realms.")
    realm_data <- make_biorealms(native_ranges)
    combined_table <- combined_table %>%
      dplyr::left_join(realm_data, by = "internal_taxon_id") %>%
      dplyr::mutate(
        BiogeographicRealm.realm = dplyr::coalesce(
          BiogeographicRealm.realm.y,
          BiogeographicRealm.realm.x
        )
      ) %>%
      dplyr::select(-BiogeographicRealm.realm.x, -BiogeographicRealm.realm.y)
  }

  return(combined_table)
}
