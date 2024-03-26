
#' Generate all SIS connect csv files
#'
#' @param keys (data frame) Derived from [`get_name_keys()`] function. Must include at least GBIF_usageKey to obtain GBIF occurrences
#' @param first_name (character) First name of assessor
#' @param second_name (character) Second name of assessor
#' @param email (character) Email of assessor
#' @param institution (character) Name of institution or affiliation
#' @param gbif_ref (data frame) A GBIF download citation according to IUCN format.
#' @param native_ranges (data frame) Native ranges derived from [`get_native_range()`]
#'
#' @return Returns an SIS compliant zip file
#' @export

make_sis_csvs <-
  function(unique_id,
           wcvp_ipni_id,
           first_name,
           second_name,
           email,
           institution,
           gbif_ref,
           native_ranges) {

  # get most of the csvs here
    countries <- sis_countries(unique_id, wcvp_ipni_id, native_ranges)
    allfields <- sis_allfields(unique_id)
    assessments <- sis_assessments(unique_id)
    plantspecific <- sis_plantspecific(unique_id)
    habitats <- sis_habitats(unique_id)
    credits <- sis_credits(unique_id, first_name, second_name, email, affiliation=institution)

  # need to embed map into the function, but these are a bit awkward - try again later
    taxonomy <- purrr::map2(unique_id, wcvp_ipni_id, sis_taxonomy) %>%  dplyr::bind_rows()
    references <- purrr::map_dfr(unique_id, sis_references, powo_ref = TRUE, gbif_ref)

  return(list(allfields = allfields,
              assessments = assessments,
              plantspecific = plantspecific,
              habitats = habitats,
              taxonomy = taxonomy,
              credits = credits,
              references = references,
              countries = countries
              )
  )

  return(all_cvs)

}
