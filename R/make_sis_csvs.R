

#' Generate all SIS connect csv files
#'
#' @param unique_id (character) Unique identifier - default is the GBIF usage key
#' @param first_name (character) First name of assessor
#' @param second_name (character) Second name of assessor
#' @param email (character) Email of assessor
#' @param institution (character) Name of institution or affiliation
#' @param gbif_ref (data frame) A GBIF download citation according to IUCN format.
#' @param powo_ref (data frame) A citation for use of POWO according to IUCN format.
#' @param native_ranges (data frame) Native ranges derived from [`get_native_range()`]
#' @param family (character) Field containing the family
#' @param genus (character) Field containing the genus
#' @param species (character) Field containing the specific epithet
#' @param taxonomicAuthority (character) Field containing the taxonomic authority
#' @param kingdom (character) Default is 'plantae', but can also be 'fungi'
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
           gbif_ref = NULL,
           powo_ref = FALSE,
           native_ranges = NULL,
           occs = NULL,
           family,
           genus,
           species,
           taxonomicAuthority,
           kingdom = "plantae") {
    if (kingdom == "plantae") {
      # get most of the csvs here
      if (!is.null(native_ranges)) {
      countries <- sis_countries(native_ranges, unique_id)
      #print("countries finished")
      cli::cli_alert_success("countries complete")
      }
      if (!is.null(occs)) {
      allfields <- sis_allfields(unique_id, occs = occs)
      #print("allfields finished")
      cli::cli_alert_success("allfields complete")
      } else {
        allfields <- sis_allfields(unique_id)
      }
      if (!is.null(occs)) {
        assessments <- sis_assessments(unique_id, native_ranges, wcvp_ipni_id, occs = occs)
        #print("assessments finished")
        cli::cli_alert_success("assessments complete")
      } else {
        assessments <- sis_assessments(unique_id, native_ranges, wcvp_ipni_id)
      }
      plantspecific <- sis_plantspecific(unique_id, wcvp_ipni_id, kingdom)
      #print("plantspecific finished")
      cli::cli_alert_success("plantspecific complete")
      habitats <- sis_habitats(unique_id)
      #print("habitats finished")
      cli::cli_alert_success("habitats complete")
      credits <- sis_credits(unique_id, first_name, second_name, email, affiliation = institution)
      #print("credits finished")
      cli::cli_alert_success("credits complete")
      taxonomy <- sis_taxonomy(unique_id, family, genus, species, taxonomicAuthority)
      #print("taxonomy finished")
      cli::cli_alert_success("taxonomy complete")

      # need to embed map into the function, but refs a bit awkward - try again later
      references <- purrr::map_dfr(unique_id, sis_references, powo_ref = powo_ref, gbif_ref = gbif_ref)
      #print("references finished")
      cli::cli_alert_success("references complete")
      # list of default results - these should always be generated
      results <-
        list(
          allfields = allfields,
          assessments = assessments,
          plantspecific = plantspecific,
          habitats = habitats,
          credits = credits,
          taxonomy = taxonomy,
          references = references
        )
      #print("results finished")
      cli::cli_alert_success("results complete")

      # countries df depends on native ranges, so only add countries if exists
      if (exists("countries")) {
        results$countries <- countries
      }

      return(results)
    }

    if (kingdom == "fungi") {
      # get most of the csvs here
      if (!is.null(native_ranges)) {
        countries <- sis_countries(native_ranges, unique_id)
      }
      if (!is.null(occs)) {
        allfields <- sis_allfields(unique_id, occs = occs)
      } else {
        allfields <- sis_allfields(unique_id)
      }
      assessments <- sis_assessments(unique_id, native_ranges)
      plantspecific <- sis_plantspecific(unique_id, kingdom)
      habitats <- sis_habitats(unique_id)
      credits <- sis_credits(unique_id, first_name, second_name, email, affiliation = institution)
      taxonomy <- sis_taxonomy(unique_id, family, genus, species, taxonomicAuthority)

      # need to embed map into the function, but refs a bit awkward - try again later
      references <- purrr::map_dfr(unique_id, sis_references, gbif_ref, powo_ref)

      # list of default results - these should always be generated
      results <-
        list(
          allfields = allfields,
          assessments = assessments,
          plantspecific = plantspecific,
          habitats = habitats,
          credits = credits,
          taxonomy = taxonomy,
          references = references
        )

      # countries df depends on native ranges, so only add countries if exists
      if (exists("countries")) {
        results$countries <- countries
      }

      return(results)

    }
  }
