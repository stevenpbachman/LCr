
#' Generate the references.csv file
#'
#' @param wcvp_ipni_id (character) WCVP identifier.
#' @param powo_ref (logical) Default TRUE means a POWO reference will be added.
#' @param gbif_ref (data frame) A GBIF download citation according to IUCN format.
#'
#' @returns Returns an SIS compliant data frame
#' @export
#'

sis_references = function(wcvp_ipni_id, powo_ref = FALSE, gbif_ref = FALSE) {

  LCr_ref <-
  tibble::tibble(
    Reference_type = "Assessment",
    type = "electronic source",
    author = "Bachman, S., Brown, M.",
    year =  format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"),
    title = "LCr: Generate minimal Least Concern Red List assessments",
    url = "https://github.com/stevenpbachman/LCr",
    access_date = format(as.Date(Sys.Date(), format="%d/%m/%Y"))
  )

  sis_refs <- LCr_ref

    if (powo_ref == TRUE) {
  powo_ref <- powo_ref(wcvp_ipni_id)
  sis_refs <- dplyr::bind_rows(sis_refs, powo_ref)
    }

    if (gbif_ref == TRUE) {
    sis_refs <- dplyr::bind_rows(sis_refs, gbif_ref)
  }

  sis_refs$internal_taxon_id <- wcvp_ipni_id

  return(sis_refs)
}



