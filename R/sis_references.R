
#' Generate the references.csv file
#'
#' @param unique_id (character) Unique identifier.
#' @param powo_ref (logical) Adds POWO reference if TRUE.
#' @param gbif_ref (data frame) A GBIF download citation according to IUCN format.
#'
#' @returns Returns an SIS compliant data frame
#' @export
#'

sis_references = function(unique_id, powo_ref = FALSE, gbif_ref) {

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
  powo_ref <- powo_ref()
  sis_refs <- dplyr::bind_rows(sis_refs, powo_ref)
    }
  if (!is.null(gbif_ref)) {
    sis_refs <- dplyr::bind_rows(sis_refs, gbif_ref)
  }

  sis_refs$internal_taxon_id <- unique_id

  return(sis_refs)
}

