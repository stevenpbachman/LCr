
#' Return native range of an accepted plant species from POWO (Plants of the
#' World Online) using WCVP identifier
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#'
#' @return (dataframe) Native range codes according to World Geographic Scheme for Recordings Plant Distributions (WGSRPD)

powo_range = function(wcvp_ipni_id){
  results = tibble::tibble(
    LEVEL3_COD=NA_character_,
    featureId=NA_character_,
    tdwgLevel=NA_integer_,
    establishment=NA_character_,
    LEVEL3_NAM=NA_character_,
    POWO_ID=NA_character_
  )

  returned_data <- powo_lookup(wcvp_ipni_id, distribution=TRUE)
  Sys.sleep(0.5)
  distribution <- returned_data$distribution$natives

  if (! is.null(distribution)) {
    results = dplyr::mutate(distribution, POWO_ID=wcvp_ipni_id)
    results = dplyr::rename(results, LEVEL3_NAM=name, LEVEL3_COD=tdwgCode)
    results = dplyr::mutate(results, LEVEL3_NAM=dplyr::recode(LEVEL3_NAM, "á"="a"))
  }

  return(results)
}
