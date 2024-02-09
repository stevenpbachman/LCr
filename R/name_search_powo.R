
#' Query taxon name against POWO (Plants of the World Online) backbone
#' @description Queries a taxon name against the POWO names backbone. Simple wrapper around
#' the rWCVP `wcvp_match_names` function.
#'
#' @param name (character) A taxon name
#' @param powo_tax_stat (character) Default `any` provides all taxonomic
#' matches, otherwise `accepted` returns only accepted names according to POWO
#'
#' @return Returns a data frame with initial search term and matching name(s)
#' @export
#'
#' @examples
#' name_search_gbif("Poa annua L.")

name_search_powo <- function(name, powo_tax_stat = "any"){
  name_df <- data.frame(name = name)
  results <- rWCVP::wcvp_match_names(names_df = name_df, name_col = "name")

  # allow user to filter on accepted name only
  if (powo_tax_stat == "any"){
    results = results
  } else {
    if (powo_tax_stat == "accepted") {
      results = dplyr::filter(results, wcvp_status == "Accepted")
    }}
  return(results)
}
