
#' Get accepted GBIF and POWO identifiers (keys) for taxon names to support data downloads
#'
#' @param names_df character) Dataframe with taxon names
#' @param name (character) Column for taxon names
#'
#' @return Returns a data frame with accepted GBIF and POWO identifiers
#' @export

get_name_keys <- function(names_df, name){

  # search terms
  search_names <- as.vector(unlist(names_df[, name]))

  # get the GBIF keys
  gbif_names_out <- purrr::map_dfr(search_names, name_search_gbif, match = "single", gbif_tax_stat = "accepted")
  colnames(gbif_names_out) <- paste0("GBIF", "_", colnames(gbif_names_out))

  # get the POWO keys
  powo_names_out <- purrr::map_dfr(names_df, name_search_powo, powo_tax_stat = "accepted")

  # bind them together - need to fix when WCVP return multiple matches
  keys_df <- dplyr::inner_join(gbif_names_out, powo_names_out, by = c("GBIF_searchName" = "name"))

  keys_df <- dplyr::rename(keys_df, "searchName" = "GBIF_searchName")

  return(keys_df)

}
