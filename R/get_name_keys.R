
#' Get accepted GBIF and POWO identifiers (keys) for taxon names to support data downloads
#'
#' @param df character) Data frame with taxon names
#' @param name_col (character) Column for taxon names. Include taxonomic authority for better matching
#' @param tax_status (character) Default `any` provides all taxonomic, otherwise 'accepted'
#' @param match (character) Controls the output of the search. Use `single` to
#' force a single match result that has the highest confidence or `any` to return
#' all possible matches sorted in order of confidence
#'
#' @return Returns a data frame with accepted GBIF and POWO identifiers
#' @export
#' @details Designed for batch processing. Default setting (tax_status = "accepted" and match = "single")
#' returns an accepted name with the best single match against GBIF and POWO name backbones.
#' Final list may return fewer names if there are discrepancies e.g. accepted
#'  in GBIF, but not in POWO. Output data frame includes GBIF 'usageKey' that can be used with
#'   [`get_gbif_occs()`] to get occurrences from GBIF, and 'wcvp_ipni_id' that can be used with [`powo_range()`]
#' to get native ranges. To see a wider range of plausible matches adjust 'tax_status' and 'match' to 'any'.

# add option to determine which sources you want to search e.g. WCVP for plants, or IF for fungi
get_name_keys <- function(df, name_column, tax_status = "accepted", match = "single", kingdom = "Plantae") {
  # search terms
  search_names <- as.vector(unlist(df[, name_column]))

  # get the GBIF keys
  gbif_names_out <-
    purrr::map_dfr(search_names,
                   name_search_gbif,
                   match = match,
                   gbif_tax_stat = tax_status)
  colnames(gbif_names_out) <- paste0("GBIF", "_", colnames(gbif_names_out))

  keys_df <- gbif_names_out

  if (kingdom == "Plantae"){

  # get the POWO keys
    powo_names_out <-
      name_search_powo(df = df,
                       name_column = name_column,
                       powo_tax_stat = tax_status)

  # bind them together - need to fix when WCVP returns multiple matches
  keys_df <-
    dplyr::inner_join(gbif_names_out,
                      powo_names_out,
                      by = c("GBIF_searchName" = name_column))
  }

  keys_df <-
    dplyr::rename(keys_df, "searchName" = "GBIF_searchName")

  return(keys_df)

}
