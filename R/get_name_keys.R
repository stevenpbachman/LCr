
#' Get accepted GBIF and POWO identifiers (keys) for taxon names to support data downloads
#'
#' @param df character) Data frame with taxon names
#' @param name_col (character) Column for taxon names. Include taxonomic authority for better matching
#' @param match (character) Controls the output of the search. Use `single` to
#' force a single match result that has the highest confidence or `any` to return
#' all possible matches sorted in order of confidence
#' @param kingdom (character) Defaults to `plantae` and carries out match to POWO. If set as `fungi`
#' will only carry out GBIF search
#'
#' @return Returns a data frame with accepted GBIF and POWO identifiers
#' @export
#' @details Designed for batch processing. Default setting (match = "single", kingdom = "plantae")
#' returns an accepted name with the best single match against GBIF and POWO name backbones (GBIF
#' only for kingdom = "fungi"). Final list may return fewer names if there are discrepancies e.g. accepted
#'  in GBIF, but not in POWO. Output data frame includes GBIF 'usageKey' that can be used with
#'   [`get_gbif_occs()`] to get occurrences from GBIF, and 'wcvp_ipni_id' that can be used with [`powo_range()`]
#' to get native ranges. To see a wider range of plausible matches adjust 'match' to 'any'.

# add option to determine which sources you want to search e.g. WCVP for plants, or IF for fungi
get_name_keys <- function(df, name_column, match = "single", kingdom = "plantae") {
  # Create a working copy of the dataframe
  working_df <- df

  # Parse names into binomial and author components
  search_names <- as.vector(unlist(working_df[, name_column]))

  # use gbif parser
  name_parts <- purrr::map(search_names,  rgbif::name_parse)
  name_parts <- dplyr::bind_rows(name_parts)

  if (all(c("bracketauthorship", "authorship") %in% names(name_parts))) {
    working_df <- name_parts %>%
      dplyr::mutate(
        taxonomicAuthority = dplyr::case_when(
          !is.na(bracketauthorship) ~ paste0("(", bracketauthorship, ") ", authorship),
          !is.na(authorship) ~ authorship,
          TRUE ~ ""
        )
      )
  } else {
    # If either column is missing, just copy the input df
    working_df <- name_parts
  }

  #get the GBIF keys using full name strings - canonicalnamecomplete
  gbif_names_out <- purrr::map_dfr(
    working_df$canonicalnamecomplete,  # extract just the column of names
    ~name_search_gbif(name = .x, match = "single")  # use .x to refer to each value
  )

  # label as GBIF columns to distinguish from POWO
  colnames(gbif_names_out) <- paste0("GBIF", "_", colnames(gbif_names_out))

  # important  add the search name back in to match later
  gbif_names_out$GBIF_searchName <- search_names
  keys_df <- gbif_names_out

  if (kingdom == "plantae") {

    if ("taxonomicAuthority" %in% names(working_df)) {
      powo_names_out <- name_search_powo(
        df = working_df,
        name_col = "canonicalname",
        author_col = "taxonomicAuthority"
      )
    } else {
      powo_names_out <- name_search_powo(
        df = working_df,
        name_col = "canonicalname"
        # omit author_col entirely
      )
    }

    # Join by the original search name
    keys_df <-
      dplyr::inner_join(gbif_names_out,
                        powo_names_out,
                        by = c("GBIF_searchName" = "scientificname"))
  }

  # rename
  keys_df <-
    dplyr::rename(keys_df, "searchName" = "GBIF_searchName")

  return(keys_df)
}
