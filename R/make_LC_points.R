
#' Combined function to get clean SIS points file using name identifiers
#'
#' @param keys_df (data frame) Derived from [`get_name_keys()`] function. Must include at least GBIF_usageKey to obtain GBIF occurrences
#' @param first_name (character) First name of assessor
#' @param second_name (character) Second name of assessor
#' @param institution (character) Name of institution or affiliation
#' @param range_check (boolean) TRUE if you want to carry out occurrence cleaning with native range filter
#'
#' @return (list) Includes the GBIF citation, and "points" the cleaned SIS compatible point file
#' @export
#' @details Designed for batch processing using keys derived from [`get_name_keys()`]


make_LC_points <-
  function(keys_df, first_name = "", second_name = "", institution = "", range_check = FALSE) {

    # get the raw gbif occs
    gbif_points <- get_gbif_occs(keys_df)

    # keep the gbif citation to return later
    gbif_ref <- gbif_points$citation

    # clean the points - option to include native range
    if (range_check == TRUE) {

      # get native ranges
      native_ranges <- get_native_range(keys = keys_df)

      # run the cleaning
      lc_points <- clean_occs(gbif_points$points, native_ranges)

    } else {

      # if no native ranges, do normal CC clean
      lc_points <- clean_occs(gbif_points$points)

    }

    # make the SIS points
    final_points <- make_sis_occs(lc_points,
                                first_name = first_name,
                                second_name = second_name,
                                institution = institution)

    # Check if native_ranges exists
    if (!is.null(native_ranges)) {
      # If it exists, add it to the results list
      res_list <- list("citation" = gbif_ref, "points" = final_points, "native_ranges" = native_ranges)
    } else {
      # If it doesn't exist, create the list without it
      res_list <- list("citation" = gbif_ref, "points" = final_points)
    }

    #res_list <- list("citation" = gbif_ref, "points" = final_points)
    return(res_list)

  }
