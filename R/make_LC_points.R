
#' Combined function to get clean SIS points file using name identifiers
#'
#' @param keys_df (data frame) Derived from [`get_name_keys()`] function. Must include at least GBIF_usageKey to obtain GBIF occurrences
#' @param first_name (character) First name of assessor
#' @param second_name (character) Second name of assessor
#' @param institution (character) Name of institution or affiliation
#' @param range_check (boolean) TRUE if you want to carry out occurrence cleaning with native range filter
#'
#' @return (list) Includes the GBIF "citation", and "points" the cleaned SIS compatible point file
#' @export
#' @details Designed for batch processing using keys derived from [`get_name_keys()`]

# update this!!!

make_lc_points <-
  function(keys_df, first_name = "", second_name = "", institution = "", range_check = FALSE, kingdom = "plantae") {

    # get the raw gbif occs
    gbif_points <- get_gbif_occs(keys_df)

    # keep the gbif citation to return later
    gbif_ref <- gbif_points$citation

    # clean the points - option to include native range
    if (range_check == TRUE) {

      # get native ranges based on kingdom parameter
      if (tolower(kingdom) == "plantae") {
        # use existing function for plants
        native_ranges <- get_native_range(keys = keys_df)
      } else if (tolower(kingdom) == "fungi") {
        # use alternate function for fungi
        native_ranges <- get_occs_range(keys = keys_df)
      } else {
        stop("kingdom must be either 'plantae' or 'fungi'")
      }

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
    if (range_check == TRUE) {
      # If it exists, add it to the results list
      res_list <- list("citation" = gbif_ref, "points" = final_points, "native_ranges" = native_ranges)
    } else {
      # If it doesn't exist, create the list without it
      res_list <- list("citation" = gbif_ref, "points" = final_points)
    }

    return(res_list)

  }
