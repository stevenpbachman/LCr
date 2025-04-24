
#' Get native ranges for taxa
#'
#' @param keys (data frame) Contain identifier for the taxon e.g. derived from [`get_name_keys()`]
#'
#' @return (data frame) A list of 'botanical countries' (World Geogrpahic Scheme for Recording Plant
#' Distributions) where a taxon occurs.
#' @export
#'
#' @details Currently one option to get native ranges from (Plants of the
#' World Online) using WCVP identifier. Other options to be added later e.g. GIFT


get_native_range <- function(keys) {
  # Get the search ids
  search_ids <- as.vector(unlist(keys[, "wcvp_ipni_id"]))

  # Create an empty list to store results
  all_ranges <- list()

  # Loop through each ID
  for (i in seq_along(search_ids)) {
    # Get current ID
    current_id <- search_ids[i]

    # Get ranges for this ID
    ranges <- powo_range(current_id)

    # Only process if we got results
    if (nrow(ranges) > 0) {
      # Add the wcvp_ipni_id to each row
      ranges$wcvp_ipni_id <- current_id

      # Add to our list
      all_ranges[[i]] <- ranges
    }
  }

  # Combine all results
  if (length(all_ranges) > 0) {
    native_ranges <- dplyr::bind_rows(all_ranges)

    # Join with the keys dataframe to add GBIF_usageKey
    native_ranges <- dplyr::left_join(native_ranges,
                                      keys[, c("wcvp_ipni_id", "GBIF_usageKey")],
                                      by = "wcvp_ipni_id")

    names(native_ranges)[names(native_ranges) == "GBIF_usageKey"] <- "internal_taxon_id"

    return(native_ranges)
  } else {
    # Return empty dataframe with expected columns if no results
    return(data.frame(wcvp_ipni_id = character(0), internal_taxon_id = character(0)))
  }
}
