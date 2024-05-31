
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


get_native_range <- function(keys) { #,name_col

  # get the search ids
  search_ids <- as.vector(unlist(keys[, "wcvp_ipni_id"]))

  # run the powo range function through map_dfr
  native_ranges <- purrr::map_dfr(search_ids, powo_range)

  return(native_ranges)

}

