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
  all_ranges <- vector("list", length(search_ids))

  # Track species without native ranges
  no_range_ids <- character(0)

  # Set up a progress bar
  pb <- cli::cli_progress_bar(
    name = "Matching native ranges",
    total = length(search_ids),
    format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"
  )

  # Loop through each ID
  for (i in seq_along(search_ids)) {
    current_id <- search_ids[i]
    ranges <- powo_range(current_id)

    if (nrow(ranges) > 0 && !all(is.na(ranges$LEVEL3_COD))) {
      ranges$wcvp_ipni_id <- current_id
      all_ranges[[i]] <- ranges
    } else {
      # Track IDs that returned no ranges (or only NA values)
      no_range_ids <- c(no_range_ids, current_id)
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # Report species without native ranges
  if (length(no_range_ids) > 0) {
    cli::cli_alert_warning(
      "Native range not found for {length(no_range_ids)} species out of {length(search_ids)} total species"
    )
    cli::cli_inform("Species without native range: {.val {no_range_ids}}")
  } else {
    cli::cli_alert_success("Native ranges found for all {length(search_ids)} species")
  }

  if (length(Filter(Negate(is.null), all_ranges)) > 0) {
    native_ranges <- dplyr::bind_rows(all_ranges)

    native_ranges <- dplyr::left_join(
      native_ranges,
      keys[, c("wcvp_ipni_id", "GBIF_usageKey")],
      by = "wcvp_ipni_id"
    )

    names(native_ranges)[names(native_ranges) == "GBIF_usageKey"] <- "internal_taxon_id"

    return(native_ranges)
  } else {
    cli::cli_alert_danger("No native ranges found for any species")
    return(data.frame(wcvp_ipni_id = character(0), internal_taxon_id = character(0)))
  }
}
