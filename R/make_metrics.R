
#' Generate metrics from a clean point occurrence file
#'
#' @param occs_clean (character) Cleaned GBIF occurrence file in DWCA format. See
#' occs_clean() function.
#' @param native_ranges (df) WGSRPD native ranges at level 3
#' @param eoo_thresh (integer) threshold for EOO to determine Least Concern
#' @param aoo_thresh (integer) threshold for AOO to determine Least Concern
#' @param points_thresh (integer) threshold for number of points to determine Least Concern
#' @param regions_thresh (integer) threshold for number of regions to determine Least Concern
#'
#' @return Returns a dataframe with species level metrics
#' @export

# what about number of contemporary points? last 30 yrs?
# restricted elevation range?

make_metrics <- function(occs, native_ranges = FALSE,
                         eoo_thresh = 30000,
                         aoo_thresh = 3000,
                         points_thresh = 75,
                         regions_thresh = 5,
                         recent_thresh = 50) {

  # Step 1: Add 'recent' flag to occs for last 30 years
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  occs <- occs %>%
    dplyr::mutate(recent = year >= (current_year - 30))

  # Step 2: Get EOO, AOO, and number of points (NOP)
  resultsdf <-rCAT::batchCon(occs$speciesKey,
                               occs$decimalLongitude,
                               occs$decimalLatitude,
                               cellsize = 10000)

  resultsdf$taxon <- as.integer(resultsdf$taxon)

  # Step 3: Clean up
  rownames(resultsdf) <- NULL
  resultsdf <- resultsdf %>%
    dplyr::select(-proj_metadata, -EOOcat, -AOOcat)

  # Step 4: Add native range counts
  if (!is.null(native_ranges) && !identical(native_ranges, FALSE)) {
    range_count <- native_ranges %>%
      dplyr::group_by(internal_taxon_id) %>%
      dplyr::summarize(WGSRPD_count = dplyr::n(), .groups = "drop")

    resultsdf <- resultsdf %>%
      dplyr::left_join(range_count, by = c("taxon" = "internal_taxon_id"))
  }

  # Step 5: Add recent records count
  recent_counts <- occs %>%
    dplyr::group_by(speciesKey) %>%
    dplyr::summarize(recent_records = sum(recent, na.rm = TRUE), .groups = "drop")

  resultsdf <- resultsdf %>%
    dplyr::left_join(recent_counts, by = c("taxon" = "speciesKey"))

  # Step 6: Fill NAs with 0 for safe comparison
  resultsdf <- resultsdf %>%
    dplyr::mutate(WGSRPD_count = tidyr::replace_na(WGSRPD_count, 0),
                  recent_records = tidyr::replace_na(recent_records, 0))

  # Step 7: Apply Least Concern logic
  resultsdf <- resultsdf %>%
    dplyr::mutate(leastconcern = EOOkm2 >= eoo_thresh &
                    AOOkm >= aoo_thresh &
                    NOP >= points_thresh &
                    WGSRPD_count >= regions_thresh &
                    recent_records >= recent_thresh)

  print(tibble::as_tibble(resultsdf))

  return(resultsdf)

}
