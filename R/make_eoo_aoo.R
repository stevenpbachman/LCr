
#' Generate the eoo and aoo values for the allfields.csv file
#'
#' @param occs
#'
#' @return Returns upper and lower elevation based on elevation field in GBIF occurrence file.

make_eoo_aoo <- function(occs) {

  # use rCAT to get the main results
  resultsdf <-rCAT::batchCon(taxa = occs$internal_taxon_id,
                             long = occs$dec_long,
                             lat = occs$dec_lat)

  resultsdf$taxon <- as.integer(resultsdf$taxon)
  resultsdf$internal_taxon_id <- resultsdf$taxon
  resultsdf <- resultsdf %>% dplyr::select(internal_taxon_id, EOOkm2,AOOkm) %>%
    dplyr::rename(AOO.range = AOOkm, EOO.range = EOOkm2)
  resultsdf$AOO.justification = paste0("The AOO is a minimum estimate and is not used in the assessment.")
  resultsdf$EOO.justification = paste0("The EOO was calculated from a convex hull around cleaned and georeferenced occurrence data using the rCAT package (Moat 2020).")

  return(resultsdf)
}

