
#' Generate metrics from a clean point occurrence file
#'
#' @param occs_clean (character) Cleaned GBIF occurrence file in DWCA format. See
#' occs_clean() function.
#'
#' @return Returns a dataframe with species level metrics
#' @export

# need to group by unique ID and derive metrics
# get area of TDWG as well
# number of ecoregions?
# what about number of contemporary points? last 30 yrs?
# should this be raw or cleaned points? cleaned is easier.
# also number of TDWG - maybe we need to get all relevant items from POWO
# restricted elevation range?

make_metrics <- function(occs_clean){

  # this will get EOO, AOO and number of points NOP
  resultsdf <- rCAT::batchCon(occs_clean$species,
                              occs_clean$decimalLongitude,
                              occs_clean$decimalLatitude)


}




