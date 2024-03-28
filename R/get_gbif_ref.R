

#' Generate GBIF download citation according to IUCN format
#'
#' @param gbif_meta (character) GBIF download metadata
#'
#' @return Returns a data frame with IUCN SIS Connect reference
#' @export

occs_gbif_ref <- function(gbif_meta){
  tibble::tibble(
    Reference_type = "Assessment",
    type = "electronic source",
    author = "GBIF.org",
    year =  format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"),
    title = "GBIF Occurrence Download accessed from R via rgbif (https://github.com/ropensci/rgbif)",
    url = paste0("https://doi.org/", gbif_meta$doi),
    access_date = format(as.Date(Sys.Date(), format="%d/%m/%Y"))
  )
}
