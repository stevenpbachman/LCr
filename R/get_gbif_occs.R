#' Download georeferenced occurrence data using GBIF name identifier
#'
#' @param gbif_ids (character) GBIF identifiers
#' @param keys_df (character) WCVP identifiers.
#'
#' @return (list) 'Points' = the downloaded GBIF points, 'Citation' = the GBIF citation to be used in references
#' @export

get_gbif_occs <- function(gbif_ids, keys_df = NULL) {
  gbif_download <- rgbif::occ_download(
    rgbif::pred_in("taxonKey", gbif_ids),    # important to use pred_in
    rgbif::pred("hasGeospatialIssue", FALSE),
    rgbif::pred("hasCoordinate", TRUE),    # filter on those with coordinates
    format = "DWCA"
  )

  rgbif::occ_download_wait(gbif_download) # checks if download is finished

  temporary_folder <- tempdir()

  downloaded <- rgbif::occ_download_get(gbif_download, path = temporary_folder) # download
  the_points <- rgbif::occ_download_import(downloaded, path = temporary_folder) # and import

  if (!is.null(keys_df)) {
    the_points <-
      dplyr::left_join(the_points,
                       keys_df,
                       by = c("acceptedTaxonKey" = "GBIF_usageKey"))
  }

  meta <- rgbif::occ_download_meta(gbif_download) # get meta data to get the citation data
  cite <- occs_gbif_ref(meta) # custom script to convert reference into SIS format

  res_list <- list("citation" = cite, "points" = the_points)

  return(res_list)

}
