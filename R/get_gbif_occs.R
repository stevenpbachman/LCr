#' Download georeferenced occurrence data using GBIF name identifier
#'
#' @param keys_df (data frame) Derived from [`get_name_keys()`] function. Must include at least GBIF_usageKey to obtain GBIF occurrences
#'
#' @return (list) 'Points' = the downloaded GBIF points, 'Citation' = the GBIF citation to be used in references
#' @export
#' @details Initiates an occurrence download in DWCA format using [rgbif::occ_download()]. If `keys_df`contains multiple GBIF_usageKey values, they will be
#' combined in a single output file.


get_gbif_occs <- function(keys_df) {
  gbif_download <- rgbif::occ_download(
    rgbif::pred_in("taxonKey", keys_df$GBIF_usageKey),    # important to use pred_in
    rgbif::pred("occurrenceStatus", "present"),
    #rgbif::pred("hasGeospatialIssue", FALSE),
    #rgbif::pred("hasCoordinate", TRUE),    # filter on those with coordinates
    format = "DWCA"
  )

  rgbif::occ_download_wait(gbif_download) # checks if download is finished

  temporary_folder <- tempdir()

  downloaded <- rgbif::occ_download_get(gbif_download, path = temporary_folder) # download
  the_points <- rgbif::occ_download_import(downloaded, path = temporary_folder) # and import

  the_points <- dplyr::left_join(the_points,
                       keys_df,
                       by = c("speciesKey" = "GBIF_usageKey"))


  meta <- rgbif::occ_download_meta(gbif_download) # get meta data to get the citation data
  cite <- get_gbif_ref(meta) # custom script to convert reference into SIS format

  res_list <- list("citation" = cite, "points" = the_points)

  return(res_list)

}
