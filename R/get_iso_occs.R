#' Get ISO two-digit code from occurrence lat longs
#'
#' @param sis_points (dataframe) SIS points file
#'
#' @return Occurrence dataframe with ISO two-digit codes added

get_iso_occs <- function(sis_points) {
  iso <- rgbif::gbif_geocode(latitude = sis_points$dec_lat,
                             longitude = sis_points$dec_long)
  iso <- dplyr::filter(iso, distance == 0)
  iso <- dplyr::filter(iso, type == "Political")
  iso <- dplyr::select(iso, isoCountryCode2Digit)
  iso <- dplyr::rename(iso, countrycode = isoCountryCode2Digit)

  return(iso)

}
