#' Get ISO two-digit code from occurrence lat longs
#'
#' @param sis_points (dataframe) SIS points file
#'
#' @return Occurrence dataframe with ISO two-digit codes added

get_iso_occs <- function(sis_points) {

  # get unique lat longs
  df <- sis_points %>% distinct(dec_lat, dec_long, .keep_all = TRUE)

  iso <- rgbif::gbif_geocode(latitude = df$dec_lat,
                             longitude = df$dec_long)

  #iso <- rgbif::gbif_geocode(latitude = df$dec_lat,longitude = df$dec_long)
  iso_ <- dplyr::filter(iso, distance == 0)
  iso_ <- dplyr::filter(iso_, type == "Political")



  iso_ <- dplyr::select(iso_, isoCountryCode2Digit, latitude, longitude)
  iso_ <- dplyr::rename(iso_, countrycode = isoCountryCode2Digit)

  return(iso)

}
