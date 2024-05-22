#' Get ISO two-digit code from occurrence lat longs
#'
#' @param sis_points (dataframe) SIS points file
#'
#' @return Occurrence dataframe with ISO two-digit codes added

get_occs_range <- function(sis_points) {

  sf_use_s2(FALSE)

  # get unique lat longs
  sis_points_sf <- sf::st_as_sf(sis_points, coords = c("dec_long","dec_lat"))

  # prepare the tdwg polygon data
  st_crs(LCr::tdwg_level3) <- st_crs(tdwg_level3)
  st_crs(sis_points_sf) <- st_crs(tdwg_level3)

  # do the intersect
  out <- sf::st_join(sis_points_sf, tdwg_level3)

  return(out)

}
