#' Get ISO two-digit code from occurrence lat longs
#'
#' @param sis_points (dataframe) SIS points file
#'
#' @export
#' @return Occurrence dataframe with ISO two-digit codes added and internal id

get_occs_range <- function(sis_points) {

  sf::sf_use_s2(FALSE)

  # get unique lat longs
  sis_points_sf <- sf::st_as_sf(sis_points, coords = c("dec_long","dec_lat"))

  # prepare the tdwg polygon data
  sf::st_crs(tdwg_level3) <- sf::st_crs(tdwg_level3)
  sf::st_crs(sis_points_sf) <- sf::st_crs(tdwg_level3)

  # do the intersect
  out <- sf::st_join(sis_points_sf, tdwg_level3)

  # tidy up the table with just the required fields
  out <- out %>% dplyr::select("LEVEL3_COD", "sci_name", "internal_taxon_id") %>%
    sf::st_drop_geometry() %>% unique() %>% tidyr::drop_na() %>% dplyr::arrange(sci_name)

  return(out)

}
