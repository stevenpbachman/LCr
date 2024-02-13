
#' Generate POWO download citation according to IUCN format
#'
#' This function uses queries POWO to get the native range
#' @param wcvp_ipni_id (character) WCVP identifier.
#' @returns Returns a dataframe with IUCN SIS Connect reference
#' @export

powo_ref <- function(wcvp_ipni_id){
  tibble::tibble(
    Reference_type = "Assessment",
    type = "electronic source",
    author = "POWO",
    year =  format(as.Date(Sys.Date(), format="%d/%m/%Y"),"%Y"),
    title = "Plants of the World Online. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet",
    url = "http://www.plantsoftheworldonline.org",
    access_date = format(as.Date(Sys.Date(), format="%d/%m/%Y"))
  )
}
