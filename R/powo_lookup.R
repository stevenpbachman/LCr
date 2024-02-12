#' Lookup name in POWO (Plants of the World Online) using WCVP ID
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#' @param distribution (character) Default `FALSE`returns taxon information.
#' Use `TRUE` to return geographic range information
#'
#' @return Returns a list with POWO data
#' @export


powo_lookup <- function(wcvp_ipni_id, distribution=FALSE) {
  lookup_url <- paste("http://plantsoftheworldonline.org/api/2/taxon/urn:lsid:ipni.org:names:", wcvp_ipni_id, sep="")
  if (distribution) {
    response <- httr::GET(lookup_url, query=list(fields="distribution"))
  } else {
    response <- httr::GET(lookup_url)
  }

  if (! httr::http_error(response)) {
    return(jsonlite::fromJSON(httr::content(response, as="text")))
  }
  return(NULL)
}
