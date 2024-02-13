
#' Generate the allfields.csv file
#'
#' @param wcvp_ipni_id (character) WCVP identifier.
#'
#' @return Returns an SIS compliant data frame
#' @export

sis_allfields = function(wcvp_ipni_id){

  tibble::tibble(
    internal_taxon_id = wcvp_ipni_id,
    CurrentTrendDataDerivation.value = "Suspected",
    AOO.range = "",
    AOO.justification = "",
    EOO.range = "",
    EOO.justification = "",
    SevereFragmentation.isFragmented = "0",
    NoThreats.noThreats = "TRUE",
    ThreatsUnknown.value = "FALSE",
    GenerationLength.range = "")
}
