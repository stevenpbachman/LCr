
#' Generate the taxonomy.csv file
#'
#' @param unique_id (character) Unique identifier.
#'
#' @return Returns an SIS compliant csv file
#' @export

sis_taxonomy = function(unique_id, wcvp_ipni_id){

  name_info <- powo_lookup(wcvp_ipni_id)

  tax = tibble::tibble(
    internal_taxon_id = unique_id,
    family = name_info$family,
    genus = name_info$genus,
    species = name_info$species,
    taxonomicAuthority = name_info$authors)

  tax = dplyr::inner_join(tax, iucn_taxonomy, by="family")
  tax = tax[c(1, 7:10, 2:5)]

  tax$infratype = ""
  tax$infra_name = ""
  tax$infra_authority = ""

  tax <- tax %>%
    dplyr::rename("kingdom" = "kingdomName") %>%
    dplyr::rename("phylum" = "phylumName")

  return(tax)
}
