
#' Generate the taxonomy.csv file
#'
#' @param unique_id (character) Unique identifier.
#' @param family (character) field containing family.
#' @param genus (character) field containing genus.
#' @param species (character) field containing species.
#' @param taxonomicAuthority (character) field containing taxonomic author.
#'
#' @return Returns an SIS compliant csv file
#' @export

sis_taxonomy = function(unique_id, family, genus, species, taxonomicAuthority){

  tax = tibble::tibble(
    internal_taxon_id = unique_id,
    family = family,
    genus = genus,
    species = species,
    taxonomicAuthority = taxonomicAuthority
    )

  tax = dplyr::inner_join(tax, iucn_taxonomy, by="family")

  tax$infratype = ""
  tax$infra_name = ""
  tax$infra_authority = ""

  return(tax)
}
