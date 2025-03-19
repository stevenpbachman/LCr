
#' Generate a list of biorealms for a species
#'
#' @param TDWG_realms (dataframe) TDWG level 3 to realm converter
#'
#' @return concatenated list of biorealms in SIS connect format
#'

make_biorealms = function(native_ranges) {

    biogeographic_realm <- native_ranges %>%
      left_join(TDWG_realms, by = c("LEVEL3_COD" = "LEVEL3_COD")) %>%  # Match regions to realms
      group_by(internal_taxon_id) %>%
      summarise(BiogeographicRealm.realm = gsub(", ", " | ", paste(unique(REALM), collapse = ", ")),
                .groups = "drop")

    return(biogeographic_realm)
  }
