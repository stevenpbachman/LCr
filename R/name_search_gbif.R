
#' Query taxon name against GBIF backbone
#'
#' @description Queries a taxon name against the GBIF names backbone. Simple wrapper around
#' the rgbif `name_backbone_verbose` function.
#'
#' @param name (character) A taxon name
#' @param species_rank (character) Default TRUE returns matches at species
#' level, otherwise FALSE returns any taxonomic rank
#' @param match (character) Controls the output of the search. Use `single` to
#' force a single match result that has the highest confidence or `any` to return
#' all possible matches sorted in order of confidence
#'
#' @return Returns a data frame with initial search term and matching name(s)
#' @export
#'
#' @examples
#' name_search_gbif("Poa annua L.")

name_search_gbif = function(name,
                            #species_rank = TRUE,
                            match = "single") {
                            #gbif_tax_stat = "any") { #removed as was not working well
  # set up default results table
  default_tbl = gbif_name_tbl_(name)

  # search using verbose to get fuzzy alternatives
  matches = rgbif::name_backbone_verbose(name = name,
                                         strict = FALSE,
                                         rank = 'species')

  # bind together in case there are missing data
  matches = dplyr::bind_rows(matches$alternatives, matches$data)

  # catch when search term is too vague
  no_match = all(matches$matchType == "NONE")
  all_higher = all(matches$matchType == "HIGHERRANK")

  if (no_match | all_higher) {
    results = default_tbl
  } else {
    # filter on species rank as default, but if not return all ranks
    #if (species_rank == TRUE) {
    #  results = dplyr::filter(matches, rank == "SPECIES")
    #} else {
      results = matches
    #}
}
    results$searchName = name # adding the search name back in to results table

    # check if family column exists (not for some fungi)
    if ("family" %in% colnames(results)) {
      # family column exists
      results = dplyr::select(results, colnames(default_tbl))
      results = dplyr::arrange(results, dplyr::desc(confidence))
    } else {
      # family column does not exist (for some fungi)
      default_tbl = default_tbl %>% dplyr::select(-family)
      results = dplyr::select(results, colnames(default_tbl))
      results$family <- "incertae sedis"
      results = dplyr::arrange(results, dplyr::desc(confidence))
    }

    # option to filter on maximum confidence from GBIF search - one option only "single"
    # accepted should be at top of list
    # or allow list of options "any"
    if (match == "single") {
      results = reorder_status(results)
      results = dplyr::slice_max(results, confidence, n = 1, with_ties = FALSE)
    } else {
      if (match == "any") {
        results = reorder_status(results)
      }
    }

    #Check if the 'scientificName' column contains NA values
    # add return if scientific name is not present
    if (!is.na(results$scientificName)) {

      gen_sp_auth <- rgbif::name_parse(results$scientificName)
      gen_sp_auth <- check_tax_auth(gen_sp_auth)

          if ("specificepithet" %in% colnames(gen_sp_auth)) {
            gen_sp_auth <- gen_sp_auth %>%
              dplyr::select(genusorabove, specificepithet, taxonomicAuthority)
          } else {
            gen_sp_auth$specificepithet <- ""
            gen_sp_auth <- gen_sp_auth %>%
              dplyr::select(genusorabove, specificepithet, taxonomicAuthority)
          }

      # Rename columns
      gen_sp_auth <- gen_sp_auth %>%
        dplyr::rename(genus = genusorabove,
                      species = specificepithet,
                      authorship = taxonomicAuthority)

      #results <- results %>% dplyr::select(-genus, -species, -taxonomicAuthority)
      results <- dplyr::bind_cols(results, gen_sp_auth)

    } else {
      results$genus <- ""
      results$species <- ""
      results$authorship <- ""
    }

    return(results)

  }

#' Generate the default table for GBIF name search results
#'
#' @importFrom tibble tibble
#'
#' @noRd
gbif_name_tbl_ = function(query) {
  tibble::tibble(
    searchName = query,
    usageKey = NA_integer_,
    scientificName = NA_character_,
    status =  NA_character_,
    rank = NA_character_,
    confidence = NA_integer_,
    family = NA_character_
    #genus = NA_character_,
    #species = NA_character_,
    #taxonomicAuthority = NA_character_
  )
}

