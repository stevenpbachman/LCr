
# Utility/helper functions


#' Resolves taxonomicAuthor field when there are varying taxonomic fields returned from GBIF query
#'
#' @param df dataframe with gbif name search output
#'
#' @return returns GBIF taxonomic information with reformatted taxonomicAuthor
#' @keywords internal
#' @noRd
#' @encoding UTF-8
#' @importFrom stats family na.omit quantile setNames

check_tax_auth <- function(df) {
  # Check if 'bracketauthorship' and 'authorship' columns exist
  has_bracket <- "bracketauthorship" %in% colnames(df)
  has_author <- "authorship" %in% colnames(df)

  if (has_bracket & has_author) {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = paste0("(", bracketauthorship, ") ", authorship))
  } else if (has_author) {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = authorship)
  } else {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = "")
  }

  return(df)
}

#' Generate GBIF download citation according to IUCN format
#'
#' @param gbif_meta (character) GBIF download metadata
#'
#' @return Returns a data frame with IUCN SIS Connect reference
#' @keywords internal
#' @noRd

get_gbif_ref <- function(gbif_meta) {
  tibble::tibble(
    Reference_type = "Assessment",
    type = "electronic source",
    author = "GBIF.org",
    year =  format(as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"),
    title = "GBIF Occurrence Download accessed from R via rgbif (https://github.com/ropensci/rgbif)",
    url = paste0("https://doi.org/", gbif_meta$doi),
    access_date = format(as.Date(Sys.Date(), format = "%d/%m/%Y"))
  )
}

#' Generate a list of biorealms for a species
#'
#' @param native_ranges (dataframe) native ranges
#'
#' @return concatenated list of biorealms in SIS connect format
#' @keywords internal
#' @noRd

make_biorealms = function(native_ranges) {
  biogeographic_realm <- native_ranges %>%
    dplyr::left_join(TDWG_realms, by = c("LEVEL3_COD" = "LEVEL3_COD"), relationship = "many-to-many") %>%  # Match regions to realms
    dplyr::group_by(internal_taxon_id) %>%
    dplyr::summarise(BiogeographicRealm.realm = gsub(", ", " | ", paste(unique(REALM), collapse = ", ")),
              .groups = "drop")

  return(biogeographic_realm)
}

#' Generate the elevation upper and lower bounds for the allfields.csv file
#'
#' @param occs
#'
#' @return Returns upper and lower elevation based on elevation field in GBIF occurrence file.
#' @keywords internal
#' @noRd

make_elevation <- function(occs) {
  elevation_stats <- occs %>%
    dplyr::group_by(internal_taxon_id) %>%
    dplyr::summarise(
      ElevationLower.limit = round(quantile(min_elev, 0.05, na.rm = TRUE)),
      ElevationUpper.limit = round(quantile(min_elev, 0.95, na.rm = TRUE)),
      #n = n(),
      .groups = "drop"
    )

  return(elevation_stats)
}

#' Lookup name in POWO (Plants of the World Online) using WCVP ID
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#' @param distribution (character) Default `FALSE`returns taxon information.
#' Use `TRUE` to return geographic range information
#'
#' @return Returns a list with POWO data
#' @keywords internal
#' @noRd

powo_lookup <- function(wcvp_ipni_id, distribution = FALSE) {
  lookup_url <- paste(
    "http://plantsoftheworldonline.org/api/2/taxon/urn:lsid:ipni.org:names:",
    wcvp_ipni_id,
    sep = ""
  )
  if (distribution) {
    response <- httr::GET(lookup_url, query = list(fields = "distribution"))
  } else {
    response <- httr::GET(lookup_url)
  }

  if (!httr::http_error(response)) {
    return(jsonlite::fromJSON(httr::content(response, as = "text")))
  }
  return(NULL)
}

#' Return native range of an accepted plant species from POWO (Plants of the
#' World Online) using WCVP identifier
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#'
#' @return (dataframe) Native range codes according to World Geographic Scheme for Recordings Plant Distributions (WGSRPD)
#' @keywords internal
#' @noRd

powo_range = function(wcvp_ipni_id) {
  results = tibble::tibble(
    LEVEL3_COD = NA_character_,
    featureId = NA_character_,
    tdwgLevel = NA_integer_,
    establishment = NA_character_,
    LEVEL3_NAM = NA_character_,
    POWO_ID = NA_character_
  )

  returned_data <- powo_lookup(wcvp_ipni_id, distribution = TRUE)
  Sys.sleep(0.5)
  distribution <- returned_data$distribution$natives

  if (!is.null(distribution)) {
    results = dplyr::rename(distribution,
                            LEVEL3_NAM = name,
                            LEVEL3_COD = tdwgCode)
    results = dplyr::mutate(results, LEVEL3_NAM = dplyr::recode(LEVEL3_NAM, "u00e1" =
                                                                  "a")) # replaced "á" with unicode. need to check it still works
  }

  return(results)
}

#' Generate POWO download citation according to IUCN format
#'
#' This function uses queries POWO to get the native range
#' @returns Returns a dataframe with IUCN SIS Connect reference
#' @keywords internal
#' @noRd

powo_ref <- function() {
  tibble::tibble(
    Reference_type = "Assessment",
    type = "electronic source",
    author = "POWO",
    year =  format(as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"),
    title = "Plants of the World Online. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet",
    url = "http://www.plantsoftheworldonline.org",
    access_date = format(as.Date(Sys.Date(), format = "%d/%m/%Y"))
  )
}

#' Lookup distribution, habit and climate text in POWO (Plants of the World Online) using WCVP ID
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#' @param occs (df) Occurrence data with elevation information
#'
#' @return Returns a list with POWO data
#' @keywords internal
#' @noRd

powo_text <- function(wcvp_ipni_id, occs, unique_id) {
  returned_data <- powo_lookup(wcvp_ipni_id)
  Sys.sleep(0.5)
  dist_text <- returned_data$taxonRemarks
  habit_text <- returned_data$lifeform
  clim_text <- returned_data$climate

  year_only <- format(Sys.Date(), format = "%Y")

  # add elevation here if possible - from occs
  # If occs is provided, calculate and merge elevation data
  # Need a check to see if there are any or sufficient points with elevation data
  if (!is.null(occs)) {
    occs <- occs %>% dplyr::filter(internal_taxon_id == unique_id)
    elevation_stats <- make_elevation(occs)
    elev_text <- paste0(
      " The estimated elevational range for this species is from ",
      format(
        elevation_stats$ElevationLower.limit,
        big.mark = ",",
        scientific = FALSE
      ),
      " to ",
      format(
        elevation_stats$ElevationUpper.limit,
        big.mark = ",",
        scientific = FALSE
      ),
      " m asl."
    )
  }

  # reformat text to make IUCN compatible
  dist_text <- paste0("The native range of this species is ",
                      dist_text,
                      " (POWO, ",
                      year_only,
                      ").")
  if (exists("elev_text") && !is.null(elev_text)) {
    dist_text <- paste0(dist_text, elev_text)
  }
  iucn_dist_text <- replace_cardinal_directions(dist_text)

  iucn_habit_text <- paste0(
    "It is a ",
    habit_text,
    " and grows primarily in the ",
    clim_text,
    " biome (POWO, ",
    year_only,
    ")."
  )

  return(
    list(
      dist_text = dist_text,
      habit_text = habit_text,
      clim_text = clim_text,
      iucn_dist_text = iucn_dist_text,
      iucn_habit_text = iucn_habit_text
    )
  )
}

#' Replace text from POWO so that it matches IUCN standards
#'
#' @param text (character) POWO distribution string
#'
#' @return IUCN standardised POWO range string
#' @keywords internal
#' @noRd

replace_cardinal_directions <- function(text) {

  # Replace ampersands with 'and'
  text <- gsub(" *& *", " and ", text)

  lowercase_terms = c("subalpine", "subarctic", "temperate","mountains", "tropical", "subtropical", "north", "south", "east", "west", "central",
                      "northeast", "southeast", "northwest", "southwest",
                      "north central", "south central", "east central", "west central",
                      "northeast central", "southeast central", "northwest central", "southwest central")

  replacements <- list(
    "\\bN\\.E\\. C\\." = "northeast central",
    "\\bS\\.E\\. C\\." = "southeast central",
    "\\bS\\.W\\. C\\." = "southwest central",
    "\\bN\\.W\\. C\\." = "northwest central",
    "\\bN\\. C\\." = "north central",
    "\\bS\\. C\\." = "south central",
    "\\bE\\. C\\." = "east central",
    "\\bW\\. C\\." = "west central",

    "\\bN\\.E\\." = "northeast",
    "\\bS\\.E\\." = "southeast",
    "\\bS\\.W\\." = "southwest",
    "\\bN\\.W\\." = "northwest",
    "\\bNE\\." = "northeast",
    "\\bSE\\." = "southeast",
    "\\bSW\\." = "southwest",
    "\\bNW\\." = "northwest",

    "\\bN\\." = "north",
    "\\bS\\." = "south",
    "\\bE\\." = "east",
    "\\bW\\." = "west",
    "\\bC\\." = "central",

    "\\bTemp\\." = "temperate",
    "\\bTrop\\." = "tropical",
    "\\bSubtrop\\." = "subtropical"
  )

  # Apply replacements, in order of decreasing pattern length
  sorted_patterns <- names(replacements)[order(nchar(names(replacements)), decreasing = TRUE)]

  for (pattern in sorted_patterns) {
    text <- gsub(
      pattern,
      replacements[[pattern]],
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  }

  # Lowercase known full-form terms, only when they appear as whole words
  for (term in lowercase_terms) {
    pattern <- paste0("\\b", tools::toTitleCase(term), "\\b")
    text <- gsub(pattern, term, text)
  }

  return(text)
}

#' Reorder taxonomic status to give priority to accepted name
#'
#' @description arranges order of taxonomic status from GBIF query
#'
#' @param df (character) data frame with names
#'
#' @return Returns a data frame with taxonomic status sorted
#' @keywords internal
#' @noRd

reorder_status <- function(df) {

  status_levels <- c("ACCEPTED", "SYNONYM", "DOUBTFUL", "OTHER")

  df <- df %>%
    dplyr::mutate(status = factor(status, levels = status_levels, ordered = TRUE)) %>%
    dplyr::arrange(status)

  return(df)
}


#' Generate the eoo and aoo values for the allfields.csv file
#'
#' @param occs
#'
#' @return Returns upper and lower elevation based on elevation field in GBIF occurrence file.
#' @keywords internal
#' @noRd

make_eoo_aoo <- function(occs) {

  # use rCAT to get the main results
  resultsdf <-rCAT::batchCon(taxa = occs$internal_taxon_id,
                             long = occs$dec_long,
                             lat = occs$dec_lat)

  resultsdf$taxon <- as.integer(resultsdf$taxon)
  resultsdf$internal_taxon_id <- resultsdf$taxon
  resultsdf <- resultsdf %>% dplyr::select(internal_taxon_id, EOOkm2,AOOkm) %>%
    dplyr::rename(AOO.range = AOOkm, EOO.range = EOOkm2)
  resultsdf$AOO.justification = paste0("The AOO is a minimum estimate and is not used in the assessment.")
  resultsdf$EOO.justification = paste0("The EOO was calculated from a convex hull around cleaned and georeferenced occurrence data using the rCAT package (Moat 2020).")

  return(resultsdf)
}

#' Generate global variables
#'
#' @return Generate global variables
#' @keywords internal
#' @noRd

globalVariables(c(
  # check_occs.R
  "flag_no_coords",
  "LEVEL3_COD",
  # utils.R
  "bracketauthorship",
  "authorship",
  # clean_keys.R
  "wcvp_status",
  "wcvp_rank",
  "GBIF_status",
  "GBIF_rank",
  "searchName",
  "n",
  # clean_occs.R
  "flag_cc_capitals",
  "flag_cc_centroids",
  "flag_cc_institutions",
  "flag_cc_equal",
  "flag_cc_gbif",
  "flag_cc_zeros",
  "flag_cc_urb",
  "flag_high_uncertainty",
  "flag_outside_native",
  "flag_manual_gbifid",
  "manual_removal_reason",
  "flag_manual_keep",
  "is_problematic",
  "GBIF_usageKey",
  # get_name_keys.R
  "GBIF_usageKey",
  #get_occs_range.R
  "sci_name",
  #make_biorealms
  "TDWG_realms",
  "internal_taxon_id",
  "REALM",
  #make_elevation
  "internal_taxon_id",
  "min_elev",
  #make_eoo_aoo
  "internal_taxon_id",
  "EOOkm2",
  "AOOkm",
  #make_metrics
  "year",
  "proj_metadata",
  "EOOcat",
  "AOOcat",
  "internal_taxon_id",
  "speciesKey",
  "recent",
  "WGSRPD_count",
  "recent_records",
  "EOOkm2",
  "AOOkm",
  "NOP",
  "lc_eoo",
  "lc_aoo",
  "lc_nop",
  "lc_regions",
  "lc_recent",
  "lc_core",
  "lc_supporting",
  "taxon",
  # make_sis_occs
  "species",
  "recordedBy",
  "recordNumber",
  "day",
  "month",
  "year",
  "decimalLatitude",
  "decimalLongitude",
  "locality",
  "coordinateUncertaintyInMeters",
  "habitat",
  "basisOfRecord",
  "elevation",
  "catalogNumber",
  "speciesKey",
  "gbifID",
  "basisofrec",
  "dec_lat",
  "dec_long",
  # make_zip
  "sis_files",
  # map_species_batch
  "speciesKey",
  "internal_taxon_id",
  #map_species_single
  "decimalLatitude",
  "decimalLongitude",
  "drawRectangleOptions",
  "drawShapeOptions",
  "drawPolygonOptions",
  "editToolbarOptions",
  "selectedPathOptions",
  "pathOptions",
  ".data",
  "species_list",
  "count",
  "gbif_ids",
  "layersControlOptions",
  "coord_key",
  "decimalLatitude",
  "decimalLongitude",
  "gbifID",
  "species",
  #powo_range
  "name",
  "tdwgCode",
  "LEVEL3_NAM",
  #powo_text
  "internal_taxon_id",
  #reorder_status
  "status",
  #search_name_gbif
  "confidence",
  "genusorabove",
  "specificepithet",
  "taxonomicAuthority",
  #sis_assessments
  "BiogeographicRealm.realm.y",
  "BiogeographicRealm.realm.x",
  #sis_countries
  "internal_taxon_id",
  "tdwg2iucn",
  "countryoccurrence.countryoccurrencesubfield.countryoccurrencename",
  "CountryOccurrence.CountryOccurrenceSubfield.presence",
  "CountryOccurrence.CountryOccurrenceSubfield.origin",
  "CountryOccurrence.CountryOccurrenceSubfield.seasonality",
  "countryoccurrence.countryoccurrencesubfield.countryoccurrencelookup",
  #sis_plantspecific
  "PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName",
  "PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup",
  #sis_taxonomy
  "iucn_taxonomy"
))
