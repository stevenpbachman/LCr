#' Display a leaflet map to show the distribution of occurrence data for each species
#' Batch version that iterates through species
#'
#' @param data A data frame containing occurrence data after cleaning flags added
#' @param native_ranges A data frame containing occurrence data show_flags
#' @param show_flags Set to false if you just want to see the points map without flags
#' @param save_map Set to true if you want to save the maps as html files in the working directory

#' @return Interactive leaflet map
#' @export

map_species_batch <- function(data, species_range = NULL, show_flags = TRUE, save_map = TRUE) {

  # Get unique species keys
  species_keys <- unique(data$speciesKey)

  # Internal helper function to generate and optionally save a map
  make_map <- function(spkey) {
    # Filter species data
    sp_filtered <- data %>% dplyr::filter(speciesKey == spkey)
    sp_range_filtered <- species_range %>% dplyr::filter(internal_taxon_id == spkey)

    # Extract canonical name (assumes one per speciesKey)
    species_name <- unique(sp_filtered$canonicalname)

    # Create map
    map <- map_species_single(sp_filtered, species_range = sp_range_filtered, show_flags = show_flags)

    # Optionally save
    if (save_map && length(species_name) == 1 && !is.na(species_name)) {
      filename <- glue::glue("{gsub(' ', '_', species_name)}.html")
      htmlwidgets::saveWidget(map, file = filename, selfcontained = TRUE)
    }

    return(map)
  }

  # Batch apply to all species keys
  maps <- map(species_keys, make_map)

  return(maps)
}
