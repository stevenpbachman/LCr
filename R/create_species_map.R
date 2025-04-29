#' Display a leaflet map to show the distribution of occurrence data for each species
#'
#' @param data A data frame containing occurrence data
#' @param native_ranges A data frame containing occurrence data
#'
#' @return Interactive leaflet map
#' @export

# add the native range

create_species_map <- function(data, species_range = NULL) {
  # List of validation flags
  validation_flags <- c(
    "flag_cc_capitals", "flag_cc_centroids", "flag_cc_institutions",
    "flag_cc_equal", "flag_cc_gbif", "flag_cc_zeros",
    "flag_high_uncertainty", "flag_outside_native"
  )

  # Remove any rows with NA coordinates
  species_data <- data %>%
    dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude))

  # Add source URL if gbifID exists
  if ("gbifID" %in% colnames(species_data)) {
    species_data$source <- paste0("https://www.gbif.org/occurrence/", species_data$gbifID)
  } else {
    species_data$source <- "No GBIF ID available"
  }

  # Create valid data subset: all flags are FALSE or NA
  valid_data <- species_data %>%
    filter(if_all(all_of(validation_flags), ~ is.na(.) | . == FALSE))

  # Create problem data subset: at least one flag is TRUE
  problem_data <- species_data %>%
    filter(if_any(all_of(validation_flags), ~ . == TRUE))

  # Initialize base leaflet map
  m <- leaflet() %>%
    addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite")

  # Add measuring tool
  m <- m %>%
    addMeasure(
      position = "topleft",
      primaryLengthUnit = "kilometers",
      secondaryLengthUnit = "miles",
      primaryAreaUnit = "sqmeters",
      secondaryAreaUnit = "acres"
    )

  # Add native range polygon if provided
  if (!is.null(species_range) && nrow(species_range) > 0) {
    # Filter the tdwg_level3 polygons by the LEVEL3_COD values in species_range
    native_polygons <- LCr::tdwg_level3 %>%
      dplyr::filter(LEVEL3_COD %in% species_range$LEVEL3_COD)

    if (nrow(native_polygons) > 0) {
      m <- m %>%
        addPolygons(
          data = native_polygons,
          fillColor = "blue",
          weight = 1,
          opacity = 1,
          color = "black",
          dashArray = "3",
          fillOpacity = 0.3,
          group = "Native Range",
          popup = ~paste("Region:", LEVEL3_COD)
        )
    }
  }

  # Add valid points (green)
  if (nrow(valid_data) > 0) {
    m <- m %>%
      addCircleMarkers(
        data = valid_data,
        lng = ~decimalLongitude,
        lat = ~decimalLatitude,
        group = "Valid Points",
        popup = ~paste("Species:", species),
        radius = 6,
        stroke = TRUE,
        color = "white",
        weight = 2,
        opacity = 1,
        fill = TRUE,
        fillColor = "green",  # Apply colors based on category
        fillOpacity = 0.8
      )
  }

  # Add problem points for each flag
  for (i in seq_along(validation_flags)) {
    flag <- validation_flags[i]
    flag_data <- problem_data %>% filter(.data[[flag]] == TRUE)

    if (nrow(flag_data) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = flag_data,
          lng = ~decimalLongitude,
          lat = ~decimalLatitude,
          radius = 6,
          stroke = TRUE,
          color = "white",
          weight = 2,
          opacity = 1,
          fill = TRUE,
          fillColor = "red",  # Apply colors based on category
          fillOpacity = 0.8,
          group = flag,
          popup = ~paste("Species:", species, "<br>", "Issue:", flag, "<br>", "Source:", source)
        )
    }
  }

  # Update layer control to include Native Range
  overlay_groups <- c("Valid Points", validation_flags)
  if (!is.null(species_range) && nrow(species_range) > 0) {
    overlay_groups <- c("Native Range", overlay_groups)
  }

  m <- m %>%
    addLayersControl(
      baseGroups = c("Satellite", "OpenStreetMap"),
      overlayGroups = overlay_groups,
      options = layersControlOptions(collapsed = FALSE)
    )

  # Update legend to include Native Range
  legend_colors <- c("green", "red")
  legend_labels <- c("Valid Points", "Problematic Points")

  if (!is.null(species_range) && nrow(species_range) > 0) {
    legend_colors <- c("blue", legend_colors)
    legend_labels <- c("Native Range", legend_labels)
  }

  m <- m %>%
    addLegend(
      position = "bottomright",
      title = "Data Quality",
      colors = legend_colors,
      labels = legend_labels,
      opacity = 0.8
    )

  return(m)
}



