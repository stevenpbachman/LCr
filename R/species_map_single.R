#' Display a leaflet map to show the distribution of occurrence data for each species
#'
#' @param data A data frame containing occurrence data
#' @param native_ranges A data frame containing occurrence data show_flags
#' @param show_flags Set to false if you just want to see the points map without flags

#' @return Interactive leaflet map
#' @export

species_map_single <- function(data, species_range = NULL, show_flags = TRUE) {

  species_name <- data$canonicalname[1]

  title_html <- sprintf(
    "<div style='
    position: absolute;
    top: 10px;
    left: 50%%;
    transform: translateX(-50%%);
    font-size: 18px;
    font-weight: bold;
    background-color: white;
    padding: 5px;
    z-index: 1000;
  '>
    %s
  </div>",
    species_name
  )

  # Remove any rows with NA coordinates
  species_data <- data %>%
    dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude))

  # Function to detect overlapping points and create enhanced popups
  create_overlap_data <- function(data) {
    # Round coordinates to identify overlaps (adjust precision as needed)
    # Using more decimal places for finer overlap detection
    data$coord_key <- paste(round(data$decimalLatitude, 6), round(data$decimalLongitude, 6), sep = "_")

    # Count occurrences at each coordinate
    overlap_summary <- data %>%
      dplyr::group_by(coord_key, decimalLatitude, decimalLongitude) %>%
      dplyr::summarise(
        count = dplyr::n(),
        gbif_ids = paste(gbifID, collapse = ", "),
        species_list = paste(unique(species), collapse = ", "),
        .groups = 'drop'
      )

    # Join back to original data
    enhanced_data <- data %>%
      dplyr::left_join(overlap_summary, by = c("coord_key", "decimalLatitude", "decimalLongitude"))

    return(enhanced_data)
  }

  # Create enhanced popup text - this will be applied row-wise
  species_data$popup_text <- ""

  # Add switch if user wants to just see map or to see flags
  if (show_flags == TRUE) {

    # List of validation flags
    validation_flags <- c(
      "flag_cc_capitals", "flag_cc_centroids", "flag_cc_institutions",
      "flag_cc_equal", "flag_cc_gbif", "flag_cc_zeros",
      "flag_high_uncertainty", "flag_outside_native"
    )

    # Add source URL if gbifID exists
    if ("gbifID" %in% colnames(species_data)) {
      species_data$source <- paste0("https://www.gbif.org/occurrence/", species_data$gbifID)
    } else {
      species_data$source <- "No GBIF ID available"
    }

    # Create valid data subset: all flags are FALSE or NA
    valid_data <- species_data %>%
      dplyr::filter(dplyr::if_all(dplyr::all_of(validation_flags), ~ is.na(.) | . == FALSE))

    # Create problem data subset: at least one flag is TRUE
    problem_data <- species_data %>%
      dplyr::filter(dplyr::if_any(dplyr::all_of(validation_flags), ~ . == TRUE))

    # Process overlap data for both valid and problem data
    if (nrow(valid_data) > 0) {
      valid_data <- create_overlap_data(valid_data)
      # Create popup text
      valid_data$popup_text <- paste(
        paste("Species:", valid_data$species_list),
        ifelse(valid_data$count > 1,
               paste("<b>Multiple points at this location (", valid_data$count, ")</b>"),
               ""),
        ifelse(valid_data$count > 1,
               paste("GBIF IDs:", valid_data$gbif_ids),
               paste("GBIF ID:", valid_data$gbif_ids)),
        paste("Source:", valid_data$source),
        sep = "<br>"
      )
    }
    if (nrow(problem_data) > 0) {
      problem_data <- create_overlap_data(problem_data)
      # Create popup text for problem data
      problem_data$popup_text <- paste(
        paste("Species:", problem_data$species_list),
        ifelse(problem_data$count > 1,
               paste("<b>Multiple points at this location (", problem_data$count, ")</b>"),
               ""),
        ifelse(problem_data$count > 1,
               paste("GBIF IDs:", problem_data$gbif_ids),
               paste("GBIF ID:", problem_data$gbif_ids)),
        paste("Source:", problem_data$source),
        sep = "<br>"
      )
    }

    # Initialize base leaflet map
    m <- leaflet() %>%
      leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite")

    # Add measuring tool
    m <- m %>%
      leaflet::addMeasure(
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
          leaflet::addPolygons(
            data = native_polygons,
            fillColor = "blue",
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "3",
            fillOpacity = 0.3,
            group = "Native Range",
            popup = ~paste("Region:", LEVEL3_COD),
            options = pathOptions(pane = "overlayPane")
          )
      }
    }

    # Add valid points (green) - deduplicated by coordinate
    if (nrow(valid_data) > 0) {
      # Get unique coordinates for display
      valid_unique <- valid_data %>%
        dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

      m <- m %>%
        leaflet::addCircleMarkers(
          data = valid_unique,
          lng = ~decimalLongitude,
          lat = ~decimalLatitude,
          group = "Valid Points",
          popup = ~popup_text,
          radius = ~pmax(6, pmin(8, 6 + (count > 1) * 2)),  # Slightly larger for overlapping points
          stroke = TRUE,
          color = ~ifelse(count > 1, "yellow", "white"),  # Yellow border for overlaps
          weight = 2,
          opacity = 1,
          fill = TRUE,
          fillColor = "green",
          fillOpacity = 0.8,
          options = pathOptions(pane = "markerPane")
        )
    }

    # Add problem points for each flag
    for (i in seq_along(validation_flags)) {
      flag <- validation_flags[i]
      flag_data <- problem_data %>% filter(.data[[flag]] == TRUE)

      if (nrow(flag_data) > 0) {
        # Get unique coordinates for this flag and add flag info to popup
        flag_unique <- flag_data %>%
          dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE) %>%
          dplyr::mutate(
            popup_text_with_flag = paste(
              paste("Species:", species_list),
              ifelse(count > 1,
                     paste("<b>Multiple points at this location (", count, ")</b>"),
                     ""),
              ifelse(count > 1,
                     paste("GBIF IDs:", gbif_ids),
                     paste("GBIF ID:", gbif_ids)),
              paste("Issue:", flag),
              paste("Source:", source),
              sep = "<br>"
            )
          )

        m <- m %>%
          leaflet::addCircleMarkers(
            data = flag_unique,
            lng = ~decimalLongitude,
            lat = ~decimalLatitude,
            radius = ~pmax(6, pmin(8, 6 + (count > 1) * 2)),
            stroke = TRUE,
            color = ~ifelse(count > 1, "yellow", "white"),
            weight = 2,
            opacity = 1,
            fill = TRUE,
            fillColor = "red",
            fillOpacity = 0.8,
            group = flag,
            popup = ~popup_text_with_flag,
            options = pathOptions(pane = "markerPane")
          )
      }
    }

    # Update layer control to include Native Range
    overlay_groups <- c("Valid Points", validation_flags)
    if (!is.null(species_range) && nrow(species_range) > 0) {
      overlay_groups <- c("Native Range", overlay_groups)
    }

    m <- m %>%
      leaflet::addLayersControl(
        baseGroups = c("Satellite", "OpenStreetMap"),
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )

    # Update legend to include Native Range and overlap indication
    legend_colors <- c("green", "red", "yellow")
    legend_labels <- c("Valid Points", "Problematic Points", "Multiple Points (border)")

    if (!is.null(species_range) && nrow(species_range) > 0) {
      legend_colors <- c("blue", legend_colors)
      legend_labels <- c("Native Range", legend_labels)
    }

    m <- m %>%
      leaflet::addLegend(
        position = "bottomright",
        title = "Data Quality",
        colors = legend_colors,
        labels = legend_labels,
        opacity = 0.8
      )

    m <- htmlwidgets::prependContent(m, htmltools::HTML(title_html))

    return(m)

  } else {

    # simple map
    # Add source URL if gbifID exists
    if ("gbifID" %in% colnames(species_data)) {
      species_data$source <- paste0("https://www.gbif.org/occurrence/", species_data$gbifID)
    } else {
      species_data$source <- "No GBIF ID available"
    }

    # Process overlap data
    species_data <- create_overlap_data(species_data)

    # Create popup text
    species_data$popup_text <- paste(
      paste("Species:", species_data$species_list),
      ifelse(species_data$count > 1,
             paste("<b>Multiple points at this location (", species_data$count, ")</b>"),
             ""),
      ifelse(species_data$count > 1,
             paste("GBIF IDs:", species_data$gbif_ids),
             paste("GBIF ID:", species_data$gbif_ids)),
      paste("Source:", species_data$source),
      sep = "<br>"
    )

    # Initialize base leaflet map
    m <- leaflet() %>%
      leaflet::addProviderTiles("OpenStreetMap", group = "OpenStreetMap") %>%
      leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite")

    # Add measuring tool
    m <- m %>%
      leaflet::addMeasure(
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
          leaflet::addPolygons(
            data = native_polygons,
            fillColor = "blue",
            weight = 1,
            opacity = 1,
            color = "black",
            dashArray = "3",
            fillOpacity = 0.3,
            group = "Native Range",
            popup = ~paste("Region:", LEVEL3_COD),
            options = pathOptions(pane = "overlayPane")
          )
      }
    }

    # Add points - deduplicated by coordinate
    if (nrow(species_data) > 0) {
      # Get unique coordinates for display
      species_unique <- species_data %>%
        dplyr::distinct(decimalLatitude, decimalLongitude, .keep_all = TRUE)

      m <- m %>%
        leaflet::addCircleMarkers(
          data = species_unique,
          lng = ~decimalLongitude,
          lat = ~decimalLatitude,
          group = "Valid Points",
          popup = ~popup_text,
          radius = ~pmax(6, pmin(8, 6 + (count > 1) * 2)),  # Slightly larger for overlapping points
          stroke = TRUE,
          color = ~ifelse(count > 1, "yellow", "white"),  # Yellow border for overlaps
          weight = 2,
          opacity = 1,
          fill = TRUE,
          fillColor = "green",
          fillOpacity = 0.8,
          options = pathOptions(pane = "markerPane")
        )
    }

    # Update layer control to include Native Range
    overlay_groups <- "Valid Points"
    if (!is.null(species_range) && nrow(species_range) > 0) {
      overlay_groups <- c("Native Range", "Valid Points")
    }

    m <- m %>%
      leaflet::addLayersControl(
        baseGroups = c("Satellite", "OpenStreetMap"),
        overlayGroups = overlay_groups,
        options = layersControlOptions(collapsed = FALSE)
      )

    # Add legend
    legend_colors <- c("green", "yellow")
    legend_labels <- c("Single Points", "Multiple Points (border)")

    if (!is.null(species_range) && nrow(species_range) > 0) {
      legend_colors <- c("blue", legend_colors)
      legend_labels <- c("Native Range", legend_labels)
    }

    m <- m %>%
      leaflet::addLegend(
        position = "bottomright",
        title = "Data Quality",
        colors = legend_colors,
        labels = legend_labels,
        opacity = 0.8
      )

    m <- htmlwidgets::prependContent(m, htmltools::HTML(title_html))

  }
  return(m)
}
