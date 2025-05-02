
#' Lookup distribution, habit and climate text in POWO (Plants of the World Online) using WCVP ID
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#' @param occs (df) Occurrence data with elevation information
#'
#' @return Returns a list with POWO data

powo_text <- function(wcvp_ipni_id, occs) {

  returned_data <- powo_lookup(wcvp_ipni_id)
  Sys.sleep(0.5)
  dist_text <- returned_data$taxonRemarks
  habit_text <- returned_data$lifeform
  clim_text <- returned_data$climate

  year_only <- format(Sys.Date(), format="%Y")

  # add elevation here if possible - from occs
  # If occs is provided, calculate and merge elevation data
  # Need a check to see if there are any or sufficient points with elevation data
  if (!is.null(occs)) {
    elevation_stats <- make_elevation(occs)
    elev_text <- paste0(" The estimated elevational range for this species is from ",
                        elevation_stats$ElevationLower.limit, " to ",
                        elevation_stats$ElevationUpper.limit, " m asl.")
  }

  # reformat text to make IUCN compatible
  dist_text <- paste0("The native range of this species is ",dist_text," (POWO, ",year_only, ").")
  if (!is.null(elev_text)) {
    dist_text <- paste0(dist_text,  elev_text)
  }
  iucn_dist_text <- replace_cardinal_directions(dist_text)

  iucn_habit_text <- paste0("It is a ",habit_text," and grows primarily in the ", clim_text," biome (POWO, ",year_only, ").")



  return(list(
    dist_text = dist_text,
    habit_text = habit_text,
    clim_text = clim_text,
    iucn_dist_text = iucn_dist_text,
    iucn_habit_text = iucn_habit_text
  ))
}
