
#' Lookup distribution, habit and climate text in POWO (Plants of the World Online) using WCVP ID
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#'
#' @return Returns a list with POWO data

powo_text <- function(wcvp_ipni_id, distribution=FALSE) {

  returned_data <- powo_lookup(wcvp_ipni_id)
  Sys.sleep(0.5)
  dist_text <- returned_data$taxonRemarks
  habit_text <- returned_data$lifeform
  clim_text <- returned_data$climate

  year_only <- format(Sys.Date(), format="%Y")

  # reformat text to make IUCN compatible
  dist_text <- paste0("The native range of this species is ",dist_text," (POWO, ",year_only, ").")
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
