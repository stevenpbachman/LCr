
#' Replace text from POWO so that it matches IUCN standards
#'
#' @param text (character) POWO distribtion string
#'
#' @return IUCN standardised POWO range string


replace_cardinal_directions <- function(text) {
  # Create a mapping of abbreviations to full forms (lowercase)
  replacements <- list(
    "N\\.E\\." = "northeast",
    "S\\.E\\." = "southeast",
    "S\\.W\\." = "southwest",
    "N\\.W\\." = "northwest",
    "NE\\." = "northeast",
    "SE\\." = "southeast",
    "SW\\." = "southwest",
    "NW\\." = "northwest",
    "N\\." = "north",
    "S\\." = "south",
    "E\\." = "east",
    "W\\." = "west",
    "C\\." = "central",
    "Trop\\." = "tropical",
    "Subtrop\\." = "subtropical"
  )

  # Apply replacements sequentially
  for (pattern in names(replacements)) {
    text <- gsub(pattern, replacements[pattern], text, perl = TRUE)
  }

  # Replace ampersands with "and"
  text <- gsub("\\s*&\\s*", " and ", text)

  return(text)
}
