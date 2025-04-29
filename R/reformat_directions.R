
#' Replace text from POWO so that it matches IUCN standards
#'
#' @param text (character) POWO distribtion string
#'
#' @return IUCN standardised POWO range string


replace_cardinal_directions <- function(text) {
  # Create a mapping of abbreviations to full forms (lowercase)
  replacements <- c(
    "N\\." = "north",
    "N\\.E\\." = "northeast", # Handle N.E. first before N.
    "NE\\." = "northeast",
    "E\\." = "east",
    "S\\." = "south",
    "S\\.E\\." = "southeast", # Handle S.E. first before S.
    "SE\\." = "southeast",
    "S\\.W\\." = "southwest", # Handle S.W. first before S.
    "SW\\." = "southwest",
    "W\\." = "west",
    "N\\.W\\." = "northwest", # Handle N.W. first before N.
    "NW\\." = "northwest"
  )

  # Apply replacements sequentially
  for (pattern in names(replacements)) {
    text <- gsub(pattern, replacements[pattern], text, perl = TRUE)
  }

  # Replace ampersands with "and"
  text <- gsub("\\s*&\\s*", " and ", text)

  return(text)
}
