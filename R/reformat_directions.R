
#' Replace text from POWO so that it matches IUCN standards
#'
#' @param text (character) POWO distribtion string
#'
#' @return IUCN standardised POWO range string


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

