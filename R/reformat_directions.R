
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
    "S\\.E\\." = "southeast", # Handle S.E. first before S.data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACgAAAAkCAYAAAD7PHgWAAABBklEQVR4Xu2XMQrCQBBFBQvR6wgJHsEDpHVjBDvvoBhbI3bWCkZbFUyhFrYiEat0WgmC6AVkdQqbIVmWZAOi82C64b+/bDWZDEEQP4phTLMaa9d003bTGMgu1psF7JVGNzuWPdzs18GDz443rgrIcndXbvW8g1axGfZKo7P2eBXc+WB74a3FGXtiA1kwzfnpqTF7hL3SwDfAaz+BqvjkwYADe6WhglQwJlQwKVQwKakVTGOoYNL5z4JxwBlUMEwqAu9SwTCpCLxLBcOkIvCusoKT9/WFQ6OkIvCukoJwt5rO0sehUVIReBem6ng+OLBXmnKjn4PbGM5PeKnqgXIlo5vHXoL4Nl4ZYqbbEGA7+wAAAABJRU5ErkJggg==
    "SE\\." = "southeast",
    "S\\.W\\." = "southwest", # Handle S.W. first before S.
    "SW\\." = "southwest",
    "W\\." = "west",
    "N\\.W\\." = "northwest", # Handle N.W. first before N.
    "NW\\." = "northwest",
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
