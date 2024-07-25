
#' resolves taxonomicAuthor field when there are varying taxonimic fields returned from GBIF query
#'
#' @param df
#'
#' @return returns GBIF taxonimic information with reformatted taxonomicAuthor

check_tax_auth <- function(df) {
  # Check if 'bracketauthorship' and 'authorship' columns exist
  has_bracket <- "bracketauthorship" %in% colnames(df)
  has_author <- "authorship" %in% colnames(df)

  if (has_bracket & has_author) {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = paste(bracketauthorship, authorship))
  } else if (has_author) {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = authorship)
  } else {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = "")
  }

  return(df)
}
