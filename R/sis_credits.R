

#' Generate the credits.csv file
#'
#' @param unique_id (character) Unique identifier
#' @param first_name (character) First name of assessor
#' @param second_name (character) Second name of assessor
#' @param email (character) Email of assessor
#' @param affiliation (character) Name of institution or affiliation
#' @return Returns an SIS compliant csv file
#' @export

sis_credits = function(unique_id,
                       first_name = "your first name",
                       second_name = "your second name",
                       email = "your email",
                       affiliation = "your affiliation") {

  combined_table <- purrr::map_dfr(unique_id, function(id) {
    credits <- tibble::tibble(
      internal_taxon_id = id,
      credit_type = "Assessor",
      firstName = first_name,
      lastName = second_name,
      initials = "",
      Order = "1",
      email = email,
      affiliation = affiliation,
      user_id = "1"
    )
  })

  return(combined_table)
}
