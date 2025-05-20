

#' Clean a keys output to ensure only accepted species from GBIF and WCVP are being analysed
#'
#' @param keys data frame of GBIF and WCVP identifier keys
#' @return Returns a data frame with cleaned GBIF and WCVP keys
#' @export
#' @details Problematic records e.g. not accepted in either GBIF or WCVP,
#' or not at species level, are removed. Duplicate records are reported, but not removed.

clean_keys <- function(keys) {

  cleaned_keys <- keys %>% dplyr::filter(wcvp_status == "Accepted",
                                         GBIF_status == "ACCEPTED",
                                         GBIF_rank == "SPECIES",
                                         wcvp_rank == "Species"
                                         )

  cli::cli_alert_info (
    "Cleaning complete."
  )

  multi <- cleaned_keys %>%
    dplyr::group_by(searchName) %>%
    dplyr::count() %>%
    dplyr::filter(n >= 2)

  # only show message if there are any multi - matches
  if (nrow(multi) > 0) {
  cli::cli_alert_warning(
      "There {?is/are} {nrow(multi)} record{?s} that matched more than one name. Please review."
    )
    purrr::walk(
      multi$searchName,
      ~cli::cli_li("{.val {.x}}"))
  }

  return(cleaned_keys)
  }
