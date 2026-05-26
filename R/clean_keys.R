

#' Clean a keys output to ensure only accepted species from GBIF and/or WCVP are being analysed
#'
#' @param keys data frame of GBIF and/or WCVP identifier keys, as produced by
#'   \code{get_name_keys()}. Must have a \code{sources} attribute indicating
#'   which sources are present (e.g. \code{"GBIF"}, \code{"WCVP"}, or both).
#' @return Returns a data frame with cleaned keys, retaining only records that
#'   are accepted and at species rank in all present sources.
#' @export
#' @details Problematic records — e.g. those not accepted in GBIF or WCVP,
#'   or not at species level — are removed. WCVP filtering is only applied when
#'   WCVP data are present in \code{keys} (i.e. \code{"WCVP"} appears in the
#'   \code{sources} attribute). Duplicate records are reported but not removed.

clean_keys <- function(keys) {

  sources <- attr(keys, "sources")
  if (is.null(sources)) {
    cli::cli_abort(
      "No {.field sources} attribute found on {.arg keys}.
       Ensure keys were generated with {.fn get_name_keys}."
    )
  }

  cli::cli_inform("Sources detected: {.val {sources}}")

  cleaned_keys <- keys

  # --- WCVP filtering (only when WCVP data are present) ---
  if ("WCVP" %in% sources) {
    cleaned_keys <- cleaned_keys %>%
      dplyr::filter(wcvp_status == "Accepted", wcvp_rank == "Species")
  } else {
    cli::cli_inform("No WCVP data detected — skipping WCVP filtering.")
  }

  # --- GBIF filtering (only when GBIF data are present) ---
  if ("GBIF" %in% sources) {
    cleaned_keys <- cleaned_keys %>%
      dplyr::filter(GBIF_status == "ACCEPTED", GBIF_rank == "SPECIES")
  } else {
    cli::cli_inform("No GBIF data detected — skipping GBIF filtering.")
  }

  cli::cli_alert_success("Cleaning complete. {nrow(cleaned_keys)} record{?s} retained.")

  # --- Duplicate check ---
  # Group only by searchName; avoid referencing source-specific columns
  # that may not be present in all configurations.
  multi <- cleaned_keys %>%
    dplyr::group_by(searchName) %>%
    dplyr::count() %>%
    dplyr::filter(n >= 2)

  if (nrow(multi) > 0) {
    cli::cli_alert_warning(
      "There {?is/are} {nrow(multi)} searchName{?s} matched to more than one record. \\
       Please review:"
    )
    purrr::walk(multi$searchName, ~ cli::cli_li("{.val {.x}}"))
  }

  return(cleaned_keys)
}
