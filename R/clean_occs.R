
#' Clean GBIF occurrences based on user-selected flags and/or specific gbifIDs
#'
#' @param checked_occs Result from check_occs() function
#' @param remove_flags Vector of flag names to use for cleaning.
#'                     Set to NULL to ignore all automatic flags. Flag names must be: "flag_no_coords" Missing coordinates;
#'                     "flag_cc_capitals" Near country capitals; "flag_cc_centroids" Near country centroids;
#'                     "flag_cc_institutions" Near biodiversity institutions; "flag_cc_equal" Equal coordinates; "flag_cc_gbif"
#'                     GBIF headquarters; "flag_high_uncertainty" High coordinate uncertainty; "flag_outside_native" Outside native range
#'
#' @param remove_gbifids Vector of gbifIDs to remove from the dataset, or a named list where
#'                       names are reasons and values are vectors of gbifIDs sharing that reason.
#'                       Example: list("identification uncertain" = c("1234", "5678"),
#'                                    "habitat mismatch" = c("9012"))
#' @param gbifid_reason Character string providing the default reason for manual gbifID removal
#'                     when remove_gbifids is a simple vector.
#' @param keep_gbifids Vector of gbifIDs to keep in the clean dataset even if they are flagged
#'                     as problematic by automatic flags. These records will override any flags.
#'                     Example: c("5101884416", "1252668978")
#'
#' @return A list containing:
#'   - clean_occs: Cleaned occurrences based on selected flags and/or gbifIDs
#'   - problem_occs: Occurrences that were filtered out with their flags/reasons
#'
#' @export
clean_occs <- function(checked_occs,
                       remove_flags = c("flag_no_coords",
                                        "flag_cc_capitals",
                                        "flag_cc_centroids",
                                        "flag_cc_institutions",
                                        "flag_cc_equal",
                                        "flag_cc_gbif",
                                        "flag_cc_zeros",
                                        "flag_high_uncertainty",
                                        "flag_outside_native"),
                       remove_gbifids = NULL,
                       gbifid_reason = NULL,
                       keep_gbifids = NULL) {

  cli::cli_alert_info("Cleaning {nrow(checked_occs)} records")

  #data <- checked_occs$flagged_data
  data <- checked_occs

  # Initialize the problematic flag
  data$is_problematic <- FALSE

  # Handle flags based on user input
  if (!is.null(remove_flags)) {
    # Apply selected flags
    for (flag in remove_flags) {
      if (flag %in% colnames(data)) {
        data$is_problematic <- data$is_problematic | data[[flag]]
      }
    }
  }

  # Add gbifID-based filtering
  if (!is.null(remove_gbifids)) {
    # Create a new column to store reasons for manual removal
    data$manual_removal_reason <- NA_character_

    if (is.list(remove_gbifids)) {
      # If remove_gbifids is a named list, process each reason group
      for (reason_name in names(remove_gbifids)) {
        ids_for_reason <- remove_gbifids[[reason_name]]
        # Mark records with this reason
        matches <- data$gbifID %in% ids_for_reason
        data$is_problematic <- data$is_problematic | matches
        # Set the removal reason for these specific records
        data$manual_removal_reason[matches] <- paste0("Manually removed by gbifID - ", reason_name)
      }
    } else if (is.vector(remove_gbifids)) {
      # If remove_gbifids is a simple vector, use the default reason
      matches <- data$gbifID %in% remove_gbifids
      data$is_problematic <- data$is_problematic | matches

      # Format the default reason
      default_reason <- "Manually removed by gbifID"
      if (!is.null(gbifid_reason) && nchar(gbifid_reason) > 0) {
        default_reason <- paste0(default_reason, " - ", gbifid_reason)
      }

      # Set the removal reason for these records
      data$manual_removal_reason[matches] <- default_reason
    }

    # Mark records as manually removed for flagging
    data$flag_manual_gbifid <- !is.na(data$manual_removal_reason)

    # Add the gbifID flag to remove_flags for tracking reasons
    if (is.null(remove_flags)) {
      remove_flags <- "flag_manual_gbifid"
    } else {
      remove_flags <- c(remove_flags, "flag_manual_gbifid")
    }
  } else {
    # Add empty flag and reason column if not using gbifID filtering
    data$flag_manual_gbifid <- FALSE
    data$manual_removal_reason <- NA_character_
  }

  # Override problematic status for manually kept gbifIDs
  # This should happen AFTER all other flags have been applied
  if (!is.null(keep_gbifids)) {
    keep_matches <- data$gbifID %in% keep_gbifids
    # Override the problematic flag for these records
    data$is_problematic[keep_matches] <- FALSE

    # Add a flag to track which records were manually kept
    data$flag_manual_keep <- keep_matches
  } else {
    data$flag_manual_keep <- FALSE
  }

  # Add a problem_reason column that lists all problems - improved approach to avoid extra commas
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      problem_reasons = {
        reasons <- c()
        if (flag_no_coords & "flag_no_coords" %in% remove_flags) reasons <- c(reasons, "Missing coordinates")
        if (flag_cc_capitals & "flag_cc_capitals" %in% remove_flags) reasons <- c(reasons, "Near country capitals")
        if (flag_cc_centroids & "flag_cc_centroids" %in% remove_flags) reasons <- c(reasons, "Near country centroids")
        if (flag_cc_institutions & "flag_cc_institutions" %in% remove_flags) reasons <- c(reasons, "Near biodiversity institutions")
        if (flag_cc_equal & "flag_cc_equal" %in% remove_flags) reasons <- c(reasons, "Equal coordinates")
        if (flag_cc_gbif & "flag_cc_gbif" %in% remove_flags) reasons <- c(reasons, "GBIF headquarters")
        if (flag_cc_zeros & "flag_cc_zeros" %in% remove_flags) reasons <- c(reasons, "Zero coordinates")
        if (flag_high_uncertainty & "flag_high_uncertainty" %in% remove_flags) reasons <- c(reasons, "High coordinate uncertainty")
        if (flag_outside_native & "flag_outside_native" %in% remove_flags) reasons <- c(reasons, "Outside native range")

        # Add the specific manual removal reason if present
        if (flag_manual_gbifid & "flag_manual_gbifid" %in% remove_flags && !is.na(manual_removal_reason)) {
          reasons <- c(reasons, manual_removal_reason)
        }

        # Add note if record was manually kept despite flags
        if (flag_manual_keep && length(reasons) > 0) {
          reasons <- c(reasons, "Manually kept despite flags")
        }

        # Join with commas only if there are reasons
        if (length(reasons) > 0) {
          paste(reasons, collapse = ", ")
        } else {
          ""
        }
      }
    ) %>%
    dplyr::ungroup()

  # Split into clean and problem datasets
  clean_occs <- dplyr::filter(data, !is_problematic)
  problem_occs <- dplyr::filter(data, is_problematic)

  # Remove flag columns from clean occurrences but keep problem reasons in problem dataset
  clean_cols <- colnames(clean_occs)[!grepl("^flag_|is_problematic|manual_removal_reason", colnames(clean_occs))]
  clean_occs <- clean_occs[, clean_cols]

  problem_cols <- c(colnames(problem_occs)[!grepl("^flag_|is_problematic|manual_removal_reason", colnames(problem_occs))])
  problem_occs <- problem_occs[, problem_cols]

  cli::cli_alert_success("Cleaning complete.")

  return(list(
    clean_occs = clean_occs,
    problem_occs = problem_occs
  ))
}
