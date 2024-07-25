#' Reorder taxonomic status to give priority to accepted name
#'
#' @description arranges order of taxonomic status from GBIF query
#'
#' @param df (character) data frame with
#'
#' @return Returns a data frame with taxonomic status sorted

reorder_status <- function(df) {

  status_levels <- c("ACCEPTED", "SYNONYM", "DOUBTFUL", "OTHER")

  df <- df %>%
    dplyr::mutate(status = factor(status, levels = status_levels, ordered = TRUE)) %>%
    dplyr::arrange(status)

  return(df)
}
