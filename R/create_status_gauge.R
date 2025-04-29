
#' Generate metrics from a clean point occurrence file
#'
#' @param species_data (df) results from make_metrics() function
#'
#' @return Returns a ggplot to visualise the Least Concern results
#' @export
#'
create_status_gauge <- function(species_data) {

  # Define thresholds
  thresholds <- c(EOO = 30000, AOO = 3000, NOP = 75, WGSRPD_count = 5, recent_records = 50)

  # Extract metrics and calculate percentages
  metrics <- data.frame(
    Metric = c("EOO", "AOO", "Number of points", "WGSRPD count", "Recent records"),
    Value = c(species_data$EOOkm2, species_data$AOOkm, species_data$NOP, species_data$WGSRPD_count, species_data$recent_records),
    Threshold = c(thresholds["EOO"], thresholds["AOO"], thresholds["NOP"], thresholds["WGSRPD_count"], thresholds["recent_records"])
  )

  metrics$Percentage <- pmin(metrics$Value / metrics$Threshold, 1)
  metrics$Status <- ifelse(metrics$Percentage >= 1, "LC", "Concern")

  # Create plot
  p <- ggplot(metrics, aes(x = Metric, y = Percentage)) +
    geom_col(aes(fill = Percentage)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    scale_fill_gradient2(low = "red", mid = "orange", high = "green", midpoint = 0.5, limits = c(0, 1)) +
    scale_y_continuous(
      labels = function(x) paste0(x * 100, "%"),
      limits = c(0, 1.1),
      breaks = c(0.25, 0.5, 0.75, 1.0)
    ) +
    labs(title = paste("Least Concern Metrics for", species_data$taxon),
         y = "% of LC Threshold", x = "") +
    theme_minimal() +
    theme(legend.position = "none")

  return(p)
}
