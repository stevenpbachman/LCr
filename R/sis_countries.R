
#' Generate the countries.csv file
#'
#' @param unique_id (character) Unique identifier
#' @param wcvp_ipni_id (character) WCVP identifier
#' @param native_ranges (character) native range list of WGSRPD level 3
#'
#' @return Returns an SIS compliant csv file
#' @export


sis_countries <- function(unique_id, wcvp_ipni_id, native_ranges) {
  combined_table <- purrr::map_dfr(seq_along(unique_id), function(i) {
    unique_id <- unique_id[i]
    wcvp_ipni_id <- wcvp_ipni_id[i]

    native_ranges_filtered <- dplyr::filter(native_ranges, POWO_ID == wcvp_ipni_id)

    country_table <- dplyr::left_join(native_ranges_filtered, tdwg2iucn, by = c("LEVEL3_COD" = "Level.3.code"))

    country_table$CountryOccurrence.CountryOccurrenceSubfield.presence <- "Extant"
    country_table$CountryOccurrence.CountryOccurrenceSubfield.origin <- "Native"
    country_table$CountryOccurrence.CountryOccurrenceSubfield.seasonality <- "Resident"
    country_table$internal_taxon_id <- unique_id

    country_table <- dplyr::select(country_table,
                            internal_taxon_id,
                            countryoccurrence.countryoccurrencesubfield.countryoccurrencename,
                            CountryOccurrence.CountryOccurrenceSubfield.presence,
                            CountryOccurrence.CountryOccurrenceSubfield.origin,
                            CountryOccurrence.CountryOccurrenceSubfield.seasonality,
                            countryoccurrence.countryoccurrencesubfield.countryoccurrencelookup)

    country_table <- dplyr::distinct(country_table, .keep_all = TRUE)

    return(country_table)
  })

  return(combined_table)
}
