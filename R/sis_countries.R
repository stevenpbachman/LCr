
#' Generate the countries.csv file
#'
#' @param native_ranges (character) native range list of WGSRPD level 3
#'
#' @return Returns an SIS compliant csv file
#' @export

sis_countries <- function(native_ranges, unique_id) {

  country_table <- purrr::map_dfr(unique_id, function(id) {

        country_table <- dplyr::filter(native_ranges, POWO_ID == id)

        country_table <- dplyr::left_join(sp_countries, tdwg2iucn, by = c("LEVEL3_COD" = "Level.3.code"))

        country_table$CountryOccurrence.CountryOccurrenceSubfield.presence <- "Extant"
        country_table$CountryOccurrence.CountryOccurrenceSubfield.origin <- "Native"
        country_table$CountryOccurrence.CountryOccurrenceSubfield.seasonality <- "Resident"
        country_table$internal_taxon_id <- id

        country_table <- dplyr::select(
          country_table,
          internal_taxon_id,
          countryoccurrence.countryoccurrencesubfield.countryoccurrencename,
          CountryOccurrence.CountryOccurrenceSubfield.presence,
          CountryOccurrence.CountryOccurrenceSubfield.origin,
          CountryOccurrence.CountryOccurrenceSubfield.seasonality,
          countryoccurrence.countryoccurrencesubfield.countryoccurrencelookup
        )
  })

  country_table <- dplyr::distinct(country_table, .keep_all = TRUE)

  return(country_table)
  }
