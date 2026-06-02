#' lifeform_mapping_POWO_RL
#'
#' A dataset used internally by the LCr package.
#' Maps POWO lifeform codes to IUCN life form codes
#'
#' @format A data frame with 448 rows and 3 variables:
#' \describe{
#'   \item{lifeform_description}{chr}
#'   \item{description}{chr}
#'   \item{code}{chr}
#' }
#' @keywords internal
"lifeform_mapping_POWO_RL"

#' 'TDWG_realms'
#'
#' A dataset used internally by the LCr package.
#' Describes biogeographic realms used by IUCN Red List
#'
#' @format A data frame with 394 rows and 4 variables:
#' \describe{
#'   \item{LEVEL3_NAM}{chr}
#'   \item{LEVEL3_COD}{chr}
#'   \item{FID_Ecoreg}{dbl}
#'   \item{REALM}{chr}
#' }
#' @keywords internal
"TDWG_realms"

#' 'iucn_taxonomy'
#'
#' A dataset used internally by the LCr package.
#' Provides upper level taxonomic ranks used by IUCN Red List
#'
#' @format A data frame with 3942 rows and 5 variables:
#' \describe{
#'   \item{family}{chr}
#'   \item{kingdom}{chr}
#'   \item{phylum}{chr}
#'   \item{classname}{chr}
#'   \item{ordername}{chr}
#' }
#' @keywords internal
"iucn_taxonomy"

#' 'tdwg2iucn'
#'
#' A dataset used internally by the LCr package.
#' Maps WGSRPD L3 TDWG values to IUCN land region values
#'
#' @format A data frame with 866 rows and 5 variables:
#' \describe{
#'   \item{ID}{dbl}
#'   \item{Level.3.code}{chr}
#'   \item{TDWG_Name}{chr}
#'   \item{countryoccurrence.countryoccurrencesubfield.countryoccurrencename}{chr}
#'   \item{countryoccurrence.countryoccurrencesubfield.countryoccurrencelookup}{chr}
#' }
#' @keywords internal
"tdwg2iucn"

#' 'tdwg_level3'
#'
#' A dataset used internally by the LCr package.
#' WGSRPD L3 TDWG values with geometry
#'
#' @format A data frame with 369 rows and 5 variables:
#' \describe{
#'   \item{LEVEL3_NAM}{chr}
#'   \item{LEVEL3_COD}{chr}
#'   \item{LEVEL2_COD}{dbl}
#'   \item{LEVEL1_COD}{dbl}
#'   \item{geometry}{MULTIPOLYGON}
#' }
#' @keywords internal
"tdwg_level3"

