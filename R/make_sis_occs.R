
#' Convert cleaned gbif DWCA structure to IUCN SIS format
#'
#' @param occs_clean (character) Cleaned GBIF occurrence file in DWCA format. See
#' occs_clean() function.
#' @param first_name (character) First name of the assessor
#' @param second_name (character) Second name of the assessor
#' @param institution (character) Institution/affiliation of the assessor
#'
#' @return An dataframe of occurence points aligned with IUCN Red List spatial data standards
#' @export

make_sis_occs <- function(occs_clean, first_name = "", second_name = "", institution = ""){
  sis_points <- dplyr::select(occs_clean,
                              species,
                              recordedBy,
                              recordNumber,
                              day,
                              month,
                              year,
                              decimalLatitude,
                              decimalLongitude,
                              locality,
                              coordinateUncertaintyInMeters,
                              habitat,
                              basisOfRecord,
                              elevation,
                              catalogNumber,
                              speciesKey,
                              gbifID
  )

  sis_points <- dplyr::rename(sis_points,
                              basisofrec = basisOfRecord,
                              sci_name = species,
                              catalog_no = catalogNumber,
                              coorduncert = coordinateUncertaintyInMeters,
                              event_year = year,
                              min_elev = elevation,
                              max_elev = elevation,
                              recordedby = recordedBy,
                              recordno = recordNumber,
                              dist_comm = habitat,
                              dec_lat = decimalLatitude,
                              dec_long = decimalLongitude
  )

  sis_points <- dplyr::mutate(sis_points,
                              internal_taxon_id = speciesKey,
                              source = paste0("https://www.gbif.org/occurrence/", gbifID),
                              yrcompiled = format(Sys.Date(), "%Y"),
                              citation = institution,
                              compiler = paste0(first_name, " ", second_name),
                              data_sens = "0",
                              sens_comm = "",
                              presence = "1",
                              origin = "1",
                              seasonal = "1",
                              spatialref = "WGS84",
                              basisofrec = dplyr::recode(
                                basisofrec,
                                "FOSSIL_SPECIMEN" = "FossilSpecimen",
                                "HUMAN_OBSERVATION" = "HumanObservation",
                                "LIVING_SPECIMEN" = "LivingSpecimen",
                                "MACHINE_OBSERVATION" = "MachineObservation",
                                "PRESERVED_SPECIMEN" = "PreservedSpecimen",
                                "UNKNOWN" = "Unknown",
                                "OCCURRENCE" = "HumanObservation",
                                "LITERATURE" = "",
                                "OBSERVATION" = "HumanObservation",
                                "MATERIAL_ENTITY" = "",
                                "MATERIAL_SAMPLE" = "PreservedSpecimen",
                                "MATERIAL_CITATION" = "PreservedSpecimen"
                              )
  )

  sis_points <- dplyr::select(sis_points, -speciesKey,-gbifID)

  return(sis_points)
}
