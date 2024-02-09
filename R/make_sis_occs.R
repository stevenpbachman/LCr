
#' Convert cleaned gbif DWCA structure to IUCN SIS format
#'
#' @param occs_clean (character) Cleaned GBIF occurrence file in DWCA format. See
#' occs_clean() function.
#' @param first_name (character) First name of the assessor
#' @param second_name (character) Second name of the assessor
#' @param institution (character) Institution/affiliation of the assessor
#'
#' @return An dataframe of occurence points aligned with IUCN Red List spatial data standards

make_sis_occs <- function(occs_clean, first_name = "", second_name = "", institution = ""){
  sis_points <- dplyr::select(occs_clean,
                              wcvp_ipni_id,
                              species,
                              recordedBy,
                              recordNumber,
                              day,
                              month,
                              year,
                              dec_lat,
                              dec_long,
                              locality,
                              coordinateUncertaintyInMeters,
                              basisOfRecord,
                              elevation,
                              catalogNumber,
                              gbifID
  )

  sis_points <- dplyr::rename(sis_points,
                              internal_taxon_id = wcvp_ipni_id,
                              basisofrec = basisOfRecord,
                              sci_name = species,
                              catalog_no = catalogNumber,
                              coorduncert = coordinateUncertaintyInMeters,
                              event_year = year,
                              min_elev = elevation,
                              recordedby = recordedBy,
                              recordno = recordNumber
  )

  sis_points <- dplyr::mutate(sis_points,
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
                                "LITERATURE" = "",
                                "LIVING_SPECIMEN" = "LivingSpecimen",
                                "MACHINE_OBSERVATION" = "MachineObservation",
                                "OBSERVATION" = "",
                                "PRESERVED_SPECIMEN" = "PreservedSpecimen",
                                "UNKNOWN" = "Unknown"
                              )
  )
  sis_points <- dplyr::select(sis_points, -gbifID)
  #sis_points <- sf::st_drop_geometry(sis_points)

  # adding in ISO codes
  iso_codes <- occs_to_iso(sis_points)
  sis_points <- dplyr::bind_cols(sis_points, iso_codes)

  return(sis_points)
}
