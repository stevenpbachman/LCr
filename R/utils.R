
# Utility/helper functions


#' Resolves taxonomicAuthor field when there are varying taxonomic fields returned from GBIF query
#'
#' @param df dataframe with gbif name search output
#'
#' @return returns GBIF taxonomic information with reformatted taxonomicAuthor
#' @keywords internal
#' @noRd
#' @encoding UTF-8
#' @importFrom stats family na.omit quantile setNames

check_tax_auth <- function(df) {
  # Check if 'bracketauthorship' and 'authorship' columns exist
  has_bracket <- "bracketauthorship" %in% colnames(df)
  has_author <- "authorship" %in% colnames(df)

  if (has_bracket & has_author) {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = paste0("(", bracketauthorship, ") ", authorship))
  } else if (has_author) {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = authorship)
  } else {
    df <- df %>%
      dplyr::mutate(taxonomicAuthority = "")
  }

  return(df)
}

#' Generate GBIF download citation according to IUCN format
#'
#' @param gbif_meta (character) GBIF download metadata
#'
#' @return Returns a data frame with IUCN SIS Connect reference
#' @keywords internal
#' @noRd

get_gbif_ref <- function(gbif_meta) {
  tibble::tibble(
    Reference_type = "Assessment",
    type = "electronic source",
    author = "GBIF.org",
    year =  format(as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"),
    title = "GBIF Occurrence Download accessed from R via rgbif (https://github.com/ropensci/rgbif)",
    url = paste0("https://doi.org/", gbif_meta$doi),
    access_date = format(as.Date(Sys.Date(), format = "%d/%m/%Y"))
  )
}

#' Generate a list of biorealms for a species
#'
#' @param native_ranges (dataframe) native ranges
#'
#' @return concatenated list of biorealms in SIS connect format
#' @keywords internal
#' @noRd

make_biorealms = function(native_ranges) {
  biogeographic_realm <- native_ranges %>%
    dplyr::left_join(TDWG_realms, by = c("LEVEL3_COD" = "LEVEL3_COD"), relationship = "many-to-many") %>%  # Match regions to realms
    dplyr::group_by(internal_taxon_id) %>%
    dplyr::summarise(BiogeographicRealm.realm = gsub(", ", " | ", paste(unique(REALM), collapse = ", ")),
              .groups = "drop")

  return(biogeographic_realm)
}

#' Generate the elevation upper and lower bounds for the allfields.csv file
#'
#' @param occs
#'
#' @return Returns upper and lower elevation based on elevation field in GBIF occurrence file.
#' @keywords internal
#' @noRd

make_elevation <- function(occs) {
  elevation_stats <- occs %>%
    dplyr::group_by(internal_taxon_id) %>%
    dplyr::summarise(
      ElevationLower.limit = round(quantile(min_elev, 0.05, na.rm = TRUE)),
      ElevationUpper.limit = round(quantile(min_elev, 0.95, na.rm = TRUE)),
      #n = n(),
      .groups = "drop"
    )

  return(elevation_stats)
}

#' Return native range of an accepted plant species from POWO (Plants of the
#' World Online) using WCVP identifier
#'
#' @param wcvp_name (character) WCVP name
#' @param names (data frame) A data frame of taxonomic names from WCVP
#' @param distributions (data frame) A data frame of distributions from WCVP
#'
#' @return (dataframe) Native range codes according to World Geographic Scheme for Recordings Plant Distributions (WGSRPD)
#' @keywords internal
#' @noRd

powo_range = function(wcvp_name, names, distributions) {
  results = tibble::tibble(
    LEVEL3_COD = NA_character_,
    LEVEL3_NAM = NA_character_
    #powo_name = wcvp_name,
  )

  distribution <- tryCatch(
    rWCVP::wcvp_distribution(wcvp_name,
                              taxon_rank = "species",
                              introduced = FALSE,
                              extinct = FALSE,
                              location_doubtful = FALSE,
                              wcvp_names = names,
                              wcvp_distributions = distributions),
    error = function(e) NULL
  )

  if (!is.null(distribution)) {
    results <- distribution %>%
      sf::st_drop_geometry() %>%
      dplyr::select(-c(LEVEL2_COD,
                       LEVEL1_COD,
                       occurrence_type))
      #dplyr::mutate(powo_name = wcvp_name)

    results = dplyr::mutate(results,
                            LEVEL3_NAM = dplyr::recode(LEVEL3_NAM, "u00e1" = "a"))
  }

  return(results)
}

#' Generate POWO download citation according to IUCN format
#'
#' This function uses queries POWO to get the native range
#' @returns Returns a dataframe with IUCN SIS Connect reference
#' @keywords internal
#' @noRd

powo_ref <- function() {
  tibble::tibble(
    Reference_type = "Assessment",
    type = "electronic source",
    author = "POWO",
    year =  format(as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"),
    title = "Plants of the World Online. Facilitated by the Royal Botanic Gardens, Kew. Published on the Internet",
    url = "http://www.plantsoftheworldonline.org",
    access_date = format(as.Date(Sys.Date(), format = "%d/%m/%Y"))
  )
}

#' Lookup distribution, habit and climate text in POWO (Plants of the World Online) using WCVP ID
#'
#' @param wcvp_ipni_id (character) WCVP identifier
#' @param occs (data frame) Occurrence data with elevation information
#' @param names (data frame) A data frame of taxonomic names from WCVP
#'
#' @return Returns a list with POWO data
#' @keywords internal
#' @noRd

powo_text <- function(wcvp_ipni_id, occs, unique_id, names) {
  #returned_data <- powo_lookup(wcvp_ipni_id)
  #Sys.sleep(0.5)
  names_sp <- names %>%
    dplyr::filter(powo_id == wcvp_ipni_id)

  dist_text <- names_sp$geographic_area
  habit_text <- names_sp$lifeform_description
  clim_text <- names_sp$climate_description

  year_only <- format(Sys.Date(), format = "%Y")

  # add elevation here if possible - from occs
  # If occs is provided, calculate and merge elevation data
  # Need a check to see if there are any or sufficient points with elevation data
  if (!is.null(occs)) {
    occs <- occs %>% dplyr::filter(internal_taxon_id == unique_id)
    elevation_stats <- make_elevation(occs)
    elev_text <- paste0(
      " The estimated elevational range for this species is from ",
      format(
        elevation_stats$ElevationLower.limit,
        big.mark = ",",
        scientific = FALSE
      ),
      " to ",
      format(
        elevation_stats$ElevationUpper.limit,
        big.mark = ",",
        scientific = FALSE
      ),
      " m asl."
    )
  }

  # reformat text to make IUCN compatible
  dist_text <- paste0("The native range of this species is ",
                      dist_text,
                      " (POWO, ",
                      year_only,
                      ").")
  if (exists("elev_text") && !is.null(elev_text)) {
    dist_text <- paste0(dist_text, elev_text)
  }
  iucn_dist_text <- replace_cardinal_directions(dist_text)

  iucn_habit_text <- paste0(
    "It is a ",
    habit_text,
    " and grows primarily in the ",
    clim_text,
    " biome (POWO, ",
    year_only,
    ")."
  )

  return(
    list(
      dist_text = dist_text,
      habit_text = habit_text,
      clim_text = clim_text,
      iucn_dist_text = iucn_dist_text,
      iucn_habit_text = iucn_habit_text
    )
  )
}

#' Replace text from POWO so that it matches IUCN standards
#'
#' @param text (character) POWO distribution string
#'
#' @return IUCN standardised POWO range string
#' @keywords internal
#' @noRd

replace_cardinal_directions <- function(text) {

  # Replace ampersands with 'and'
  text <- gsub(" *& *", " and ", text)

  lowercase_terms = c("subalpine", "subarctic", "temperate","mountains", "tropical", "subtropical", "north", "south", "east", "west", "central",
                      "northeast", "southeast", "northwest", "southwest",
                      "north central", "south central", "east central", "west central",
                      "northeast central", "southeast central", "northwest central", "southwest central")

  replacements <- list(
    "\\bN\\.E\\. C\\." = "northeast central",
    "\\bS\\.E\\. C\\." = "southeast central",
    "\\bS\\.W\\. C\\." = "southwest central",
    "\\bN\\.W\\. C\\." = "northwest central",
    "\\bN\\. C\\." = "north central",
    "\\bS\\. C\\." = "south central",
    "\\bE\\. C\\." = "east central",
    "\\bW\\. C\\." = "west central",

    "\\bN\\.E\\." = "northeast",
    "\\bS\\.E\\." = "southeast",
    "\\bS\\.W\\." = "southwest",
    "\\bN\\.W\\." = "northwest",
    "\\bNE\\." = "northeast",
    "\\bSE\\." = "southeast",
    "\\bSW\\." = "southwest",
    "\\bNW\\." = "northwest",

    "\\bN\\." = "north",
    "\\bS\\." = "south",
    "\\bE\\." = "east",
    "\\bW\\." = "west",
    "\\bC\\." = "central",

    "\\bTemp\\." = "temperate",
    "\\bTrop\\." = "tropical",
    "\\bSubtrop\\." = "subtropical"
  )

  # Apply replacements, in order of decreasing pattern length
  sorted_patterns <- names(replacements)[order(nchar(names(replacements)), decreasing = TRUE)]

  for (pattern in sorted_patterns) {
    text <- gsub(
      pattern,
      replacements[[pattern]],
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  }

  # Lowercase known full-form terms, only when they appear as whole words
  for (term in lowercase_terms) {
    pattern <- paste0("\\b", tools::toTitleCase(term), "\\b")
    text <- gsub(pattern, term, text)
  }

  return(text)
}

#' Reorder taxonomic status to give priority to accepted name
#'
#' @description arranges order of taxonomic status from GBIF query
#'
#' @param df (character) data frame with names
#'
#' @return Returns a data frame with taxonomic status sorted
#' @keywords internal
#' @noRd

reorder_status <- function(df) {

  status_levels <- c("ACCEPTED", "SYNONYM", "DOUBTFUL", "OTHER")

  df <- df %>%
    dplyr::mutate(status = factor(status, levels = status_levels, ordered = TRUE)) %>%
    dplyr::arrange(status)

  return(df)
}


#' Generate the eoo and aoo values for the allfields.csv file
#'
#' @param occs
#'
#' @return Returns upper and lower elevation based on elevation field in GBIF occurrence file.
#' @keywords internal
#' @noRd

make_eoo_aoo <- function(occs) {

  # use rCAT to get the main results
  resultsdf <-batchCon(taxa = occs$internal_taxon_id,
                             long = occs$dec_long,
                             lat = occs$dec_lat)

  resultsdf$taxon <- as.integer(resultsdf$taxon)
  resultsdf$internal_taxon_id <- resultsdf$taxon
  resultsdf <- resultsdf %>% dplyr::select(internal_taxon_id, EOOkm2,AOOkm) %>%
    dplyr::rename(AOO.range = AOOkm, EOO.range = EOOkm2)
  resultsdf$AOO.justification = paste0("The AOO is a minimum estimate and is not used in the assessment.")
  resultsdf$EOO.justification = paste0("The EOO was calculated from a convex hull around cleaned and georeferenced occurrence data using the rCAT package (Moat 2020).")

  return(resultsdf)
}

#' Generate global variables
#'
#' @return Generate global variables
#' @keywords internal
#' @noRd

# global variables ####
globalVariables(c(
  # check_occs.R
  #"flag_no_coords",
  "LEVEL3_COD",
  # utils.R
  "bracketauthorship",
  "authorship",
  # clean_keys.R
  "wcvp_status",
  "wcvp_rank",
  "GBIF_status",
  "GBIF_rank",
  "searchName",
  #"n",
  # clean_occs.R
  #"flag_cc_capitals",
  #"flag_cc_centroids",
  #"flag_cc_institutions",
  #"flag_cc_equal",
  #"flag_cc_gbif",
  #"flag_cc_zeros",
  #"flag_cc_urb",
  #"flag_high_uncertainty",
  #"flag_outside_native",
  #"flag_manual_gbifid",
  #"manual_removal_reason",
  #"flag_manual_keep",
  #"is_problematic",
  # get_name_keys.R
  "GBIF_usageKey",
  #get_occs_range.R
  "sci_name",
  #make_biorealms
  "TDWG_realms",
  "REALM",
  #make_elevation
  "min_elev",
  #make_eoo_aoo
  "EOOkm2",
  "AOOkm",
  #make_metrics
  "year",
  "proj_metadata",
  "EOOcat",
  "AOOcat",
  "taxonKey",
  "recent",
  "WGSRPD_count",
  "recent_records",
  "EOOkm2",
  "AOOkm",
  "NOP",
  "lc_eoo",
  "lc_aoo",
  "lc_nop",
  "lc_regions",
  "lc_recent",
  "lc_core",
  "lc_supporting",
  "taxon",
  # make_sis_occs
  "species",
  "recordedBy",
  "recordNumber",
  "day",
  "month",
  "year",
  "decimalLatitude",
  "decimalLongitude",
  "locality",
  "coordinateUncertaintyInMeters",
  "habitat",
  "basisOfRecord",
  "elevation",
  "catalogNumber",
  "taxonKey",
  "gbifID",
  "basisofrec",
  "dec_lat",
  "dec_long",
  # make_zip
  "sis_files",
  # map_species_batch
  "taxonKey",
  #map_species_single
  "decimalLatitude",
  "decimalLongitude",
  "drawRectangleOptions",
  "drawShapeOptions",
  "drawPolygonOptions",
  "editToolbarOptions",
  "selectedPathOptions",
  "pathOptions",
  #".data",
  "species_list",
  "count",
  "gbif_ids",
  "layersControlOptions",
  "coord_key",
  "decimalLatitude",
  "decimalLongitude",
  "gbifID",
  "species",
  #powo_range
  "name",
  "tdwgCode",
  "LEVEL3_NAM",
  #powo_text
  #reorder_status
  "status",
  #search_name_gbif
  "confidence",
  "genusorabove",
  "specificepithet",
  "taxonomicAuthority",
  #sis_assessments
  "BiogeographicRealm.realm.y",
  "BiogeographicRealm.realm.x",
  #sis_countries
  "tdwg2iucn",
  "countryoccurrence.countryoccurrencesubfield.countryoccurrencename",
  "CountryOccurrence.CountryOccurrenceSubfield.presence",
  "CountryOccurrence.CountryOccurrenceSubfield.origin",
  "CountryOccurrence.CountryOccurrenceSubfield.seasonality",
  "countryoccurrence.countryoccurrencesubfield.countryoccurrencelookup",
  #sis_plantspecific
  "PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsName",
  "PlantGrowthForms.PlantGrowthFormsSubfield.PlantGrowthFormsLookup",
  #sis_taxonomy
  "iucn_taxonomy"
))


# rCAT scripts full ####

#' @title Batch process, preliminary conservation assessments
#' @description
#' Combines the main of routines in rCAT to process multiple species for AOO, EOO etc.
#' @author Justin Moat. J.Moat@kew.org
#' @author Steve Bachman
#' @keywords internal
#' @noRd
#' @param taxa field which defines a list of species or taxa
#' @param lat field which defines the latitude set of points
#' @param long field which defines the longitude set of points
#' @param project2gether TRUE or FALSE, TRUE all data is projected together using the centre of all latitudes and longitudes. FALSE each species is projected separately. Default = TRUE
#' @param cellsize cell length in metres used to for AOO projection N.B. IUCN recommend 2000 m (default 2000)
#' @param aooMin calls the aooMin routines as well as simple aoo, be warned with lots of species and points this can take some time to run (default=FALSE)
#' @param it if aooMin=TRUE this determines the number of iterations it will run to find aooMin (default=1296)
#' @param returnV switches to return different sets of results: \cr
#' S = simple returns a dataframe of results = (taxa ,Number of points,EOO in km2, Simple AOO in km2,Minimum AOO, EOO category, AOOcategory, Cellwidth, projection parameters) \cr
#' SF = simple features dataframe will all results, taxa in taxon field, type=(EOO,AOO,points). NB all points will be projected together and aooMin is ignored
batchCon <- function(taxa,long,lat,project2gether=TRUE,cellsize=2000,aooMin=FALSE,it=1296, returnV='S'){
  if (returnV == "SF") {
    project2gether = TRUE
  }

  points <- data.frame(lat, long)

  if (project2gether) {
    points <- simProjWiz(points)
  }

  split_points <- split(points, f=taxa)

  if(returnV=='SF'){
    crs <- attr(points, "crs")
    ntaxa <- length(unique(taxa))

    eoo_geoms <- lapply(split_points, function(p) eoo(p, "SF"))
    aoo_geoms <- lapply(split_points, function(p) aoo(p, cellsize, "SF"))
    aoo_geoms <- lapply(aoo_geoms, function(g) st_sfc(st_multipolygon(g), crs=crs))
    point_geoms <- lapply(split_points, function(p) st_sfc(st_multipoint(data.matrix(p)), crs=crs))

    geoms <- c(
      do.call(c, eoo_geoms),
      do.call(c, aoo_geoms),
      do.call(c, point_geoms)
    )

    results <- st_sf(
      taxon=rep(unique(taxa), 3),
      type=c(rep("eoo", ntaxa), rep("aoo", ntaxa), rep("points", ntaxa)),
      geometry=geoms
    )

  } else {
    if (! project2gether) {
      split_points <- lapply(split_points, simProjWiz)
      proj_strings <- lapply(split_points, function(p) attr(p, "crs"))
      proj_strings <- do.call(c, proj_strings)
    } else {
      proj_strings <- attr(points, "crs")
    }

    n_points <- lapply(split_points, nrow)
    eoo_areas <- lapply(split_points, eoo)
    eoo_ratings <- lapply(eoo_areas, ratingEoo)
    aoo_areas <- lapply(split_points, function(p) aoo(p, cellsize))

    if (aooMin) {
      min_aoo_areas <- lapply(split_points, function(p) aooFixedRotation(p, cellsize, it))
      aoo_ratings <- lapply(min_aoo_areas, ratingAoo)
    } else {
      aoo_ratings <- lapply(aoo_areas, ratingAoo)
    }

    results <- data.frame(
      taxon=unique(taxa),
      NOP=do.call(c, n_points),
      EOOkm2=do.call(c, eoo_areas),
      AOOkm=do.call(c, aoo_areas),
      EOOcat=do.call(c, eoo_ratings),
      AOOcat=do.call(c, aoo_ratings),
      cellwidth=cellsize,
      proj_metadata=proj_strings,
      row.names = NULL
    )

    if (aooMin) {
      min_aoo_areas <- lapply(split_points, function(p) aooFixedRotation(p, cellsize, it))
      results$MinAOO <- do.call(c, min_aoo_areas)
    }

  }

  results
}

#returns intercept for two points and radius
#example
#l_c_intercepts(c(2,1),c(-6,8),2)
l_c_intercepts <- function(midp,edgepoint,R){
  Ax <- midp[1]
  Ay <- midp[2]
  Bx <- edgepoint[1]
  By <- edgepoint[2]
  #compute the euclidean distance between A and B
  LAB <- sqrt((Bx-Ax)^2+(By-Ay)^2)
  #compute the direction vector D from A to B
  Dx <- (Bx-Ax)/LAB
  Dy = (By-Ay)/LAB
  # first intersection point
  Fx <- (LAB-R)*Dx + Ax
  Fy <- (LAB-R)*Dy + Ay
  # second intersection point
  Gx <- (LAB+R)*Dx + Ax
  Gy <- (LAB+R)*Dy + Ay
  return(c(Fx,Fy,Gx,Gy))
}

#' Construct a polygon from vertices.
#'
#' Accepts an x and a y vector to define the vertices of
#' the polygon, to make it easier
#'
#' @param x East to West coordinate in metres
#' @param y South to North coordinate in metres
#' @param crs coordinate reference system
#' @keywords internal
#' @noRd
constructPolygon <- function(x, y, crs){
  points <- cbind(x, y)

  is_closed <- all(points[1,] == points[nrow(points),])

  if (! is_closed) {
    points <- rbind(points, points[1,])
  }

  geom <- st_polygon(list(points))

  # put geometry into an sfc so we can attach a crs
  if (is.null(crs)) {
    crs <- ""
  }

  polygon <- st_sfc(geom, crs=crs)

  if (is.na(st_crs(polygon))) {
    warning("No valid CRS provided so setting it to `NA`")
  }

  polygon
}

#' @title Extent of Occurrence (EOO) Area
#' @description
#' Calculates the Extent of Occurrence in km2 or returns a simple feature polygon from a set of points (x,y)
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param points dataframe of points in metres i.e. c(x,y)
#' @param returnV switch to return different sets of results: \cr
#' S = Simple, returns just the minimum area in km2, (DEFAULT) \cr
#' SF = returns a polygon simple feature for mapping, plotting in ggplot/plot or to export to a GIS format
#'
#' @return float_value area of EOO polygon or sf polygon
#' @note area returned is in x,y units, but negative as polygon is constructed anticlockwise
#' @seealso \code{\link{ratingEoo}} for EOO Ratings
#' @importFrom grDevices chull
#' @importFrom pracma polyarea
#' @import sf
#' @references
#' Bachman, S., Moat, J., Hill, A.W., de Torre, J., Scott, B., 2011. Supporting Red List threat assessments with GeoCAT: geospatial conservation assessment tool. Zookeys 126, 117–26. doi:10.3897/zookeys.150.2109
#'
#' Joppa, L.N., Butchart, S.H.M., Hoffmann, M., Bachman, S.P., Akçakaya, H.R., Moat, J.F., Böhm, M., Holland, R.A., Newton, A., Polidoro, B., Hughes, A., 2016. Impact of alternative metrics on estimates of extent of occurrence for extinction risk assessment. Conserv. Biol. 30, 362–370. doi:10.1111/cobi.12591
eoo <- function(points, returnV="S") {
  if (! "X" %in% colnames(points) | ! "Y" %in% colnames(points)) {
    stop("Point coordinates must be supplied in columns named 'X' and 'Y'.")
  }
  hull_idx <- chull(points)
  hull <- points[hull_idx,]

  area <- polyarea(x=hull$X, y=hull$Y)
  # hull is constructed backwards, so area is negative and in m^2
  area <- -1 * area / 1e6
  #check if area is truly NA or just one point
  if (is.na(area)){
    print("hit")
    if(nrow(unique(points)) < 2){
      area = 0
    }
  }
  if (nrow(unique(points)) == 0){
    area = NA
  }
  #returns results
  if (returnV == "S") {
    area
  } else {
    constructPolygon(hull$X, hull$Y, attr(points, "crs"))
  }
}

#calculates the IUCN rating based on AOO and EOO area
#' Calculates IUCN rating on EOO
#' @title IUCN rating based on EOO Area
#' @description
#' Calculates IUCN rating based on Extent of Occurrence (EOO) Area in km2
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param EOOArea Area in km2
#' @param abb abbreviation TRUE or FALSE , TRUE = 2 letter code, FALSE = full text (see value), default = TRUE
#' @return Text
#' one of CR, EN, VU, NT, LC or Critically Endangered, Endangered, Vulnerable, Near Threatened, Least Concern
#' @seealso \code{\link{eoo}} for EOO calculations
ratingEoo <- function(EOOArea,abb=TRUE){
  #  EOOArea <- 250
  #  abb <- FALSE
  #make positive
  EOOArea <- sqrt(EOOArea * EOOArea)
  cat <- NA
  if (identical(abb,FALSE)){
    if (is.na(EOOArea)) {
      cat <- "Data Deficient"
    } else if (EOOArea < 100){
      cat <- "Critically Endangered"
    } else if (EOOArea < 5000){
      cat <- "Endangered"
    } else if (EOOArea < 20000){
      cat <- "Vulnerable"
    } else if (EOOArea < 30000){
      cat <- "Near Threatened"
    } else
      cat <- "Least Concern"

  } else {
    if (is.na(EOOArea)){
      cat <- "DD"
    } else if (EOOArea < 100){
      cat <- "CR"
    } else if (EOOArea < 5000){
      cat <- "EN"
    } else if (EOOArea < 20000){
      cat <- "VU"
    } else if (EOOArea < 30000){
      cat <- "NT"
    } else
      cat <- "LC"
  }
  return (cat)
}

#kept for consistency with old version 0.1.6
ratingEoo <- ratingEoo

#' @title IUCN rating based on AOO Area
#' @description
#' Calculates IUCN rating based on Area of occupancy (AOO) in km2
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param AOOArea Area in km2
#' @param abb abbreviation TRUE or FALSE , TRUE = 2 letter code, FALSE = full text (default = TRUE)
#' @return Text one of CR, EN, VU, NT, LC or Critically Endangered, Endangered, Vulnerable, Near Threatened, Least Concern
ratingAoo <- function(AOOArea,abb=TRUE){
  if(missing(abb)){
    abb = TRUE
  }
  cat <- NA
  cat <- ratingEoo(AOOArea*10,abb)
  return(cat)
}

#kept for consistency with old version 0.1.6
ratingAoo <- ratingAoo


#calculates the IUCN rating based on population reduction
#' Calculates IUCN rating based on population reduction
#' @title IUCN rating based on population reduction
#' @description
#' Calculates IUCN rating based on based on population reduction as a percentage
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param pReduction reduction as a percentage
#' @param subCr sub Criteria category: 1 or 2 or 3 or 4 (2 default)
#' @param abb abbreviation TRUE or FALSE , TRUE = 2 letter code, FALSE = full text (see value), default = TRUE
#' @return Text
#' one of CR, EN, VU, NT, LC or Critically Endangered, Endangered, Vulnerable, Near Threatened, Least Concern
ratingPop <- function(pReduction,subCr=2,abb=TRUE){
  #  EOOArea <- 250
  #  abb <- FALSE
  cat <- NA
  if ( subCr > 1){ #2 or 3 or 4
    if (identical(abb,FALSE)){
      if (pReduction >= 80){
        cat <- "Critically Endangered"
      } else if (pReduction >= 50){
        cat <- "Endangered"
      } else if (pReduction >= 30){
        cat <- "Vulnerable"
      } else if (pReduction >= 10){
        cat <- "Near Threatened"
      } else
        cat <- "Least Concern"

    } else {
      if (pReduction >= 80){
        cat <- "CR"
      } else if (pReduction >= 50){
        cat <- "EN"
      } else if (pReduction >= 30){
        cat <- "VU"
      } else if (pReduction >= 10){
        cat <- "NT"
      } else
        cat <- "LC"
    }}
  if ( subCr == 1){ #2 or 3 or 4
    if (identical(abb,FALSE)){
      if (pReduction >= 90){
        cat <- "Critically Endangered"
      } else if (pReduction >= 70){
        cat <- "Endangered"
      } else if (pReduction >= 50){
        cat <- "Vulnerable"
      } else if (pReduction >= 25){
        cat <- "Near Threatened"
      } else
        cat <- "Least Concern"

    } else {
      if (pReduction >= 90){
        cat <- "CR"
      } else if (pReduction >= 70){
        cat <- "EN"
      } else if (pReduction >= 50){
        cat <- "VU"
      } else if (pReduction >= 25){
        cat <- "NT"
      } else
        cat <- "LC"
    }}
  return (cat)
}


#Simple area Projection Wizard
#' @title Simple Projection Wizard
#' @description
#' Projects any set of latitude and longitude points to a "suitable" area projection, based on thieir "true centre of gravity".
#' Data is expected as lat long in decimal degrees and returned in metres.
#' Input data is checked to make sure it’s sensible before projection (i.e. lat and longs on the earth no null or NA values)
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints set of points as a dataframe with latitude and longitude
#' @param thecentre one point i.e. c(lat,long), if not specified this will be calculated from the center of gravity of all points
#' @param returnV switches to return either  dataframe (x,y) or simple feature of points  \cr
#' S = simple, returns as dataframe of x,y \cr
#' SF = simple feature of points
#' @return Defaults is a set of points in meters as a dataframe with projection details attributed (stored as crs to retrieve attr(myprojectedpoints,'crs'))
#' @import sf
simProjWiz <- function(thepoints,thecentre,returnV="S"){
  #check dataframe is sensible
  llCheck(thepoints)
  #names(thepoints) <- c("long", "lat")
  #
  if (missing(thecentre)){
    thecentre <- trueCOGll(thepoints)
  }
  #setup and set projection to WGS84
  thepoints <- st_as_sf(thepoints, coords = c("long", "lat"), crs = 4326)
  #depending on centre point, choose projection
  if((thecentre[1] < 70) & (thecentre[1] > -70)){
    CRSstring <- paste("+proj=cea +lon_0=", thecentre[2],   " +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep = "")
  } else {
    CRSstring <- paste("+proj=laea +lat_0=", thecentre[1]," +lon_0=", thecentre[2], " +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep = "")
  }

  #reproject
  xysp <- st_transform(thepoints, CRSstring)

  if(returnV=="SF"){return (xysp)}
  else {
    xy <- as.data.frame(st_coordinates(xysp))
    attr(xy,'crs') <- CRSstring
    return(xy)
  }
}

#calculates 'true' centre of gravity from a set of lat long points in decimal degrees   #
#note z from mean of cartesian give some idea of weighted spread on the globe#

#' @title True centre of gravity from a set of Lat longs
#' @description
#' Calculates the "true" centre of gravity (weighted) from a set of lat longs, using cartesian geometry. Used as part of the projection wizard.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints set of points c(lat,long)
#' @return a point (lat,long) from centre
trueCOGll <-function(thepoints){

  llrad <- deg2rad(thepoints) #to radians
  cartp <- ll2cart(llrad$lat,llrad$long) #to cartesian
  mp <- data.frame(x=mean(cartp$x),y=mean(cartp$y),z=mean(cartp$z)) #central point
  pmp <- pro2sph(mp$x,mp$y,mp$z) #projection to surface
  pmprll <- cart2ll(pmp$x,pmp$y,pmp$z) #to ll in radians
  pmpll <- rad2deg(pmprll) #to degrees
  return(data.frame(lat=pmpll$latr,long=pmpll$longr))
}


#calculates the Cartesian cordinates (x,y,z) from lat long in radians#

#' @title Geographic coordinates to cartesian (x,y,z)
#' @description
#' Calculates the Cartesian coordinates (x,y,z) from lat long in radians. Used as part of the projection wizard.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param latr latitude point in radians
#' @param longr longtitude point in radians
#' @return dataframe of x,y,z
ll2cart <- function(latr,longr){
  x <- cos(latr) * cos(longr)
  y <- cos(latr) * sin(longr)
  z <- sin(latr)
  return(data.frame(x,y,z))
}


#calculates the lat long cordinates in radians from Cartesian (x,y,z)#

#' @title Cartesian (x,y,z) to Geographic coordinates
#' @description
#' calculates the latitude and longitude cordinates in radians from Cartesian coordinates (x,y,z). Used as part of the projection wizard.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param x East to West coordinate in metres
#' @param y South to North coordinate in metres
#' @param z height coordinate in metres
#' @return dataframe of latitude,longtitude
cart2ll <-function (x,y,z){
  latr <- asin(z)
  longr <- atan2(y,x)
  return(data.frame(latr,longr))
}

#calculates Cartesian (x,y,z), projected from the centre of the sphere
#to the earth surface, returns cartesian (x,y,z)
#used to calculate "true" centre of set of lat longs
# http://stackoverflow.com/questions/9604132/how-to-project-a-point-on-to-a-sphere

#' @title Cartesian coordinate projection
#' @description
#' Used as part of the projection wizard, calculates Cartesian (x,y,z), projected from the centre of the sphere to the earth surface, returns cartesian coordinates (x,y,z)
#'
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @note
#' http://stackoverflow.com/questions/9604132/how-to-project-a-point-on-to-a-sphere
#'
#' @param x East to West coordinate in metres
#' @param y South to North coordinate in metres
#' @param z height coordinate in metres
#' @return x,y,z
pro2sph <- function (x,y,z){
  sc <- 1/sqrt(x^2 + y^2 + z^2)
  x <- x * sc
  y <- y * sc
  z <- z * sc
  return(data.frame(x,y,z))
}


#radians to degrees and degrees to radians

#' @title Radians to Degrees
#' @description
#' Calculates degrees from radians
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param rad number in radians
#' @return number
rad2deg <- function(rad) {(rad * 180) / (pi)}

#radians to degrees and degrees to radians

#' @title
#' Degrees to radians
#' @description
#' Calculates radians from degrees
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param deg number in degrees
#' @return number
deg2rad <- function(deg) {(deg * pi) / (180)}


#Crude dataframe check for sensible latitude and longitude data

#' @title
#' Check point data and latitude and longitude are sensible to work with
#' @description
#' Checked the dataframe for NA, latitude below -90 or above 90, longitude below -180 and above 180, also warns if it finds whole numbers or 0
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints set of points as a dataframe with latitude and longitude
#' @return nothing or warning or error
llCheck <- function(thepoints){
  if(is.null(thepoints$lat) | is.null(thepoints$long)){
    stop("data should be in lat long ie with field names lat long")
  }
  if (lapply(thepoints,function(x) any(is.na(x)))$lat | lapply(thepoints,function(x) any(is.na(x)))$long ){
    stop("NA withing you data, please remove any NA")
  }
  if(any(thepoints$lat < -90) | any(thepoints$lat > 90)){
    stop("-90 < lat > 90, latitude not within sensible limits")
  }
  if(any(thepoints$long < -180) | any(thepoints$long > 180)){
    stop("-180 < long > 180, longtitude not within sensible limits")
  }
  pwhole <- sum(round(thepoints) == thepoints)/(nrow(thepoints) * ncol(thepoints))
  if (pwhole > 0.1){
    warning(paste (pwhole*100, "% of your points are whole numbers, as you sure they are in lat long?"), call. = FALSE)
  }
  pzero <- sum(thepoints == 0)/(nrow(thepoints) * ncol(thepoints))
  if (pzero > 0){
    warning(paste (pzero*100, "% of your points have 0 lat or long, are these true lat longs or no data, if no data then remove?"), call. = FALSE)
  }
}

#Scripts for Rapoport's mean propinquity

#returns the Euclidean Minimum spanning tree from a set of points

#' @title Euclidean Minimum spanning tree
#' @description
#' Calculates the Euclidean Minimum spanning tree from a set of points.
#' This is used for the  tree and branch building part of Rapoport's (1982) mean propinquity method.
#'
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints dataframe of points in metres i.e. c(X,Y)
#' @return Simple feature of linestring, with a df of X1,Y1,X2,Y2,distance and geom. N.B. X1,Y1 & and X2 Y2 are the to and from points
#' @import sf
#' @import emstreeR
eMST <- function (thepoints){
  crs <- attr(thepoints,'crs')
  thepoints <- thepoints[,c('X','Y')]
  edges <- nrow(thepoints) - 1
  cmst <- ComputeMST(thepoints, verbose=FALSE)
  cmst <- cmst[1:edges, 3:5] #drops the last point and the X/Y's

  # not sure this is the best way, but hopefully clear
  line_idx <- split(cmst, f=1:nrow(cmst))

  lines <- lapply(line_idx, function(idx) {
    p1 <- c(thepoints[idx$from,]$X, thepoints[idx$from,]$Y)
    p2 <- c(thepoints[idx$to,]$X, thepoints[idx$to,]$Y)

    st_linestring(rbind(p1, p2))
  })

  lines <- st_sfc(lines, crs=crs)

  st_sf(
    X1=thepoints[cmst$from,]$X,
    Y1=thepoints[cmst$from,]$Y,
    X2=thepoints[cmst$to,]$X,
    Y2=thepoints[cmst$to,]$Y,
    distance=cmst$distance,
    geometry=lines
  )
}


#calculates the optimum AOO (smallest) by shifting the grid

#' @title Area of Occupancy (AOO), optimal shifting grid,
#' @description
#' Calculates the optimal (smallest) Area of Occupancy AOO  by shifting the grid in the x and y direction only.
#' The minimum solution will be achieved but large point datasets (i.e. over 100 points) will take some time to process.
#' Processing time is proportional to (number of points squared (n^2)).
#' Please cite below if using this algorithm:
#' Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints dataframe of points in metres i.e. c(x,y)
#' @param cellsize width of cell in metres (default 2000 m)
#' @param returnV, switches to return different sets of results:
#' S = Simple, returns just the minimum are in km2, (DEFAULT)
#' E = Expended simple, returns the solution for the smallest AOO  as list of (area,number of cells, rotation (0 in this case), shift in x direction, shift in y direction)
#' ALL = returns a dataframe of all of the results with (number of cells, rotation (0 in this case), shift in x direction (metres), shift in y direction (metres))
#' SF = returns a polygon simple feature for mapping, plotting in ggplot or export to GIS systems
#' @return dependent on switch, default is area in km2
#' @import sf
aooFixedGrid <- function(thepoints,cellsize=2000,returnV="S"){
  #Warning for large dataset, user can at least be warned and kill
  if (nrow(thepoints) > 70){ warning(paste("This will run",nrow(thepoints)^2,"times - it may take some time!"),immediate. = TRUE)}
  #starting grid 0,0 just to get  marker (not used later)
  cpoints <- thepoints/cellsize
  bestpoints <- unique(floor(cpoints))
  minN <- nrow(bestpoints)
  #setup varibles to populate with the 2x loops
  resultsdf <- data.frame(nofcells=numeric(),rotation=numeric(),xshift=numeric(),yshift=numeric())
  t <- 0
  shiftx <- 0
  shifty <- 0
  #the sequences from left of point
  xpon <- cpoints$X - floor(cpoints$X)
  ypon <- cpoints$Y - floor(cpoints$Y)
  for (i in xpon){
    for (j in ypon){
      testps <- cbind(cpoints$X - i,cpoints$Y - j)
      testcps <- unique(floor(testps))
      t <- t+1
      resultsdf[t,] <- c(nrow(testcps),0,i*cellsize,j*cellsize)
      if (nrow(testcps)< minN){
        bestpoints<- cbind(testcps[,1] + i, testcps[,2] +j)
        minN <- nrow(bestpoints)
      }
    }
  }

  #get the first minimum grid for results and returns
  bestgrid <- resultsdf[which.min(resultsdf$nofcells),]
  if(returnV == "E"){
    return(list(area=bestgrid$nofcells * (cellsize^2)/1000000,nocells=bestgrid$nofcells,
                rotation = rad2deg(bestgrid$rotation),
                xshift= bestgrid$xshift, yshift = bestgrid$yshift))
  }
  if(returnV == "SF"){
    #build df of best points for SF production
    bestpoints <- data.frame (bestpoints * cellsize)
    colnames(bestpoints)<-c("x","y")
    return(buildCells(bestpoints,cellsize,0,shiftx,shifty,attr(thepoints,'crs')))
  }
  if(returnV == "ALL"){return(resultsdf)}
  else {return(minN * (cellsize^2)/1000000)}
}


#' @title Area of Occupancy (AOO) calculated by Systematic shifting and rotating of the grid
#' @description
#' Calculates the Area of Occupancy AOO (smallest) by shifting and rotating the grid in x and y direction only.
#' In a very few occasions the minimum solution will not always be achieved but it is quick and consistent (not driven by the number of points).
#' If your species is near a threshold you may want to increase the number of iterations.
#' Please cite below if using this algorithm:
#' Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints dataframe of points in metres i.e. c(x,y)
#' @param it the number of iterations you wish it to run, (default 1296)
#' @param cellsize width of cell in metres (default 2000 m)
#' @param returnV, switches to return different sets of results:
#' S = Simple, returns just the minimum are in km2, (DEFAULT)
#' E = Expended simple, returns the solution for the smallest AOO  as list of (area,number of cells, rotation (degrees), shift in x direction, shift in y direction)
#' ALL = returns a dataframe of all of the results with (number of cells, rotation (radians), shift in x direction (metres), shift in y direction (metres))
#' SF = returns a polygon simple feature for mapping, plotting in ggplot or export to GIS systems.
#' @param rotation allow rotation of grids? (default = TRUE). If rotations are selected iterations are shared 50:50 rotation:shift
#' @return dependent on switch, default is area in km2
#' @seealso \code{\link{ratingAoo}} for AOO Ratings
#' @seealso \code{\link{aooFixedGrid}} for fixed grid optimal method
#' @seealso \code{\link{aoo}} for simple AOO method
#' @seealso \code{\link{buildCellPolys_rxy}} for building grid polygons from points, rotation and shift
#' @import sf
aooFixedRotation <- function(thepoints,cellsize=2000,it=1296,returnV="S",rotation=TRUE){
  #Build iterative lists
  if (rotation){
    rotlist <- seq(from = 0, to = pi/2, length=it^(1/2))
    xpon <- seq(from=0, to=cellsize, length=it^(1/4))
    ypon <- seq(from=0, to=cellsize, length=it^(1/4))
  } else {
    rotlist <- 0
    xpon <- seq(from=0, to=cellsize, length=it^(1/2))
    ypon <- seq(from=0, to=cellsize, length=it^(1/2))
  }
  #setup variables for storage and testing within loops
  minn <- nrow(thepoints)
  #bestpoints <- unique(floor(thepoints/cellsize))
  shiftx <- 0
  shifty <- 0
  minr <- 0
  t <- 0
  resultsdf <- data.frame(nofcells=numeric(),rotation=numeric(),xshift=numeric(),yshift=numeric())
  #the big ^3 loop
  for (i in xpon){
    for (j in ypon){
      for (r in rotlist){
        #shift points
        testps <- cbind(thepoints$X-i,thepoints$Y-j)
        #rotate points
        if(rotation){
          rps <- rotateP(testps,r)
        } else {
          rps<-data.frame(x=testps[,1],y=testps[,2])
        }
        rcells <- unique(floor(rps/cellsize)) * cellsize
        t <- t + 1
        resultsdf[t,] <- c(nrow(rcells),r,i,j)
        if (nrow(rcells) < minn){
          minn <- nrow(rcells)
          bestpoints<- cbind(rcells[,1], rcells[,2])
          shiftx <- i
          shifty <- j
          minr <- r
        }

      }
    }
  }
  bestpoints <- data.frame (bestpoints)
  colnames(bestpoints)<-c("x","y")
  bestgrid <- resultsdf[which.min(resultsdf$nofcells),]
  if(returnV == "E"){return(list(area=bestgrid$nofcells * (cellsize^2)/1000000,nocells=bestgrid$nofcells, rotation =rad2deg(bestgrid$rotation),
                                 xshift= bestgrid$xshift, yshift = bestgrid$yshift))}
  if(returnV == "ALL"){return(resultsdf)}
  if(returnV == "SF"){buildCells(bestpoints,cellsize,-minr,shiftx,shifty,attr(thepoints,'crs'))}
  else{return(bestgrid$nofcells * (cellsize^2)/1000000)}
}


#calculates the initial AOO, with simple grid 0,0 #
#' calculates a very simple AOO area from a set of points
#' @title Area of Occupancy (AOO), grid orgin 0,0
#' @description
#' Calculates the number area the of occupied cells for (Area of Occupancy AOO) from a set of points (x,y), projected into metres, with origin 0,0.
#' Please cite: Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289. if using this algorithm:
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints set of points in metres i.e. c(x,y)
#' @param cellsize size of cell (length) in metres
#' @param returnV, switches to return different sets of results: \cr
#' S = Simple, returns just the AOO area in km2, (DEFAULT) \cr
#' E = Expended simple, returns the solution for the AOO as list of (area,number of cells, rotation (0 degrees), shift in x direction(0), shift in y direction(0)). This is used so as be compatiable with other AOO calculations. \cr
#' SF = returns a polygon simple feature for mapping and plotting in ggplot/plot or export to GIS format.
#' @return as returnV, default is area in km2
#' @seealso \code{\link{ratingAoo}} for AOO Ratings from IUCN categories
aoo <- function(thepoints, cellsize=2000, returnV="S"){
  bottomleftpoints <- unique(floor(thepoints/cellsize))

  cellp <- data.frame(
    x=(bottomleftpoints$X * cellsize),
    y=(bottomleftpoints$Y * cellsize)
  )

  if (returnV == "E") {
    return(list(
      area=nrow(cellp) * (cellsize^2)/1000000,
      nocells=nrow(cellp),
      rotation=0,
      xshift=0,
      yshift=0
    ))
  }


  if (returnV == "SF") {
    buildCells(cellp, cellsize, 0, 0, 0, attr(thepoints,'crs'))
  } else {
    return(nrow(cellp) * (cellsize^2)/1000000)
  }
}
###for consistency with old rCAT 0.1.6
AOOsimp <- aoo


#' @title BETA Area of Occupancy (AOO), from systematic grid rotation and shifting
#' @description
#' Calculates the Area of Occupancy AOO (smallest) by rotating the grid and shifting in x and y direction.
#' On a very few occasions the minimum solution will not always be achieved, but this solution is quick and consistent (not driven by the number of points).
#' If your species is near a threshold you may want to increase the number of iterations.
#' Please cite if using this algorithm: Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 12781289. \cr
#' Works the same as aooFixedRotation, but much faster. In BETA until fully tested
#' On a very few occasions the minimum solution will not always be achieved but it is quick and consistent (not driven by the number of points).
#' If your species is near a threshold you may want to increase the number of iterations.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints dataframe of points in metres i.e. c(x,y)
#' @param it the number of iterations you wish it to run, (default 1296)
#' @param cellsize width of cell in metres (default 2000 m)
#' @param returnV, switches to return different sets of results: \cr  \cr
#' S = Simple, returns just the minimum AOO area in km2, (DEFAULT)  \cr
#' E = Expended simple, returns the solution for the smallest AOO  as list of (area,number of cells, rotation (degrees), shift in x direction, shift in y direction)  \cr
#' ALL = returns a dataframe of all of the results from all the trials with (number of cells, rotation (0 in this case), shift in x direction (metres), shift in y direction (metres))  \cr
#' SF = returns a polygon simple feature for mapping, plotting in ggplot/plot or to export to a GIS format.
#' @param rotation allow rotation of grids? (default = TRUE). If rotations are trigger selected iterations are shared 50:50 rotation:shift(both in x and y direction)
#' @return dependent on switch, default is area in km2

#' @seealso \code{\link{ratingAoo}} for AOO Ratings
#' @seealso \code{\link{aooFixedGrid}} for fixed grid optimal method
#' @seealso \code{\link{aooFixedRotation}} for original method method
#' @seealso \code{\link{aoo}} for simple AOO method
#' @seealso \code{\link{buildCellPolys_rxy}} for building grid polygons from points, rotation and shift

#' @import sf
aooFixedRotationo <- function(thepoints,cellsize=2000,it=1296,returnV="S",rotation=TRUE){
  #get whole number for the iterations
  itf <- round(it^0.25)
  if (rotation){
    rotlist <- seq(from = 0, to = pi/2, length=itf^2)
    xpon <- seq(from=0, to=cellsize, length=itf)
    ypon <- seq(from=0, to=cellsize, length=itf)
  } else {
    rotlist <- 0
    xpon <- seq(from=0, to=cellsize, length=itf^2)
    ypon <- seq(from=0, to=cellsize, length=itf^2)
  }
  #setup mapply lists
  rl <- rep(rotlist,each=length(rotlist))
  xl <- rep(rep(xpon,length(rotlist)),each=length(xpon))
  yl <- rep(rep(ypon,length(rotlist)),length(ypon))
  #unique (cbind(rl,xl,yl))
  shiftrotgrid <- function(i,j,r){
    #shift
    testps <- cbind(thepoints$X-i,thepoints$Y-j)
    #rotate
    if (rotation){rps <- rotatePm(testps,r)} else {rps<-cbind(testps[,1],testps[,2])}
    cells <- floor(rps/cellsize)
    celltxt <- paste(cells[,1],cells[,2])
    rcells <- unique(celltxt)
    c(length(rcells),r,i,j)
  }
  mresults <- mapply (shiftrotgrid,xl,yl,rl)
  #convert to df
  resultsdf <- as.data.frame(t(mresults))
  names(resultsdf) <- c('nofcells','rotation','xshift','yshift')
  #get the first minimum grid for results and returns
  bestgrid <- resultsdf[which.min(resultsdf$nofcells),]
  if(returnV == "E"){return(list(area=bestgrid$nofcells * (cellsize^2)/1000000,nocells=bestgrid$nofcells, rotation =rad2deg(bestgrid$rotation),
                                 xshift= bestgrid$xshift, yshift = bestgrid$yshift))}
  if(returnV == "ALL"){return(resultsdf)}
  if(returnV == "SF"){buildCellPolys_rxy(thepoints,cellsize,bestgrid$rotation,bestgrid$xshift,bestgrid$yshift)}
  else{return(bestgrid$nofcells * (cellsize^2)/1000000)}
}


#rotates a matrix of points
#much quicker that rotateP, but uses and return a matrix
#used for aooFixedRotationo
rotatePm <- function(thepoints, angle){
  #build rotation matrix
  rotationmatrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)),byrow = TRUE, 2, 2)
  pr <- thepoints %*% rotationmatrix
  return(pr)
}

#calculates the optimum AOO (smallest) by shifting the grid

#' @title BETA Area of Occupancy (AOO), optimal shifting grid,
#' @description
#' Calculates the optimal (smallest) Area of Occupancy AOO  by shifting the grid in x and y direction only.
#' The minimum solution will be achieved but large point datasets (i.e. over 70 points) will take some time to process.
#' Processing time is proportional to nop^2 (number of points squared).
#' Please cite below if using this algorithm:
#' Moat, J., Bachman, S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to address the modifiable areal unit problem in ecology and conservation. Conservation biology, 32(6), 1278-1289.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints dataframe of points in metres i.e. c(x,y)
#' @param cellsize width of cell in metres (default 2000 m)
#' @param returnV, switches to return different sets of results:  \cr \cr
#' S = Simple, returns just the minimum are in km2, (DEFAULT)  \cr
#' E = Expended simple, returns the solution for the smallest AOO  as list of (area,number of cells, rotation (0 in this case), shift in x direction, shift in y direction) \cr
#' ALL = returns a dataframe of all of the results with (number of cells, rotation (0 in this case), shift in x direction (metres), shift in y direction (metres))  \cr
#' SF = returns a polygon simple feature for mapping, plotting in ggplot or export to GIS systems  \cr
#' @return dependent on switch, default is area in km2
#' @seealso \code{\link{ratingAoo}} for AOO Ratings
#' @seealso \code{\link{aoo}} for simple AOO method
#' @seealso \code{\link{aooFixedRotation}} for systematic methods with rotation
#' @seealso \code{\link{aooFixedGrid}} for orginal method
#' @seealso \code{\link{buildCellPolys_rxy}} for building grid polygons from points, rotation and shift

#' @import sf
aooFixedGrido <- function(thepoints,cellsize=2000,returnV="S"){
  #Warning for large dataset, user can at least be warned and kill
  if (nrow(thepoints) > 100){ warning(paste("This will run",nrow(thepoints)^2,"times - it may take some time!"),immediate. = TRUE)}
  cpoints <- thepoints/cellsize

  #main function for mapply
  shiftgrid <- function(i,j){
    testps <- cbind(X=cpoints$X - i,Y=cpoints$Y - j)
    testcpi <- floor(testps)
    testcpi <- paste(testcpi[,1],testcpi[,2])
    testcps <- unique(testcpi)
    c(length(testcps),0,i*cellsize,j*cellsize)
  }
  #builds the actual lists for mapply
  xpon <- cpoints$X - floor(cpoints$X)
  ypon <- cpoints$Y - floor(cpoints$Y)
  xponl <- rep(xpon,each=nrow(thepoints))
  yponl <- rep(ypon,nrow(thepoints))
  mresults <- mapply (shiftgrid,xponl,yponl)
  #reformat to df returning results etc
  resultsdf <- as.data.frame(t(mresults))

  names(resultsdf) <- c('nofcells','rotation','xshift','yshift')
  #get the first minimum grid for results and returns
  bestgrid <- resultsdf[which.min(resultsdf$nofcells),]
  if(returnV == "E"){
    return(list(area=bestgrid$nofcells * (cellsize^2)/1000000,nocells=bestgrid$nofcells,
                rotation = rad2deg(bestgrid$rotation),
                xshift= bestgrid$xshift, yshift = bestgrid$yshift))
  }
  if(returnV == "SF"){
    bestpoly <- buildCellPolys_rxy(thepoints,cellsize,bestgrid$rotation,bestgrid$xshift,bestgrid$yshift)
    return(bestpoly)
  }

  if(returnV == "ALL"){return(resultsdf)}
  else {return(bestgrid$nofcells * (cellsize^2)/1000000)}
}

#building blocks for polygon production#
#builds polygons from points and rotation, shift in X and y
#returns polygons for ggplot2 and mapping

#' @title Build simple feature polygons from point data, rotation and shift in x and y direction
#' @description
#' Builds cell polygons (as simple features) from points and rotation, shift in X and y returns polygons for ggplot2 and mapping.
#' Generally used to plot data from AOO calculations.
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints set of points in metres i.e. c(x,y)
#' @param cellsize size of cell (length) in metres
#' @param rot rotation of the grid in radian
#' @param shiftx shift in the x direction in metres
#' @param shifty shift in the y direction in metres
#' @return Simple Feature of polygons
buildCellPolys_rxy<- function(thepoints,cellsize,rot,shiftx,shifty){
  #shift first
  testps <- cbind(thepoints$X - shiftx,thepoints$Y - shifty)
  #then rotate
  rpoints <- rotateP(testps,rot)
  testcps <- unique(floor(rpoints/cellsize))*cellsize
  colnames(testcps)<-c("x","y")
  buildCells(testcps,cellsize,-rot,shiftx,shifty,attr(thepoints,'crs'))

}

#Rotates a set of points                                  #
#Note angle in radians and only needed between 0 and 2pi for 360's
#but if using with shift you really only need 0 and pi/2
#' @title Rotates a set of points
#' @description
#' Rotates a set of point by an angle in radians. Used as part of the AOO rotation calculations.
#'
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints set of points in metres i.e. c(x,y)
#' @param angle in radians
#' @return dataframe of points
rotateP <- function(thepoints, angle){
  pointmatrix <- as.matrix(thepoints)
  rotationmatrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)),byrow = TRUE, 2, 2)
  pr <- pointmatrix %*% rotationmatrix
  pdf <- as.data.frame(pr)
  colnames(pdf) <- c("x","y")
  return(pdf)
}

#builds all corners for a square from the lower left corner, returns df id,x,y for use in ggplot
buildCellPolys <- function (llcorners,cellsize){
  mydf <- data.frame(id=integer(),x=double(),y=double())
  for (i in 1:nrow(llcorners)){
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x,llcorners[i,]$y)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x+cellsize,llcorners[i,]$y)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x+cellsize,llcorners[i,]$y+cellsize)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x,llcorners[i,]$y+cellsize)
    mydf[nrow(mydf)+1,] <- c(i,llcorners[i,]$x,llcorners[i,]$y)
  }
  return(mydf)
}

#builds all corners for a square from the lower left corner,
#rotation, shift in X and y
#returns polygons
#internal called from main scripts
buildCells <- function (llcorners, cellsize, rot=0, shiftx=0, shifty=0, crs=""){
  #build cells
  mincells <- buildCellPolys(as.data.frame(llcorners),cellsize)
  #rotate these back to original point orientation
  cells <- rotateP(mincells[, 2:3], rot)
  #shift
  cells$x <- cells$x + shiftx
  cells$y <- cells$y + shifty

  cell_list <- split(cells, f=mincells$id)
  poly_list <- lapply(cell_list, function(x) constructPolygon(x$x, x$y, crs))

  do.call(c, poly_list)
}

#calculates the number of Sub-population or number of locations from buffer method

#utilities and extra scripts
#calculates the longest axis from a set of points
#' @title Longest distance from a set of points
#' @description
#' Calculates the longest distances from a set of points
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @note Useful as a scale for cellsize and location buffers, Willis et al 2003 suggest 1/10 of this for cellsize for AOO calculations as does Rivers et al (2010) for buffer distance for sub-population or location calculations.
#' @param thepoints dataframe of points of x,y
#' @param returnV, two switches either S for simply the distance or P for a dataframe of the two furthest points
#' @return distance in metres or two points for the longest distance
longestAxis <- function (thepoints,returnV='S'){
  edgepoints <- thepoints[chull(thepoints),]
  distmax <- 0
  for (i in 1:(nrow(edgepoints)-1)){
    for (j in (i+1):nrow(edgepoints)){
      dist <- sqrt((edgepoints[i,1] - edgepoints[j,1])^2 + (edgepoints[i,2] - edgepoints[j,2])^2)
      if (dist > distmax){
        distmax <- dist
        p1 <- i
        p2 <- j
      }
    }
  }
  fpoints <- edgepoints[c(p1,p2),]
  if (returnV == "P"){return(fpoints)}
  distmax
}

#MER from a set of points#
#' calculates the MER of a set of numbers'
#' @title Minimum Enclosing Rectangle (mer)
#' @description
#' Calculates the minimum enclosing rectangle (mer) from a set of points (x,y)
#' @author Justin Moat. J.Moat@kew.org
#' @keywords internal
#' @noRd
#' @param thepoints dataframe of points ie c(x,y)
#' @return vector of 4 doubles = xmin,xmax,ymin,ymax
mer <- function(thepoints){
  xmin <- min(thepoints[1])
  xmax <- max(thepoints[1])
  ymin <- min(thepoints[2])
  ymax <- max(thepoints[2])
  return(c(xmin,xmax,ymin,ymax))
}

MER <- mer












