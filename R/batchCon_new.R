
###########################################################
#Simple function to process multiple species #
###########################################################
#' @title Batch process, preliminary conservation assessments
#' @description
#' Combines the main of routines in rCAT to process multiple species for AOO, EOO etc.
#' @author Justin Moat. J.Moat@kew.org
#' @note Has a switch to either project all data as a whole or each taxa separately. We would suggest you use this switch if data is all from a similar area (i.e. all from one continent/country/region)
#' @details This function expects a list of taxa and latitudes and longitudes.ie\cr
#' \tabular{lll}{
#' species_w \tab 85.388000  \tab   84.33100\cr
#' species_w \tab -45.467000   \tab  88.41500\cr
#' species_w \tab -34.339000 \tab -149.52600\cr
#' species_x \tab -29.620000   \tab  79.11900\cr
#' species_x \tab 33.409000  \tab  -33.94700\cr
#' species_x \tab 64.692000 \tab -149.18900\cr
#' species_y \tab 2.308000 \tab -140.21900\cr
#' species_y \tab 41.452000  \tab -3.65600\cr
#' species_y \tab -30.475000 \tab -129.99600\cr}
#' etc
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
#'
#'
#'

batchCon_new <- function(taxa,long,lat,project2gether=TRUE,cellsize=2000,
                     aooMin=FALSE,it=1296, returnV='S'){
  if (returnV == "SF") {
    project2gether = TRUE
  }

  points <- data.frame(lat, long)

  if (project2gether) {
    points <- rCAT::simProjWiz(points)
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

    taxa_names <- names(split_points)

    results <- st_sf(
      taxon=rep(unique(taxa_names), 3),
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
    eoo_areas <- lapply(split_points, rCAT::eoo)
    #eoo_ratings <- lapply(eoo_areas, rCAT::ratingEoo)
    # catch when eoo area is NA
    eoo_ratings <- lapply(eoo_areas, function(x) {
      if (is.na(x)) {
        "DD"
      } else {
        rCAT::ratingEoo(x)
      }
    })

    aoo_areas <- lapply(split_points, function(p) rCAT::aoo(p, cellsize))

    if (aooMin) {
      min_aoo_areas <- lapply(split_points, function(p) aooFixedRotation(p, cellsize, it))
      #aoo_ratings <- lapply(min_aoo_areas, ratingAoo)
      aoo_ratings <- lapply(aoo_areas, function(x) {
        if (is.na(x)) {
          "DD"
        } else {
          rCAT::ratingAoo(x)
        }
      })

    } else {
      #aoo_ratings <- lapply(aoo_areas, rCAT::ratingAoo)
      aoo_ratings <- lapply(aoo_areas, function(x) {
        if (is.na(x)) {
          "DD"
        } else {
          rCAT::ratingAoo(x)
        }
      })
    }

    taxa_names <- names(split_points)

    results <- data.frame(
      taxon=unique(taxa_names),
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
