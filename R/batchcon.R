
batchCon <- function(taxa,long,lat,project2gether=TRUE,cellsize=2000,
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
    eoo_areas <- lapply(split_points, rCAT::eoo)
    # catch when eoo_area is NA and rate as DD
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
      # catch when aoo_area is NA and rate as DD
      aoo_ratings <- lapply(aoo_areas, function(x) {
        if (is.na(x)) {
          "DD"
        } else {
          rCAT::ratingAoo(x)
        }
      })
    } else {
      # catch when eoo_area is NA and rate as DD
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
