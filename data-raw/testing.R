
# LCr demo

########################
devtools::document()
devtools::load_all()
devtools::check()
########################

library(readr)
library(dplyr)
library(LCr)
library(rgbif)

first_name <- "Steven"
second_name <- "Bachman"
email = "s.bachman@kew.org"
institution = "Royal Botanic Gardens, Kew"

# species list
lc_species <-
  data.frame(sp = c(
    #"Suaeda tampicensis",
    #"Celosia nitida"
    #"Russula amethystina",
    #"Lactarius decipiens",
    # "Asplenium longissimum  Blume",
    # "Asplenium macrophyllum  Sw.",
    # "Asplenium nidus  L.",
    # "Boerhavia repens L.",
    # "Canavalia cathartica  Thouars",
    # "Cassytha filiformis L.",
    # "Dactyloctenium ctenoides  (Steud.) Bosser",
    # "Euphorbia stoddartii  Fosberg",
    # "Ipomoea violacea L.",
    # "Lagrezia micrantha (Bak.)Schinz.",
    # "Lepturus repens G.Forst.) R.Br."
    "lntsia bijuga  Colebr.) Kuntze"
  ))
print(lc_species)

# get the keys
lc_keys <-
  get_name_keys(
    lc_species,
    name_column = "sp",
    tax_status = "accepted",
    match = "single",
    kingdom = "fungi"
  )

lc_keys

test_taxonomy <- sis_taxonomy(lc_keys$GBIF_usageKey, family = lc_keys$GBIF_family)

# ###########################
# # get the GBIF occurrences
# lc_occs <- get_gbif_occs(lc_keys)
#
# # now clean the occs - no native range filter
# cleaned_occs <- clean_occs(lc_occs$points) # optional native range clean
#
# # now make the SIS file
#lc_points <- make_sis_occs(cleaned_occs, first_name = first_name,
#                           second_name = second_name, institution = institution)
# #########################

lc_points <- make_LC_points(lc_keys, range_check = FALSE)

# write the spatial data to file - needed for IUCN submission
write_csv(lc_points$points, "lc_points_fungi.csv")

# get the SIS connect files
sis_occs <- make_sis_csvs(unique_id = lc_keys$GBIF_usageKey,
                              wcvp_ipni_id = lc_keys$wcvp_ipni_id,
                              first_name = "Steven",
                              second_name = "Bachman",
                              email = "s.bachman@kew.org",
                              institution = "Royal Botanic Gardens, Kew",
                              gbif_ref = lc_points$citation,
                              native_ranges = lc_points$native_ranges,
                              kingdom = "fungi"
)

# make the zip file
make_zip(fung_sis_occs)






occs_range <- get_occs_range(fung_sis_occs)
## see the package workflow presentation

#fung_occs <- make_LC_points(keys_df = lc_keys)

fung_occs_clean <- fung_occs$points
saveRDS(fung_occs_clean, file = "G:/Users/sb42kg/LCr/data-raw/fung_occs_clean.rds")

library(sf)
test_iso <- get_iso_occs(fung_occs_clean)

library(LCr)

str(tdwg_level3)

fung_with_WCVP <- left_join(fung_occs_clean, test_iso, by = c("dec_lat" = "latitude", "dec_long" = "longitude"))


lc_native <- get_native_range(lc_keys)

cleaned_occs <- clean_occs(raw_occurrence)

lc_metrics <- make_metrics(cleaned_occs)


lc_sis_occs <-
  make_LC_points(
    lc_keys,
    first_name = "Steven",
    second_name = "Bachman",
    institution = "Royal Botanic Gardens, Kew",
    range_check = TRUE
  )

lc_sis_files <- make_sis_csvs(unique_id = lc_keys$wcvp_ipni_id,
                              wcvp_ipni_id = lc_keys$wcvp_ipni_id,
                              first_name = "Steven",
                              second_name = "Bachman",
                              email = "s.bachman@kew.org",
                              institution = "Royal Botanic Gardens, Kew",
                              gbif_ref = lc_sis_occs$citation,
                              native_ranges = lc_sis_occs$native_ranges
)


test <- sis_countries(unique_id = lc_keys$wcvp_ipni_id, wcvp_ipni_id = lc_keys$wcvp_ipni_id, native_ranges = lc_sis_occs$native_ranges)

# fungi test
keys <- get_name_keys(validated_sp, "acceptedNameAuthor", kingdom = "")
#ranges <- get_native_range(keys, "wcvp_ipni_id")
occs <- make_LC_points(keys_df = keys, first_name = first_name,
                       second_name = second_name, institution = institution,
                       range_check = FALSE)

gbif_ref <- occs$citation

allcsvfiles <- sis_all_csvs(unique_id = keys$wcvp_ipni_id, first_name = first_name, second_name = second_name,
                            email = email, institution = institution, gbif_ref = occs$citation,
                            wcvp_ipni_id = keys$wcvp_ipni_id, native_ranges = ranges)

make_zip(lc_sis_files)

install.packages("occupancyTuts")
install.packages('RPresence', repo = 'https://eesc.usgs.gov/mbr/mbrCRAN')

learnr::run_tutorial(
  name = "single_season",
  package = "occupancyTuts"
)

lc_sis_occs <-
  make_LC_points(
    lc_keys,
    first_name = "Steven",
    second_name = "Bachman",
    institution = "Royal Botanic Gardens, Kew",
    range_check = TRUE
  )

lc_sis_files <- make_sis_csvs(unique_id = lc_keys$wcvp_ipni_id,
                              wcvp_ipni_id = lc_keys$wcvp_ipni_id,
                              first_name = "Steven",
                              second_name = "Bachman",
                              email = "s.bachman@kew.org",
                              institution = "Royal Botanic Gardens, Kew",
                              gbif_ref = lc_sis_occs$citation,
                              native_ranges = lc_sis_occs$native_ranges
                              )
#make a map


themap <- leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(
    data = occs,
    lng = occs_ll$dec_long,
    lat = occs_ll$dec_lat,
    radius = 3,
    color = "red",
    group = "raw occurrences"
  ) %>%
  leaflet::addCircleMarkers(
    data = cleaned_occs,
    lng = cleaned_occs$dec_long,
    lat = cleaned_occs$dec_lat,
    radius = 3,
    color = "green",
    group = "clean occurrences"
  ) %>%
  leaflet::addLayersControl(
    overlayGroups = c("raw occurrences", "clean occurrences"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )


x <- runif (20,0,10)
y <- runif (20,0,10)
df <- data.frame(x,y)
df$sp <- "sp1"

x <- runif (20,0,10)
y <- runif (20,0,10)
df2 <- data.frame(x,y)
df2$sp <- "sp2"

df <- bind_rows(df, df2)

library(rCAT)
test <- batchCon(test_multi_sp$Binomial, test_multi_sp$longitude, test_multi_sp$latitude)


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
      row.names = NULL,  # Set row names to NULL
      taxon=unique(taxa),
      NOP=do.call(c, n_points),
      EOOkm2=do.call(c, eoo_areas),
      AOOkm=do.call(c, aoo_areas),
      EOOcat=do.call(c, eoo_ratings),
      AOOcat=do.call(c, aoo_ratings),
      cellwidth=cellsize,
      proj_metadata=proj_strings
    )

    if (aooMin) {
      min_aoo_areas <- lapply(split_points, function(p) aooFixedRotation(p, cellsize, it))
      results$MinAOO <- do.call(c, min_aoo_areas)
    }

  }

  results
}

