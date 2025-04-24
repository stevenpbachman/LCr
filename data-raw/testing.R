
# LCr demo

########################
devtools::document()
devtools::install()   # Reinstall package
devtools::load_all()
#devtools::check()
########################

# to do
# bio geographic realm

# map explanation - how was map generated <- no field available
# check higher level geog e.g. great britain
# use and trade - tick no use information < done
# remove match = single option.
# wcvp name matching - single field with name and author string combined
# works well for GBIF, but WCVP better if name and author separated

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
    #"Onobrychis aliacmonia Rech.f."
    #"Suaeda tampicensis",
    #"Celosia nitida"
    #"Russula",
    "Lactarius decipiens"
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
    #"lntsia bijuga  Colebr.) Kuntze"
  ))
print(lc_species)

lc_species <-
  data.frame(sp = c(
    "Crabbea acaulis",
    "Crabbea velutina"
  ))

SPECIES_LIST_23_07_24 <- read_csv("C:/Users/sb42kg/Downloads/SPECIES_LIST_23-07-24.csv")

# Cecropia schreberiana subsp antillarum 125
# Chamaecrista glandulosa var. swartzii 140
# Croton astroites 203

lc_species <- SPECIES_LIST_23_07_24

df <- SPECIES_LIST_23_07_24 %>%
  dplyr::filter(SPECIES == "Croton astroites")

name_column = "SPECIES"
name = df$SPECIES

test <- name_search_gbif(name)

# get the keys
lc_keys <-
  LCr::get_name_keys(
    lc_species,
    name_column = "sp",
    kingdom = "fungi")


lc_keys

test_taxonomy <- sis_taxonomy(lc_keys$GBIF_usageKey, family = lc_keys$GBIF_family)

###########################
# get the GBIF occurrences
lc_occs <- get_gbif_occs(lc_keys)
occs_only <- lc_occs$points

# get occs
lc_sis_occs <-
  make_LC_points(
    keys_df = lc_keys,
    first_name = "Steven",
    second_name = "Bachman",
    institution = "Royal Botanic Gardens, Kew",
    range_check = FALSE # for fungi, so that you don't try and check POWO ranges
  )

# occs
Lactarius_occs <- lc_sis_occs$points

#habitats <- Lactarius_occs %>% group_by(dist_comm) %>% count()

# for fungi - get the native ranges
native_ranges <- get_occs_range(Lactarius_occs)

lc_sis_files <- LCr::make_sis_csvs(unique_id = lc_keys$GBIF_usageKey,
                                   #wcvp_ipni_id = lc_keys_bolets$wcvp_ipni_id,
                                   first_name = "Steven",
                                   second_name = "Bachman",
                                   email = "s.bachman@kew.org",
                                   institution = "Royal Botanic Gardens, Kew",
                                   family = lc_keys$GBIF_family,
                                   genus = lc_keys$GBIF_genus,
                                   species = lc_keys$GBIF_species,
                                   gbif_ref = lc_sis_occs$citation,
                                   taxonomicAuthority = lc_keys$GBIF_authorship,
                                   kingdom = "fungi",
                                   occs = Lactarius_occs,
                                   native_ranges = native_ranges
                                   #native_ranges = lc_sis_occs$native_ranges #only for plants
)

allfields <- sis_allfields(unique_id = lc_keys$GBIF_usageKey, occs = Lactarius_occs)

assessments <- sis_assessments(unique_id = lc_keys$GBIF_usageKey, native_ranges = native_ranges)

assessments <- lc_sis_files$assessments
allfields <- lc_sis_files$allfields

devtools::document()  # Regenerates NAMESPACE and documentation
devtools::install()   # Reinstalls the package
library(LCr)         # Loads the package

?make_elevation


# now clean the occs - no native range filter
cleaned_occs <- clean_occs(lc_occs$points) # optional native range clean

# now make the SIS file
lc_points <- make_sis_occs(cleaned_occs, first_name = first_name,
                          second_name = second_name, institution = institution)
#########################

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







library(LCr)

# some parameters needed for the point and csv files
first_name = "Elloise"
second_name = "Budd"
email = "e.budd2@kew.org"
institution = "RBG Kew"

##### Test download of 3 Endemic BVI species #####

# species list
sp<- data.frame(SPECIES = c("Vachellia anegadensis", "Metastelma anegadense", "Pitcairnia jareckii"))

# check names against GBIF and WCVP name backbones
sp_keys <- get_name_keys(df = sp, name_column = "SPECIES")

# generate a point file according to IUCN standards
sp_occs <-  make_LC_points(keys_df = sp_keys, range_check = TRUE)
sp_occsdf<- sp_occs$points
native_ranges <- sp_occs$native_ranges

# generate the csv files
sp_csvs <- make_sis_csvs(unique_id = sp_keys$wcvp_ipni_id,
                         wcvp_ipni_id = sp_keys$wcvp_ipni_id,
                         first_name = first_name,
                         second_name = second_name,
                         email = email,
                         institution = institution,
                         gbif_ref = sp_occs$citation,
                         powo_ref = FALSE,
                         family = sp_keys$GBIF_family,
                         genus = sp_keys$GBIF_genus,
                         species = sp_keys$GBIF_species,
                         taxonomicAuthority = sp_keys$GBIF_taxonomicAuthority,
                         kingdom = "plantae",
                         native_ranges = native_ranges)



make_biorealms <- function(native_ranges, TDWG_realms) {
  biogeographic_realm <- native_ranges %>%
    left_join(TDWG_realms, by = c("LEVEL3_COD" = "LEVEL3_COD")) %>%  # Match regions to realms
    group_by(internal_taxon_id) %>%
    summarise(BiogeographicRealm.realm = gsub(", ", " | ", paste(unique(REALM), collapse = ", ")),
              .groups = "drop")

  return(biogeographic_realm)
}







