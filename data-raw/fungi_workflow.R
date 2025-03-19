
# LCr workflow example for fungi

########################
devtools::document()
devtools::load_all()
devtools::check()
########################

library(readr)
library(dplyr)
library(LCr)

first_name <- "Steven"
second_name <- "Bachman"
email = "s.bachman@kew.org"
institution = "Royal Botanic Gardens, Kew"

Confident_LC_fungi <- read_csv("data-raw/Confident LC fungi 1.3.24_VS.csv")

# species list
lc_species <-
  data.frame(sp = c(
    #"Cortinarius ohlone",
    #"Cortinarius caesiobrunneus"
    #"Candelaria fibrosa"
    # "Asplenium nidus  L.",
    "Boerhavia repens L. ",
    "Canavalia cathartica"
    #"Cassytha filiformis L."
    # "Dactyloctenium ctenoides  (Steud.) Bosser",
    # "Euphorbia stoddartii  Fosberg",
    # "Ipomoea violacea L.",
    # "test test",
    # "Lagrezia micrantha (Bak.)Schinz."
    #"lntsia bijuga  Colebr.) Kuntze",
#   "Lepturus repens G.Forst.) R.Br.",
#     "Triumfetta procumbens Forst.",
#"lpomoea pes-caprae  (L.) R.Br.",
#"Pisonia grandis R.Br.",
#"Pteris tripartita  Sw.",
#"Scaevola taccada  (Gaertn.) Roxb."
# "Sida pusilla Cav.",
# "Stenotaphrum micranthum (Desv.) C.E. Hubb.",
#"Thelypteris opulentum  (Kaulf.) J.P.Roux"
# "Triumfetta procumbens Forst."
  ))
print(lc_species)

# get the keys
lc_keys <-
  get_name_keys(
    lc_species,
    Confident_LC_fungi,
    name_column = "sp",
#    name_column = "acceptedNameAuthor",
    match = "any",
    kingdom = "fungi" # setting to fungi removes the WCVP check
  )

# filter out the not accepted if you like
#lc_keys <- lc_keys %>% dplyr::filter(GBIF_status == "ACCEPTED")

#lc_keys <- lc_keys %>% filter(GBIF_family == "Russulaceae")

#### points - long version

lc_gbif <- get_gbif_occs(lc_keys)
lc_gbif_clean <-clean_occs(lc_gbif$points)
lc_sis_points <-make_sis_occs(lc_gbif_clean,
                              first_name = first_name,
                              second_name = second_name,
                            institution = institution)

gbif_ref <- lc_gbif$citation


# get gbif ref
####

# make the LC points csv file
lc_points <- make_LC_points(lc_keys, range_check = FALSE, first_name = first_name,
                            second_name = second_name, institution = institution )

# write the spatial data to file - needed for IUCN submission
write_csv(lc_sis_points, "lc_points_Russulaceae.csv")

# for fungi - get the native ranges
native_ranges <- get_occs_range(lc_sis_points)

# get the SIS connect files
sis_occs <- make_sis_csvs(unique_id = lc_keys$GBIF_usageKey,
                              #wcvp_ipni_id = lc_keys$wcvp_ipni_id, # remove this for fungi
                              first_name = "Steven",
                              second_name = "Bachman",
                              email = "s.bachman@kew.org",
                              institution = "Royal Botanic Gardens, Kew",
                              family = lc_keys$GBIF_family,
                              genus = lc_keys$GBIF_genus,
                              species = lc_keys$GBIF_species,
                              taxonomicAuthority = lc_keys$GBIF_taxonomicAuthority,
                              gbif_ref = gbif_ref,#lc_points$citation,
                              #powo_ref = FALSE,
                              #native_ranges = native_ranges, # remove this for fungi
                              kingdom = "fungi" # set to fungi
)

# make the zip file
make_zip(sis_occs)

write_csv(lc_points$points, "lc_points_Russulaceae.csv")

test_stry <- sis_occs$countries

country_table <- dplyr::left_join(test_powo_range, tdwg2iucn, by = c("LEVEL3_COD" = "Level.3.code"),relationship = "many-to-many")

test_sis_countries <- sis_countries(unique_id = lc_keys$GBIF_usageKey)

test_powo_range <- get_native_range(lc_keys)

#"locationTree"  "featureId"     "LEVEL3_COD"    "tdwgLevel"     "establishment" "LEVEL3_NAM"    "POWO_ID"

#make a map
themap <- leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(
    data = lc_points$points,
    lng = lc_points$points$dec_long,
    lat = lc_points$points$dec_lat,
    radius = 3,
    color = "red",
    group = "clean occurrences"
  ) %>%

  leaflet::addLayersControl(
    overlayGroups = c("clean occurrences"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
