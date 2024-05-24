
# LCr workflow example for fungi

library(readr)
library(dplyr)
library(LCr)

first_name <- "Steven"
second_name <- "Bachman"
email = "s.bachman@kew.org"
institution = "Royal Botanic Gardens, Kew"

# species list
lc_species <-
  data.frame(sp = c(
    "Asplenium longissimum  Blume",
    "Asplenium macrophyllum  Sw.",
    "Asplenium nidus  L.",
    "Boerhavia repens L. ",
    "Canavalia cathartica  Thouars",
    "Cassytha filiformis L.",
    "Dactyloctenium ctenoides  (Steud.) Bosser",
    "Euphorbia stoddartii  Fosberg",
    "Ipomoea violacea L.",
    "test test",
    "Lagrezia micrantha (Bak.)Schinz."
    #"lntsia bijuga  Colebr.) Kuntze",
#     "Lepturus repens G.Forst.) R.Br.",
#     "Triumfetta procumbens Forst.",
# #"lpomoea pes-caprae  (L.) R.Br.",
# #"Pisonia grandis R.Br.",
# "Pteris tripartita  Sw.",
# "Scaevola taccada  (Gaertn.) Roxb.",
# "Sida pusilla Cav.",
# "Stenotaphrum micranthum (Desv.) C.E. Hubb.",
# #"Thelypteris opulentum  (Kaulf.) J.P.Roux",
# "Triumfetta procumbens Forst."
  ))
print(lc_species)

# gbif_names_out <- purrr::map_dfr(chagos_names$name_in,
#                                  name_search_gbif,
#                                  match = match,
#                                  gbif_tax_stat = "any")

# get the keys
lc_keys <-
  get_name_keys(
    #lc_species,
    Confident.LC.fungi.1.3.24_VS,
    #name_column = "sp",
    name_column = "acceptedNameAuthor",
    tax_status = "any",
    match = "single",
    kingdom = "fungi" # setting to fungi removes the WCVP check
  )

# filter out the not accepted if you like
lc_keys <- lc_keys %>% dplyr::filter(GBIF_status == "ACCEPTED")

# make the LC points csv file
lc_points <- make_LC_points(lc_keys, range_check = FALSE)

# write the spatial data to file - needed for IUCN submission
write_csv(lc_points$points, "lc_points_fungi.csv")

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
                              #gbif_ref = lc_points$citation,
                              #native_ranges = lc_points$native_ranges, # remove this for fungi
                              kingdom = "fungi" # set to fungi
)

# make the zip file
make_zip(sis_occs)

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
