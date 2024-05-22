
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
    "Russula amethystina",
    "Lactarius decipiens",
    "Poa annua"
  ))
print(lc_species)

# get the keys
lc_keys <-
  get_name_keys(
    lc_species,
    name_column = "sp",
    tax_status = "accepted",
    match = "single",
    kingdom = "fungi" # note this needs to change to remove the WCVP name check
  )

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
                              gbif_ref = lc_points$citation,
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
