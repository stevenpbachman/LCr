
library(readr)

devtools::document()
devtools::load_all()
devtools::check()

first_name <- "Steven"
second_name <- "Bachman"
email = "s.bachman@kew.org"
institution = "Royal Botanic Gardens, Kew"

BVI_unassessed_taxa <- read_csv("G:/Users/sb42kg/RapidLC_supplementary/BVI_unassessed_taxa.csv")
Cucurbitaceae <- dplyr::filter(BVI_unassessed_taxa, Family == "Cucurbitaceae")


lc_species <-
  data.frame(species = c(
    "Crabbea acaulis",
    "Crabbea cirsioides",
    "Crabbea nana",
    "Crabbea velutina"
  ))
print(lc_species)

test_names <- name_search_gbif(lc_species$species)
lc_keys <-
  get_name_keys(
    lc_species,
    name_col = "species",
    tax_status = "any",
    match = "any"
  )

lc_sis_occs <-
  make_LC_points(
    lc_keys,
    first_name = "Steven",
    second_name = "Bachman",
    institution = "Royal Botanic Gardens, Kew",
    range_check = TRUE
  )

lc_sis_occs$

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

make_zip(allcsvfiles)

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

