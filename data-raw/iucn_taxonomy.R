## code to prepare `iucn_taxonomy` dataset goes here

library(tidyverse)
library(usethis)
library(readr)
library(rWCVP)
library(rWCVPdata)

# the IUCN taxonomy table may change and this could break the sis_taxonomy function
# PLANTS
# read in the latest WCVP version
# read in the latest Red List taxonomy version for Kingdom = Plants (via API?)

# FUNGI
# read in the latest index fungorum families list
# read in the latest Red List taxonomy version for Kingdom = Fungi (via API?)

##########
# new method - matching to GBIF families
##########
gbif_plantae_families <- read_csv("data-raw/gbif_plantae_families.csv")
gbif_fungi_families <- read_csv("data-raw/gbif_fungi_families.csv")
taxonomy_plants_2023_1 <- read.csv("data-raw/taxonomy_plants_2023_1.csv")
taxonomy_fungi_2023_1 <- read_csv("data-raw/taxonomy_fungi_2023_1.csv")

# bind plants and fungi and convert to upper case for match later
gbif_tax <- dplyr::bind_rows(gbif_plantae_families, gbif_fungi_families)
gbif_tax$family = toupper(gbif_tax$family)

# summarise rl tax table - unique values
rl_tax <- dplyr::bind_rows(taxonomy_plants_2023_1, taxonomy_fungi_2023_1) %>%
  dplyr::select(kingdomName, phylumName, className, orderName, familyName) %>%
  dplyr::distinct()

# join rl tax to gbif for exact match
iucn_taxonomy <- dplyr::left_join(gbif_tax, rl_tax, by = c("family" = "familyName"))
iucn_taxonomy$family <- stringr::str_to_title(iucn_taxonomy$family)
iucn_taxonomy <- iucn_taxonomy %>% dplyr::rename("kingdom" = "kingdomName",
                                                 "phylum" = "phylumName",
                                                 "classname" = "className",
                                                 "ordername" = "orderName")

# finally create the IUCN taxonmy file - it will save as rds. in /data folder
usethis::use_data(iucn_taxonomy, overwrite = TRUE)
devtools::document()

# #### PLANTS ####
#
# # get the full WCVP checklist
# all_wcvp <- rWCVP::wcvp_checklist()
#
# # pull just the families
# wcvp_fams <- data.frame(family_name = unique(all_wcvp$family))
# wcvp_fams$familyName = toupper(wcvp_fams$family_name)
#
# wcvp_version <- rWCVPdata::wcvp_version()
#
# # get the iucn plants taxonomy latest
# taxonomy_plants_2023_1 <- read.csv("data-raw/taxonomy_plants_2023_1.csv")
#
# rl_fams <- taxonomy_plants_2023_1 %>%
#   dplyr::select(kingdomName, phylumName, className, orderName, familyName) %>%
#   dplyr::filter(phylumName == "TRACHEOPHYTA") %>%
#   dplyr::distinct(familyName, .keep_all = TRUE)
#
# # join to compare
# joined <- dplyr::left_join(wcvp_fams,rl_fams, by = "familyName")
#
# # check for any that don't match - manually sort these out - might need export/import
# joined_nomatch <- joined %>% dplyr::filter(is.na(kingdomName))
#
# write.csv(joined, "joined.csv", row.names = FALSE, na="")
# joined_clean <- read.csv("data-raw/joined.csv")
# joined_clean <- joined_clean %>% rename_at('family_name', ~'family')
#
# # add versions
#
# iucn_taxonomy <- joined_clean
#
# #### FUNGI ####
# taxonomy_fungi_2023_1 <- read.csv("data-raw/taxonomy_fungi_2023_1.csv")
#
# rl_fung_fams <- taxonomy_fungi_2023_1 %>%
#   dplyr::select(kingdomName, phylumName, className, orderName, familyName) %>%
#   dplyr::filter(kingdomName == "FUNGI") %>%
#   dplyr::distinct(familyName, .keep_all = TRUE)
#
# rl_fung_fams$family <- rl_fung_fams$familyName
#
# iucn_taxonomy <- dplyr::bind_rows(iucn_taxonomy, rl_fung_fams)
# iucn_taxonomy$family <- stringr::str_to_title(iucn_taxonomy$family)

