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


#### PLANTS ####

# get the full WCVP checklist
all_wcvp <- rWCVP::wcvp_checklist()

# pull just the families
wcvp_fams <- data.frame(family_name = unique(all_wcvp$family))
wcvp_fams$familyName = toupper(wcvp_fams$family_name)

wcvp_version <- rWCVPdata::wcvp_version()

# get the iucn plants taxonomy latest
taxonomy_plants_2023_1 <- read.csv("data-raw/taxonomy_plants_2023_1.csv")

rl_fams <- taxonomy_plants_2023_1 %>%
  dplyr::select(kingdomName, phylumName, className, orderName, familyName) %>%
  dplyr::filter(phylumName == "TRACHEOPHYTA") %>%
  dplyr::distinct(familyName, .keep_all = TRUE)

# join to compare
joined <- dplyr::left_join(wcvp_fams,rl_fams, by = "familyName")

# check for any that don't match - manually sort these out - might need export/import
joined_nomatch <- joined %>% dplyr::filter(is.na(kingdomName))

write.csv(joined, "joined.csv", row.names = FALSE, na="")
joined_clean <- read.csv("data-raw/joined.csv")
joined_clean <- joined_clean %>% rename_at('family_name', ~'family')

# add versions

iucn_taxonomy <- joined_clean

#### FUNGI ####
taxonomy_fungi_2023_1 <- read.csv("data-raw/taxonomy_fungi_2023_1.csv")

rl_fung_fams <- taxonomy_fungi_2023_1 %>%
  dplyr::select(kingdomName, phylumName, className, orderName, familyName) %>%
  dplyr::filter(kingdomName == "FUNGI") %>%
  dplyr::distinct(familyName, .keep_all = TRUE)

rl_fung_fams$family <- rl_fung_fams$familyName

iucn_taxonomy <- dplyr::bind_rows(iucn_taxonomy, rl_fung_fams)
iucn_taxonomy$family <- stringr::str_to_title(iucn_taxonomy$family)

# finally create the IUCN taxonmy file - it will save as rds. in /data folder
usethis::use_data(iucn_taxonomy, overwrite = TRUE)
devtools::document()
