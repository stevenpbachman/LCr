## code to prepare `tdwg2iucn` dataset goes here

library(tidyverse)
library(usethis)
library(readr)
library(rWCVP)
library(rWCVPdata)

# We need to convert from WGSRPD to IUCN countries
# this code generates a translation table

# input is tdwg2iucn.csv
# edit this if changes are needed and resave for use in the package

##########
tdwg2iucn <- read_csv("data-raw/tdwg2iucn.csv")


# finally create the IUCN taxonmy file - it will save as rds. in /data folder
usethis::use_data(tdwg2iucn, overwrite = TRUE)
devtools::document()


