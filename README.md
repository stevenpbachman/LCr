
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCr

LCr: An R package to generate minimal Least Concern Red List assessments

## Overview

Generate minimal documentation for a species expected to be Least
Concern See [IUCN Red List](https://www.iucnredlist.org/). Submit to the
IUCN Red List via [SIS Connect](https://connect.iucnredlist.org/)
(registration needed)

## Installation

Not yet on [CRAN](https://CRAN.R-project.org), but you can install the
development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stevenpbachman/LCr")
```

## Usage

The workflow is broken into several steps and each step can be run
independently:

1.  `name_search_gbif` or `name_search_powo` tp check name status
    against taxonomic name backbones \#2. `occs_gbif` gather occurrence
    records from GBIF \#3. `occs_clean` clean the occurrence data \#4.
    `sis_zip` generate and zip SIS csv datafiles

Function will allow single species or a multiple species if a dataframe
is supplied

### Getting started

``` r
library(LCr)
## check a name against GBIF, Kew Names Matching Service and Plants of the World Online
name_search_gbif("Poa annua L.")
#> # A tibble: 1 × 6
#>   searchName   usageKey scientificName status   rank    confidence
#>   <chr>           <int> <chr>          <chr>    <chr>        <int>
#> 1 Poa annua L.  2704179 Poa annua L.   ACCEPTED SPECIES        100

#etc...
```
