# Convert cleaned gbif DWCA structure to IUCN SIS format

Convert cleaned gbif DWCA structure to IUCN SIS format

## Usage

``` r
make_sis_occs(occs_clean, first_name = "", second_name = "", institution = "")
```

## Arguments

- occs_clean:

  (character) Cleaned GBIF occurrence file in DWCA format. See
  occs_clean() function.

- first_name:

  (character) First name of the assessor

- second_name:

  (character) Second name of the assessor

- institution:

  (character) Institution/affiliation of the assessor

## Value

An dataframe of occurence points aligned with IUCN Red List spatial data
standards
