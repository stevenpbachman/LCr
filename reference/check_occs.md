# Checks and flags GBIF occurrence data based on common coordinate issues

Checks and flags GBIF occurrence data based on common coordinate issues

## Usage

``` r
check_occs(gbif_occs, native_ranges = NULL, buffer = 1000)
```

## Arguments

- gbif_occs:

  (data frame) GBIF occurrence file in DWCA format

- native_ranges:

  (data frame) Optional native ranges from \`get_native_range()\`

## Value

A list containing: - checked_data: original data with error flags -
summary: summary of error counts
