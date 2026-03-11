# Display a leaflet map to show the distribution of occurrence data for each species Batch version that iterates through species

Display a leaflet map to show the distribution of occurrence data for
each species Batch version that iterates through species

## Usage

``` r
map_species_batch(
  data,
  species_range = NULL,
  show_flags = TRUE,
  save_map = TRUE
)
```

## Arguments

- data:

  A data frame containing occurrence data after cleaning flags added

- show_flags:

  Set to false if you just want to see the points map without flags

- save_map:

  Set to true if you want to save the maps as html files in the working
  directory

- native_ranges:

  A data frame containing occurrence data show_flags

## Value

Interactive leaflet map
