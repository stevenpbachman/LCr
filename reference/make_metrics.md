# Generate metrics from a clean point occurrence file

Generate metrics from a clean point occurrence file

## Usage

``` r
make_metrics(
  occs,
  native_ranges = FALSE,
  keys,
  eoo_thresh = 30000,
  aoo_thresh = 3000,
  points_thresh = 75,
  regions_thresh = 5,
  recent_thresh = 50
)
```

## Arguments

- native_ranges:

  (df) WGSRPD native ranges at level 3

- eoo_thresh:

  (integer) threshold for EOO to determine Least Concern

- aoo_thresh:

  (integer) threshold for AOO to determine Least Concern

- points_thresh:

  (integer) threshold for number of points to determine Least Concern

- regions_thresh:

  (integer) threshold for number of regions to determine Least Concern

- recent_thresh:

  (integer) threshold for number of recent occurrences (\<30 yrs) to
  determine Least Concern

- occs_clean:

  (character) Cleaned GBIF occurrence file in DWCA format. See
  occs_clean() function.

## Value

Returns a dataframe with species level metrics
