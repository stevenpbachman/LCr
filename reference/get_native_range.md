# Get native ranges for taxa

Get native ranges for taxa

## Usage

``` r
get_native_range(keys, names, distributions)
```

## Arguments

- keys:

  (data frame) Contain identifier for the taxon e.g. derived from
  \[\`get_name_keys()\`\]

- names:

  (data frame) A data frame of taxonomic names from WCVP

- distributions:

  (data frame) A data frame of distributions from WCVP

## Value

(data frame) A list of 'botanical countries' (World Geographic Scheme
for Recording Plant Distributions) where a taxon occurs.

## Details

Currently one option to get native ranges from (Plants of the World
Online) using WCVP identifier. Other options to be added later e.g. GIFT
