# Download or search georeferenced occurrence data using GBIF name identifier

Download or search georeferenced occurrence data using GBIF name
identifier

## Usage

``` r
get_gbif_occs(keys_df, mode = "search")
```

## Arguments

- keys_df:

  (data frame) Derived from \[\`get_name_keys()\`\] function. Must
  include at least GBIF_usageKey to obtain GBIF occurrences

- mode:

  (character) Either `"search"` for a quick exploratory search or
  `"download"` for a full citable download. Default is `"search"`.

## Value

(list) 'points' = the GBIF occurrence points, 'citation' = the GBIF
citation (only available in `"download"` mode, `NULL` in search mode)

## Details

In `"download"` mode, initiates a full occurrence download in DWCA
format using \[rgbif::occ_download()\], which generates a citable DOI.
In `"search"` mode, uses \[rgbif::occ_search()\] for a faster
exploratory search without DOI generation. Search mode is recommended
for initial data exploration; use download mode for final assessments.
