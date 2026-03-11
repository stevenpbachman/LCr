# Download georeferenced occurrence data using GBIF name identifier

Download georeferenced occurrence data using GBIF name identifier

## Usage

``` r
get_gbif_occs(keys_df)
```

## Arguments

- keys_df:

  (data frame) Derived from \[\`get_name_keys()\`\] function. Must
  include at least GBIF_usageKey to obtain GBIF occurrences

## Value

(list) 'Points' = the downloaded GBIF points, 'Citation' = the GBIF
citation to be used in references

## Details

Initiates an occurrence download in DWCA format using
\[rgbif::occ_download()\]. If \`keys_df\`contains multiple GBIF_usageKey
values, they will be combined in a single output file.
