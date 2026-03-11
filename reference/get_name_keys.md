# Get accepted GBIF and POWO identifiers (keys) for taxon names to support data downloads

Get accepted GBIF and POWO identifiers (keys) for taxon names to support
data downloads

## Usage

``` r
get_name_keys(df, name_column, match = "single", kingdom = "plantae")
```

## Arguments

- df:

  character) Data frame with taxon names

- match:

  (character) Controls the output of the search. Use \`single\` to force
  a single match result that has the highest confidence or \`any\` to
  return all possible matches sorted in order of confidence

- kingdom:

  (character) Defaults to \`plantae\` and carries out match to POWO. If
  set as \`fungi\` will only carry out GBIF search

- name_col:

  (character) Column for taxon names. Include taxonomic authority for
  better matching

## Value

Returns a data frame with accepted GBIF and POWO identifiers

## Details

Designed for batch processing. Default setting (match = "single",
kingdom = "plantae") returns an accepted name with the best single match
against GBIF and POWO name backbones (GBIF only for kingdom = "fungi").
Final list may return fewer names if there are discrepancies e.g.
accepted in GBIF, but not in POWO. Output data frame includes GBIF
'usageKey' that can be used with \[\`get_gbif_occs()\`\] to get
occurrences from GBIF, and 'wcvp_ipni_id' that can be used with
\[\`powo_range()\`\] to get native ranges. To see a wider range of
plausible matches adjust 'match' to 'any'.
