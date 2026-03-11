# Query taxon name against GBIF backbone

Queries a taxon name against the GBIF names backbone. Simple wrapper
around the rgbif \`name_backbone_verbose\` function.

## Usage

``` r
search_name_gbif(name, match = "single")
```

## Arguments

- name:

  (character) A taxon name

- match:

  (character) Controls the output of the search. Use \`single\` to force
  a single match result that has the highest confidence or \`any\` to
  return all possible matches sorted in order of confidence

- species_rank:

  (character) Default TRUE returns matches at species level, otherwise
  FALSE returns any taxonomic rank

## Value

Returns a data frame with initial search term and matching name(s)

## Examples

``` r
search_name_gbif("Poa annua L.")
#> # A tibble: 1 × 10
#>   searchName   usageKey scientificName status   rank    confidence family  genus
#>   <chr>        <chr>    <chr>          <ord>    <chr>        <int> <chr>   <chr>
#> 1 Poa annua L. 2704179  Poa annua L.   ACCEPTED SPECIES        100 Poaceae Poa  
#> # ℹ 2 more variables: species <chr>, authorship <chr>
```
