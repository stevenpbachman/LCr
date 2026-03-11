# Clean a keys output to ensure only accepted species from GBIF and WCVP are being analysed

Clean a keys output to ensure only accepted species from GBIF and WCVP
are being analysed

## Usage

``` r
clean_keys(keys)
```

## Arguments

- keys:

  data frame of GBIF and WCVP identifier keys

## Value

Returns a data frame with cleaned GBIF and WCVP keys

## Details

Problematic records e.g. not accepted in either GBIF or WCVP, or not at
species level, are removed. Duplicate records are reported, but not
removed.
