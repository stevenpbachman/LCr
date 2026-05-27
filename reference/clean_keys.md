# Clean a keys output to ensure only accepted species from GBIF and/or WCVP are being analysed

Clean a keys output to ensure only accepted species from GBIF and/or
WCVP are being analysed

## Usage

``` r
clean_keys(keys)
```

## Arguments

- keys:

  data frame of GBIF and/or WCVP identifier keys, as produced by
  [`get_name_keys()`](https://stevenpbachman.github.io/LCr/reference/get_name_keys.md).
  Must have a `sources` attribute indicating which sources are present
  (e.g. `"GBIF"`, `"WCVP"`, or both).

## Value

Returns a data frame with cleaned keys, retaining only records that are
accepted and at species rank in all present sources.

## Details

Problematic records — e.g. those not accepted in GBIF or WCVP, or not at
species level — are removed. WCVP filtering is only applied when WCVP
data are present in `keys` (i.e. `"WCVP"` appears in the `sources`
attribute). Duplicate records are reported but not removed.
