# Generate the assessments.csv file

Generate the assessments.csv file

## Usage

``` r
sis_assessments(unique_id, native_ranges, wcvp_ipni_id = NULL, occs)
```

## Arguments

- unique_id:

  (character) Unique identifier.

- native_ranges:

  Data frame of native ranges used to calculate biogeographic realms.
  Pass `NULL` to skip realm calculation.

- wcvp_ipni_id:

  (character or NULL) WCVP IPNI identifiers used to generate
  distribution and habitat narrative text via POWO. Pass `NULL` for taxa
  without WCVP coverage (e.g. fungi), in which case
  `RangeDocumentation.narrative` and `HabitatDocumentation.narrative`
  will be left blank.

- occs:

  Occurrence data passed to `powo_text()`.

## Value

Returns an SIS compliant data frame
