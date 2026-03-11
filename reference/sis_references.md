# Generate the references.csv file

Generate the references.csv file

## Usage

``` r
sis_references(unique_id, gbif_ref = NULL, powo_ref = NULL)
```

## Arguments

- unique_id:

  (character) Unique identifier.

- gbif_ref:

  (data frame) A GBIF download citation according to IUCN format.

- powo_ref:

  (logical) Adds POWO reference if TRUE.

## Value

Returns an SIS compliant data frame
