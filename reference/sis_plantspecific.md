# Generate the plantspecific.csv file

Generate the plantspecific.csv file

## Usage

``` r
sis_plantspecific(unique_id, wcvp_ipni_id = NULL, kingdom = "plantae")
```

## Arguments

- unique_id:

  (character) Unique identifier.

- wcvp_ipni_id:

  (character or NULL) Vector of WCVP IPNI IDs used to retrieve growth
  form data via POWO. Only required when `kingdom = "plantae"`; pass
  `NULL` (the default) for fungi.

- kingdom:

  (character) Kingdom of the taxa being assessed. One of `"plantae"`
  (default) or `"fungi"`. Controls which growth form logic is applied.

## Value

An SIS compliant data frame
