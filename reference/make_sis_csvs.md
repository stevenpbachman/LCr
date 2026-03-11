# Generate all SIS connect csv files

Generate all SIS connect csv files

## Usage

``` r
make_sis_csvs(
  unique_id,
  wcvp_ipni_id,
  first_name,
  second_name,
  email,
  institution,
  gbif_ref = NULL,
  powo_ref = FALSE,
  native_ranges = NULL,
  occs = NULL,
  family,
  genus,
  species,
  taxonomicAuthority,
  kingdom = "plantae"
)
```

## Arguments

- unique_id:

  (character) Unique identifier - default is the GBIF usage key

- first_name:

  (character) First name of assessor

- second_name:

  (character) Second name of assessor

- email:

  (character) Email of assessor

- institution:

  (character) Name of institution or affiliation

- gbif_ref:

  (data frame) A GBIF download citation according to IUCN format.

- powo_ref:

  (data frame) A citation for use of POWO according to IUCN format.

- native_ranges:

  (data frame) Native ranges derived from \[\`get_native_range()\`\]

- family:

  (character) Field containing the family

- genus:

  (character) Field containing the genus

- species:

  (character) Field containing the specific epithet

- taxonomicAuthority:

  (character) Field containing the taxonomic authority

- kingdom:

  (character) Default is 'plantae', but can also be 'fungi'

## Value

Returns an SIS compliant zip file
