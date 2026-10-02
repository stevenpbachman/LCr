# Ensure a data frame has all required fields, filled with typed NAs if missing

Ensure a data frame has all required fields, filled with typed NAs if
missing

## Usage

``` r
ensure_required_fields(df, required_fields, numeric_fields)
```

## Arguments

- df:

  (data frame) The occurrence data to check

- required_fields:

  (character) Vector of field names that must be present

- numeric_fields:

  (character) Subset of \`required_fields\` that should be numeric
  (filled with \`NA_real\_\` rather than \`NA_character\_\` if missing)

## Value

(data frame) \`df\` with any missing fields added, and columns reordered
to match \`required_fields\`
