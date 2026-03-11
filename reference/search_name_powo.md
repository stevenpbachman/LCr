# Query taxon name against POWO (Plants of the World Online) backbone

Queries a taxon name against the POWO names backbone. Simple wrapper
around the rWCVP \`wcvp_match_names\` function.

## Usage

``` r
search_name_powo(df, name_column, author_col = NULL)
```

## Arguments

- df:

  (data frame) Taxon name(s)

- name_column:

  (string) Column that contains the name(s)

- author_col:

  (string) Column that contains the author name(s)

## Value

Returns a data frame with initial search term and matching name(s)
