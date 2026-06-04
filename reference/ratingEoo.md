# IUCN rating based on EOO Area

Calculates IUCN rating based on Extent of Occurrence (EOO) Area in km2

## Usage

``` r
ratingEoo(EOOArea, abb = TRUE)
```

## Arguments

- EOOArea:

  Area in km2

- abb:

  abbreviation TRUE or FALSE , TRUE = 2 letter code, FALSE = full text
  (see value), default = TRUE

## Value

Text one of CR, EN, VU, NT, LC or Critically Endangered, Endangered,
Vulnerable, Near Threatened, Least Concern

## Details

Calculates IUCN rating on EOO

## See also

[`eoo`](https://stevenpbachman.github.io/LCr/reference/eoo.md) for EOO
calculations

## Author

Justin Moat. J.Moat@kew.org
