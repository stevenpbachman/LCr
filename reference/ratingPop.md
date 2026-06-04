# IUCN rating based on population reduction

Calculates IUCN rating based on based on population reduction as a
percentage

## Usage

``` r
ratingPop(pReduction, subCr = 2, abb = TRUE)
```

## Arguments

- pReduction:

  reduction as a percentage

- subCr:

  sub Criteria category: 1 or 2 or 3 or 4 (2 default)

- abb:

  abbreviation TRUE or FALSE , TRUE = 2 letter code, FALSE = full text
  (see value), default = TRUE

## Value

Text one of CR, EN, VU, NT, LC or Critically Endangered, Endangered,
Vulnerable, Near Threatened, Least Concern

## Details

Calculates IUCN rating based on population reduction

## Author

Justin Moat. J.Moat@kew.org
