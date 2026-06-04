# Check point data and latitude and longitude are sensible to work with

Checked the dataframe for NA, latitude below -90 or above 90, longitude
below -180 and above 180, also warns if it finds whole numbers or 0

## Usage

``` r
llCheck(thepoints)
```

## Arguments

- thepoints:

  set of points as a dataframe with latitude and longitude

## Value

nothing or warning or error

## Author

Justin Moat. J.Moat@kew.org
