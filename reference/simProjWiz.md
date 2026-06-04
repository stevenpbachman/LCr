# Simple Projection Wizard

Projects any set of latitude and longitude points to a "suitable" area
projection, based on thieir "true centre of gravity". Data is expected
as lat long in decimal degrees and returned in metres. Input data is
checked to make sure it’s sensible before projection (i.e. lat and longs
on the earth no null or NA values)

## Usage

``` r
simProjWiz(thepoints, thecentre, returnV = "S")
```

## Arguments

- thepoints:

  set of points as a dataframe with latitude and longitude

- thecentre:

  one point i.e. c(lat,long), if not specified this will be calculated
  from the center of gravity of all points

- returnV:

  switches to return either dataframe (x,y) or simple feature of
  points  
  S = simple, returns as dataframe of x,y  
  SF = simple feature of points

## Value

Defaults is a set of points in meters as a dataframe with projection
details attributed (stored as crs to retrieve
attr(myprojectedpoints,'crs'))

## Author

Justin Moat. J.Moat@kew.org
