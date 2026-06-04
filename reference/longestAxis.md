# Longest distance from a set of points

Calculates the longest distances from a set of points

## Usage

``` r
longestAxis(thepoints, returnV = "S")
```

## Arguments

- thepoints:

  dataframe of points of x,y

- returnV, :

  two switches either S for simply the distance or P for a dataframe of
  the two furthest points

## Value

distance in metres or two points for the longest distance

## Note

Useful as a scale for cellsize and location buffers, Willis et al 2003
suggest 1/10 of this for cellsize for AOO calculations as does Rivers et
al (2010) for buffer distance for sub-population or location
calculations.

## Author

Justin Moat. J.Moat@kew.org
