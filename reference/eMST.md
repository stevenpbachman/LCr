# Euclidean Minimum spanning tree

Calculates the Euclidean Minimum spanning tree from a set of points.
This is used for the tree and branch building part of Rapoport's (1982)
mean propinquity method.

## Usage

``` r
eMST(thepoints)
```

## Arguments

- thepoints:

  dataframe of points in metres i.e. c(X,Y)

## Value

Simple feature of linestring, with a df of X1,Y1,X2,Y2,distance and
geom. N.B. X1,Y1 & and X2 Y2 are the to and from points

## Author

Justin Moat. J.Moat@kew.org
