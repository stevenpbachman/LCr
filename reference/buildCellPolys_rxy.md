# Build simple feature polygons from point data, rotation and shift in x and y direction

Builds cell polygons (as simple features) from points and rotation,
shift in X and y returns polygons for ggplot2 and mapping. Generally
used to plot data from AOO calculations.

## Usage

``` r
buildCellPolys_rxy(thepoints, cellsize, rot, shiftx, shifty)
```

## Arguments

- thepoints:

  set of points in metres i.e. c(x,y)

- cellsize:

  size of cell (length) in metres

- rot:

  rotation of the grid in radian

- shiftx:

  shift in the x direction in metres

- shifty:

  shift in the y direction in metres

## Value

Simple Feature of polygons

## Author

Justin Moat. J.Moat@kew.org
