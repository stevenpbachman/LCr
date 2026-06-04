# Area of Occupancy (AOO) calculated by Systematic shifting and rotating of the grid

Calculates the Area of Occupancy AOO (smallest) by shifting and rotating
the grid in x and y direction only. In a very few occasions the minimum
solution will not always be achieved but it is quick and consistent (not
driven by the number of points). If your species is near a threshold you
may want to increase the number of iterations. Please cite below if
using this algorithm: Moat, J., Bachman, S. P., Field, R., & Boyd, D. S.
(2018). Refining area of occupancy to address the modifiable areal unit
problem in ecology and conservation. Conservation biology, 32(6),
1278-1289.

## Usage

``` r
aooFixedRotation(
  thepoints,
  cellsize = 2000,
  it = 1296,
  returnV = "S",
  rotation = TRUE
)
```

## Arguments

- thepoints:

  dataframe of points in metres i.e. c(x,y)

- cellsize:

  width of cell in metres (default 2000 m)

- it:

  the number of iterations you wish it to run, (default 1296)

- returnV, :

  switches to return different sets of results: S = Simple, returns just
  the minimum are in km2, (DEFAULT) E = Expended simple, returns the
  solution for the smallest AOO as list of (area,number of cells,
  rotation (degrees), shift in x direction, shift in y direction) ALL =
  returns a dataframe of all of the results with (number of cells,
  rotation (radians), shift in x direction (metres), shift in y
  direction (metres)) SF = returns a polygon simple feature for mapping,
  plotting in ggplot or export to GIS systems.

- rotation:

  allow rotation of grids? (default = TRUE). If rotations are selected
  iterations are shared 50:50 rotation:shift

## Value

dependent on switch, default is area in km2

## See also

[`ratingAoo`](https://stevenpbachman.github.io/LCr/reference/ratingAoo.md)
for AOO Ratings

[`aooFixedGrid`](https://stevenpbachman.github.io/LCr/reference/aooFixedGrid.md)
for fixed grid optimal method

[`aoo`](https://stevenpbachman.github.io/LCr/reference/aoo.md) for
simple AOO method

[`buildCellPolys_rxy`](https://stevenpbachman.github.io/LCr/reference/buildCellPolys_rxy.md)
for building grid polygons from points, rotation and shift

## Author

Justin Moat. J.Moat@kew.org
