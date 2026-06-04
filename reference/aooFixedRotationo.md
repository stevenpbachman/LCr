# BETA Area of Occupancy (AOO), from systematic grid rotation and shifting

Calculates the Area of Occupancy AOO (smallest) by rotating the grid and
shifting in x and y direction. On a very few occasions the minimum
solution will not always be achieved, but this solution is quick and
consistent (not driven by the number of points). If your species is near
a threshold you may want to increase the number of iterations. Please
cite if using this algorithm: Moat, J., Bachman, S. P., Field, R., &
Boyd, D. S. (2018). Refining area of occupancy to address the modifiable
areal unit problem in ecology and conservation. Conservation biology,
32(6), 12781289.  
Works the same as aooFixedRotation, but much faster. In BETA until fully
tested On a very few occasions the minimum solution will not always be
achieved but it is quick and consistent (not driven by the number of
points). If your species is near a threshold you may want to increase
the number of iterations.

## Usage

``` r
aooFixedRotationo(
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

  switches to return different sets of results:  
    
  S = Simple, returns just the minimum AOO area in km2, (DEFAULT)  
  E = Expended simple, returns the solution for the smallest AOO as list
  of (area,number of cells, rotation (degrees), shift in x direction,
  shift in y direction)  
  ALL = returns a dataframe of all of the results from all the trials
  with (number of cells, rotation (0 in this case), shift in x direction
  (metres), shift in y direction (metres))  
  SF = returns a polygon simple feature for mapping, plotting in
  ggplot/plot or to export to a GIS format.

- rotation:

  allow rotation of grids? (default = TRUE). If rotations are trigger
  selected iterations are shared 50:50 rotation:shift(both in x and y
  direction)

## Value

dependent on switch, default is area in km2

## See also

[`ratingAoo`](https://stevenpbachman.github.io/LCr/reference/ratingAoo.md)
for AOO Ratings

[`aooFixedGrid`](https://stevenpbachman.github.io/LCr/reference/aooFixedGrid.md)
for fixed grid optimal method

[`aooFixedRotation`](https://stevenpbachman.github.io/LCr/reference/aooFixedRotation.md)
for original method method

[`aoo`](https://stevenpbachman.github.io/LCr/reference/aoo.md) for
simple AOO method

[`buildCellPolys_rxy`](https://stevenpbachman.github.io/LCr/reference/buildCellPolys_rxy.md)
for building grid polygons from points, rotation and shift

## Author

Justin Moat. J.Moat@kew.org
