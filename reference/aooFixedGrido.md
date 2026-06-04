# BETA Area of Occupancy (AOO), optimal shifting grid,

Calculates the optimal (smallest) Area of Occupancy AOO by shifting the
grid in x and y direction only. The minimum solution will be achieved
but large point datasets (i.e. over 70 points) will take some time to
process. Processing time is proportional to nop^2 (number of points
squared). Please cite below if using this algorithm: Moat, J., Bachman,
S. P., Field, R., & Boyd, D. S. (2018). Refining area of occupancy to
address the modifiable areal unit problem in ecology and conservation.
Conservation biology, 32(6), 1278-1289.

## Usage

``` r
aooFixedGrido(thepoints, cellsize = 2000, returnV = "S")
```

## Arguments

- thepoints:

  dataframe of points in metres i.e. c(x,y)

- cellsize:

  width of cell in metres (default 2000 m)

- returnV, :

  switches to return different sets of results:  
    
  S = Simple, returns just the minimum are in km2, (DEFAULT)  
  E = Expended simple, returns the solution for the smallest AOO as list
  of (area,number of cells, rotation (0 in this case), shift in x
  direction, shift in y direction)  
  ALL = returns a dataframe of all of the results with (number of cells,
  rotation (0 in this case), shift in x direction (metres), shift in y
  direction (metres))  
  SF = returns a polygon simple feature for mapping, plotting in ggplot
  or export to GIS systems  

## Value

dependent on switch, default is area in km2

## See also

[`ratingAoo`](https://stevenpbachman.github.io/LCr/reference/ratingAoo.md)
for AOO Ratings

[`aoo`](https://stevenpbachman.github.io/LCr/reference/aoo.md) for
simple AOO method

[`aooFixedRotation`](https://stevenpbachman.github.io/LCr/reference/aooFixedRotation.md)
for systematic methods with rotation

[`aooFixedGrid`](https://stevenpbachman.github.io/LCr/reference/aooFixedGrid.md)
for orginal method

[`buildCellPolys_rxy`](https://stevenpbachman.github.io/LCr/reference/buildCellPolys_rxy.md)
for building grid polygons from points, rotation and shift

## Author

Justin Moat. J.Moat@kew.org
