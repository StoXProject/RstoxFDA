# Merges polygons

Merge [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data
table, such as
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)

## Usage

``` r
mergePolygons(shape, mergeCol, tolerance = 0, snapProjection = "EPSG:3857")
```

## Arguments

- shape:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data.table
  with polygons to convert

- mergeCol:

  name of column that should be used for merging, all polygons with the
  same value in this column will be merged into one.

- tolerance:

  parameter passed to
  [`st_snap`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html),
  a value of 0 correspond to no snapping

- snapProjection:

  projection to perform snapping

## Value

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with polygons
merged

## Details

All columns must have the same value for all polygons that are to be
merged. If columns are not consistent in this regard, an error is
raised.

In order to deal with any inaccuracies in polygon definitions, polygons
are snapped to a common grid before merging This has to be done in
planar coordinates, the projection used is controlled by
'snapProjection' The resolution of the grid is controlled by the
parameter 'tolerance', which can be either a units object, or a
numerical value passed to
[`st_snap`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).
The interpretation of tolerance as a numerical value is defined in sf,
but visual inspection of results is always adviced.

## Examples

``` r
  #merge ICES area-definitions by division
  library(sf)
  division <- RstoxFDA::ICESareas
  division$StratumName <- paste(division$Major_FA, 
      division$SubArea, division$Division, sep=".")
  merged <- mergePolygons(division["StratumName"], "StratumName", tolerance=.001)
  #compare original area definition with merged
  RstoxFDA::plotAreaComparison(RstoxFDA::ICESareas, merged, ylim=c(30,80), areaLabels2 = TRUE)
```
