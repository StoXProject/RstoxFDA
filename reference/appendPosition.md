# Append positions

Appends columns with positions to a data table, based on an area code.

Positions are centroids of areas. For oddly shaped (concave) areas, this
can have unintended effects. The centroid may for instance lie outside
the area.

## Usage

``` r
appendPosition(
  table,
  areaPolygons,
  areaName,
  latColName,
  lonColName,
  StratumName = "StratumName"
)
```

## Arguments

- table:

  data.table to be annotated.

- areaPolygons:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data.frames
  with area names identified in the column 'StratumName'

- areaName:

  name of column that identifies the area in 'table'

- latColName:

  name of the latitdue column to be appended to 'table'

- lonColName:

  name of the longitude column to be appended to 'table'

- StratumName:

  name of the column in 'areaPolygons' that identifies the area.

## Value

'table' with the positions appended in the columns 'latColName' and
'lonColName'.
