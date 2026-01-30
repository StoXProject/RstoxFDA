# Append area code

Append area code

## Usage

``` r
appendAreaCode(
  table,
  areaPolygons,
  latName,
  lonName,
  colName,
  StratumName = "StratumName",
  strict = T
)
```

## Arguments

- table:

  data.table to be annotated.

- areaPolygons:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data.frame

- latName:

  name of WGS84 lat column in 'table'

- lonName:

  name of WGS84 lon column in 'table

- colName:

  name of column to be appended to 'table'

- StratumName:

  name of column in 'areaPolygons' that identify the area name

- strict:

  logical determining whether to run in strict mode. See details.

## Value

'table' with the area appended in the column 'colName'

## Details

Appends a column with an area code to a table, based on positions.

If polygons are overlapping, so that one point may be in several
polygons, an arbitrary choice is made and a warning is issued.

By default this function is run in 'strict' mode, meaning that it will
halt with an error if some positions are not in any of the provided
polygons, or if some positions are missing (NA). Turning of strict-mode
accepts both these cases and the area code will be NA for these
positions.
