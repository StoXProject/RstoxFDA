# Define Area Code Positions

Define positions for areas of a spatial coding system.

## Usage

``` r
DefineAreaPosition(
  processData,
  DefinitionMethod = c("ResourceFile", "StratumPolygon"),
  FileName = character(),
  StratumPolygon,
  UseProcessData = F
)
```

## Arguments

- processData:

  [`AreaPosition`](AreaPosition.md) as returned from this function.

- DefinitionMethod:

  'ResourceFile' or 'StratumPolygon', see details.

- FileName:

  path to resource file

- StratumPolygon:

  [`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
  to extract area positions from.

- UseProcessData:

  If TRUE, bypasses execution of function and returns existing
  'processData'

## Value

[`AreaPosition`](AreaPosition.md).

## Details

Defines an association between area codes and positions that represent
that area code. For example an area code could be associated with the
centre of mass of the area, or some other point within the area. This
may be useful for providing approximate coordinates when locations are
identified by area codes. The soruces of the area-position association
may be a table or a appropriately formatted polygon-definition
([`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)).

For DefinitionMethod 'ResourceFile': Definitions are read from a tab
separated, UTF-8 encoded file with headers. Columns defined as:

- Column 1: 'Area':

  Area code (key)

- Column 2: 'Location':

  optional subdivision of area. If provided, positions for missing
  locations should be encoded as well.

- Column 3: 'Latitude':

  WGS84 Latitude, decimal degrees

- Column 4: 'Longitude':

  WGS84 Longitude, decimal degrees

For DefinitionMethod 'StratumPolygon': Definitions are extracted from a
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html):
'Area' in [`AreaPosition`](AreaPosition.md) is derived from the column
'StratumName' in
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html).
'Location' in [`AreaPosition`](AreaPosition.md) is encoded as missing.
'Latitude' and 'Longitude' in [`AreaPosition`](AreaPosition.md) are the
centroids set for each polygon in
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html).
Note that for oddly shaped polygons (concave polygons) the centroid may
lay outside the area.

## See also

[`SetAreaPositionsBiotic`](SetAreaPositionsBiotic.md) and
[`AddAreaPositionStoxLanding`](AddAreaPositionStoxLanding.md) for adding
positions to data.
