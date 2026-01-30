# Writes shape files as WKT files

Writes
[`SpatialPolygonsDataFrame`](https://edzer.github.io/sp/reference/SpatialPolygons.html)
or [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data.frames,
such as
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
as Stox-WKT files (stratafiles)

## Usage

``` r
writeSpDataFrameAsWKT(shape, output, namecol = "StratumName")
```

## Arguments

- shape:

  [`SpatialPolygonsDataFrame`](https://edzer.github.io/sp/reference/SpatialPolygons.html)
  or [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data.frame
  stratadefinition to convert

- output:

  filename to save output to

- namecol:

  name of column in 'shape' that are to be used as strata names.
  Defaults to 'StratumName' pr the definition of
  [`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
