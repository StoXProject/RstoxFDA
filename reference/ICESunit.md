# ICES units (area codes)

Definition ICES Units (area codes).

## Usage

``` r
data(ICESunit)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Details

Polygons are derived from shapefiles provided by ICES web-portals, and
has been edited with some simplifications. Notably detailed coast-lines
have been removed, in favor of drawing area borders on land-mass.

Polygons are defined in WGS84 coordinates (unprojected).

## Examples

``` r
 # plot ICES areas
 data(ICESunit)
 plotArea(areaDef=ICESunit)
```
