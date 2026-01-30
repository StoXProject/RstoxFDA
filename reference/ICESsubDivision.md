# ICES sub-divisions (area codes from 2018)

Definition ICES SubDivisions as they have been defined from 2018
inclusive.

## Usage

``` r
data(ICESsubDivision)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) data.frame with
area names identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html).

## Details

Polygons are derived from shapefiles provided by ICES web-portals, and
has been edited with some simplifications. Notably detailed coast-lines
have been removed, in favor of drawing area borders on land-mass.

Polygons are defined in WGS84 coordinates (unprojected).

## Examples

``` r
 # plot ICES areas
 data(ICESsubDivision)
 plotArea(areaDef=ICESsubDivision)
```
