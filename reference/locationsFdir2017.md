# Location codes (FDIR to 2017 incl.)

Definition for location coding system defined by the Norwegian
directorate of Fisheries up until 2017 (inclusive). Polygons are defined
in WGS84 coordinates (unprojected).

## Usage

``` r
data(locationsFdir2017)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Examples

``` r
 # compare locations and ICES rectangles in Barents Sea
 RstoxFDA::plotAreaComparison(
    RstoxFDA::ICESrectangles, 
    RstoxFDA::locationsFdir2017, 
    xlim=c(20,30), 
    ylim=c(70,75), 
    linetype2 = "dotted", 
    polygonColor1 = "green", 
    areaLabels2 = TRUE)
```
