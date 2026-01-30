# Location codes (FDIR from 2018 incl.)

Definition for location coding system defined by the Norwegian
directorate of Fisheries (2018 revision). This revision has been in use
for their fishery statistics as of 2018 (inclusive). Polygons are
defined in WGS84 coordinates (unprojected).

## Usage

``` r
data(locationsFdir2018)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Examples

``` r
 RstoxFDA::plotAreaComparison(RstoxFDA::mainareaFdir2017, 
     RstoxFDA::mainareaFdir2018, 
     xlim=c(0,20), ylim=c(50,60))
```
