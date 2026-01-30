# Main areas (FDIR to 2017 incl.)

Definition for area coding system defined by the Norwegian directorate
of Fisheries up until 2017 (inclusive). Polygons are defined in WGS84
coordinates (unprojected).

## Usage

``` r
data(mainareaFdir2017)
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
