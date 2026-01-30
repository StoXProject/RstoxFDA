# Main areas (FDIR from 2018 incl.)

Definition for area coding system defined by the Norwegian directorate
of Fisheries (2018 revision). This revision has been in use for their
fishery statistics as of 2018 (inclusive). Polygons are defined in WGS84
coordinates (unprojected).

## Usage

``` r
data(mainareaFdir2018)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Examples

``` r
 RstoxFDA::plotArea(areaDef = RstoxFDA::mainareaFdir2018)
```
