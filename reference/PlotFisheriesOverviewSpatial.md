# Plot spatial distribution of fisheries

Plots catch density of landings on polygons.

## Usage

``` r
PlotFisheriesOverviewSpatial(
  ReportFdaLandingData,
  StratumPolygon,
  AreaLabels = F
)
```

## Arguments

- ReportFdaLandingData:

  [`ReportFdaLandingData`](ReportFdaLandingData.md)

- StratumPolygon:

  [`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)

- AreaLabels:

  if TRUE, labels with area codes are plotted on map.

## Value

[`PlotFisheriesOverviewSpatialData`](PlotFisheriesOverviewSpatialData.md)

## Details

'ReportFdaLandingData' must be configured with only one grouping
variable, which must match the area names in StratumPolygon\$StratumName

## See also

Provide data for this plot with
[`ReportFdaLandings`](ReportFdaLandings.md)

## Examples

``` r
 landingsReport <- RstoxFDA::ReportFdaLandings(
       RstoxFDA::StoxLandingDataExample, c("Area")
       )
 RstoxFDA::PlotFisheriesOverviewSpatial(landingsReport, RstoxFDA::mainareaFdir2018)
```
