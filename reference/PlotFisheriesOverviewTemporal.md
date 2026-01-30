# Plot landings

Plots landings by date of catch and by grouping variables.

## Usage

``` r
PlotFisheriesOverviewTemporal(ReportFdaLandingData)
```

## Arguments

- ReportFdaLandingData:

  [`ReportFdaLandingData`](ReportFdaLandingData.md)

## Value

[`PlotFisheriesOverviewTemporalData`](PlotFisheriesOverviewTemporalData.md)

## Details

"CatchDate" must be among the grouping variables of
'ReportFdaLandingData'

## See also

Provide data for this plot with
[`ReportFdaLandings`](ReportFdaLandings.md)

## Examples

``` r
 landingsReport <- RstoxFDA::ReportFdaLandings(
       RstoxFDA::StoxLandingDataExample, c("CatchDate")
       )
 RstoxFDA::PlotFisheriesOverviewTemporal(landingsReport)

 landingsReportGear <- RstoxFDA::ReportFdaLandings(
       RstoxFDA::StoxLandingDataExample, c("GearGroup", "CatchDate")
       )
 RstoxFDA::PlotFisheriesOverviewTemporal(landingsReportGear)
```
