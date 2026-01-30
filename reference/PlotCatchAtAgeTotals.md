# Plot Catch At Age

Plots total catch in each age group as a barplot with error bars.

## Usage

``` r
PlotCatchAtAgeTotals(ReportFdaCatchAtAgeData)
```

## Arguments

- ReportFdaCatchAtAgeData:

  [`ReportFdaCatchAtAgeData`](ReportFdaCatchAtAgeData.md) with catch at
  age estimates to plot

## Value

[`PlotCatchAtAgeTotalsData`](PlotCatchAtAgeTotalsData.md)

## Details

Error bars correspond to the columns 'High' and 'Low' in
'ReportFdaCatchAtAgeData' If 'ReportFdaCatchAtAgeData' has grouping
variables a bar will be plotted for each group and age-group, and bars
will be grouped by age group

## See also

Provide data for this plot with e.g.
[`ReportRecaCatchAtAge`](ReportRecaCatchAtAge.md)

## Examples

``` r
 catchAtAgeReport <- RstoxFDA::ReportRecaCatchAtAge(RstoxFDA::RecaCatchAtAgeExample, 
       PlusGroup = 13)
 RstoxFDA::PlotCatchAtAgeTotals(catchAtAgeReport)
```
