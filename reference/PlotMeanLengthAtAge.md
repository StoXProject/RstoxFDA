# Plot length at age

Plot mean individual length for each age group, along with an interval
representation for the error of the mean. The interval plotted is as
configured in 'ReportFdaLengthAtAgeData' (the interval Low / High) and
is represented by dashed lines.

The interval does not represent the range of weights occurring in each
age group, but the error of the estimate of mean length

If any grouping variables are configured for argument
'ReportFdaLengthAtAgeData', groups will be plotted in different colors.

## Usage

``` r
PlotMeanLengthAtAge(ReportFdaLengthAtAgeData)
```

## Arguments

- ReportFdaLengthAtAgeData:

  [`ReportFdaLengthAtAgeData`](ReportFdaLengthAtAgeData.md) with mean
  weight statistics from Reca simulations

## Value

[`PlotMeanLengthAtAgeData`](PlotMeanLengthAtAgeData.md)

## See also

Provide data for this plot with e.g.
[`ReportRecaLengthAtAge`](ReportRecaLengthAtAge.md)

## Examples

``` r
 lengthAtAge <- RstoxFDA::ReportRecaLengthAtAge(RstoxFDA::RecaCatchAtAgeExample,
        PlusGroup = 13)
 RstoxFDA::PlotMeanLengthAtAge(lengthAtAge)
```
