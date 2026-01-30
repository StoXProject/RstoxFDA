# Report catch at length

Tabulates summary statistics for analytical catch at length estimate.
Summary statistics are obtained as analytical domain estimates,
including a length-group domain, obtained by annotating sample data with
the function [`AddLengthGroupStoxBiotic`](AddLengthGroupStoxBiotic.md).
An estimate of the standard deviation of their sampling distribution
(the standard error) is also provided. Confidence intervals are
calculated from a Gaussian approximation to the sampling distribution.

If AnalyticalPopulationEstimateData contains estimates for domains that
include more than just length group, such as area, gear, stock, etc.,
summary statistics will be presented similarly.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for catch at length in numbers are those
listed for quantity 'cardinaltiy' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportAnalyticalCatchAtLength(
  AnalyticalPopulationEstimateData,
  LengthGroupVariable = character(),
  IntervalWidth = numeric(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("cardinality", conversionRange = c(1, 1e+12))
)
```

## Arguments

- AnalyticalPopulationEstimateData:

  Results from analytical estimates
  ([`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)).
  A variable identifying length groups (argument: 'LengthGroupVariable')
  must be among the domain variables. This must be formatted as done by
  [`AddLengthGroupStoxBiotic`](AddLengthGroupStoxBiotic.md).

- LengthGroupVariable:

  Name of domain variable in 'AnalyticalPopulationEstimateData' that
  identifies length group.

- IntervalWidth:

  The width of the reported confidence interval. A value of 0.9 gives 90
  per cent confidence intervals.

- Decimals:

  integer specifying the number of decimals to report for
  'CatchAtLength', 'SD', 'Low' and 'High'.

- Unit:

  unit for 'CatchAtLength', 'SD', 'Low' and 'High'

## Value

[`ReportFdaCatchAtLengthData`](ReportFdaCatchAtLengthData.md)

## See also

[`AddLengthGroupStoxBiotic`](AddLengthGroupStoxBiotic.md) for annotating
length groups,
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md) and
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md) for obtaining
analytical estimates. See
[`ReportAnalyticalCatchAtAge`](ReportAnalyticalCatchAtAge.md) for
reporting catch at age.
