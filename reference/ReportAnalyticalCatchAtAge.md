# Report catch at age

Tabulates summary statistics for analytical catch at age estimate.
Summary statistics are obtained as analytical domain estimates, along
with an estimate of the standard deviation of their sampling
distribution (the standard error). Confidence intervals are calculated
from a Gaussian approximation to the sampling distribution.

If AnalyticalPopulationEstimateData contains estimates for domains that
include more than just age, such as area, gear, stock, etc., summary
statistics will be presented similarly.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for catch at age in numbers are those listed
for quantity 'cardinaltiy' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportAnalyticalCatchAtAge(
  AnalyticalPopulationEstimateData,
  PlusGroup = integer(),
  IntervalWidth = numeric(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("cardinality", conversionRange = c(1, 1e+12))
)
```

## Arguments

- AnalyticalPopulationEstimateData:

  Results from analytical estimates
  ([`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)).
  The StoxBiotic variable 'IndividualAge' must be among the
  DomainVariables.

- PlusGroup:

  If given, ages 'PlusGroup' or older are included in a plus group.

- IntervalWidth:

  The width of the reported confidence interval. A value of 0.9 gives 90
  per cent confidence intervals.

- Decimals:

  integer specifying the number of decimals to report for 'CatchAtAge',
  'SD', 'Low' and 'High'.

- Unit:

  unit for 'CatchAtAge', 'SD', 'Low' and 'High'

## Value

[`ReportFdaCatchAtAgeData`](ReportFdaCatchAtAgeData.md)

## See also

[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md) and
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md) for obtaining
analytical estimates.
