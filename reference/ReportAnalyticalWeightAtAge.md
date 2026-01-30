# Report mean weight at age

Tabulates summary statistics for analytical mean weight at age
estimates. Summary statistics are obtained as analytical domain
estimates, along with an estimate of the standard deviation of their
sampling distribution (the standard error). Confidence intervals are
calculated from a Gaussian approximation to the sampling distribution.

If AnalyticalPopulationEstimateData contains estimates for domains that
include more than just age, such as area, gear, stock, etc., summary
statistics will be presented similarly.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

Covariances for means of a variable between domains are not always
defined, and the variances (and hence 'SD', 'Low' and 'High') for means
in the plusgroup is approximated with an assumption of independence.

The units considered valid for catch at age in numbers are those listed
for quantity 'mass' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportAnalyticalWeightAtAge(
  AnalyticalPopulationEstimateData,
  PlusGroup = integer(),
  IntervalWidth = numeric(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("mass", conversionRange = c(0.001, 1))
)
```

## Arguments

- AnalyticalPopulationEstimateData:

  Results from analytical estimates
  ([`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)).
  The StoxBiotic variable 'IndividualAge' must be among the
  DomainVariables, and estimates must be available for the
  StoxBiotic-variable 'IndividualRoundWeight'.

- PlusGroup:

  If given, ages 'PlusGroup' or older are included in a plus group.

- IntervalWidth:

  The width of the reported confidence interval. A value of 0.9 gives 90
  per cent confidence intervals.

- Decimals:

  integer specifying the number of decimals to report for
  'MeanIndividualWeight', 'SD', 'Low' and 'High'.

- Unit:

  unit for 'MeanIndividualWeight', 'SD', 'Low' and 'High'

## Value

[`ReportFdaWeightAtAgeData`](ReportFdaWeightAtAgeData.md)

## See also

[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md) and
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md) for obtaining
analytical estimates.
