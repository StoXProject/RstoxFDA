# Report catch statistics

Report summary statistics for landed catches from MCMC simulations using
Reca. This function reports estimated total catch, mean length, mean
weight, and mean age.

If 'RecaCatchAtAge' contains estimate for a set of aggregation
variables, such as area, gear, stock, etc., summary statistics will be
presented similarly.

The units considered valid for mean lengths are those listed for
quantity 'length' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html) The
units considered valid for weights are those listed for quantity 'mass'
in [`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html) The
units considered valid for total catch in numbers are those listed for
quantity 'cardinality' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

Summary statistics are obtained from the posterior distribution, and the
interval is reported as equal-tailed credible intervals are reported.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

## Usage

``` r
ReportRecaCatchStatistics(
  RecaCatchAtAge,
  IntervalWidth = numeric(),
  UseDefaultDecimalOptions = TRUE,
  DecimalTotalNumber = integer(),
  DecimalTotalWeight = integer(),
  DecimalMeanAge = integer(),
  DecimalMeanWeight = integer(),
  DecimalMeanLength = integer(),
  UseDefaultUnitOptions = TRUE,
  UnitTotalNumber = RstoxData::getUnitOptions("cardinality", conversionRange = c(1,
    1e+12)),
  UnitTotalWeight = RstoxData::getUnitOptions("mass", conversionRange = c(1, 1e+12)),
  UnitMeanWeight = RstoxData::getUnitOptions("mass", conversionRange = c(1e-04, 10)),
  UnitMeanLength = RstoxData::getUnitOptions("length", conversionRange = c(1e-04, 10))
)
```

## Arguments

- RecaCatchAtAge:

  Results from MCMC simulations ([`RecaCatchAtAge`](RecaCatchAtAge.md)).

- IntervalWidth:

  The width of the reported credible interval. A value to 0.9 gives 90
  per cent credible intervals. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$IntervalWidth`.

- UseDefaultDecimalOptions:

  logical determining whether to use default decimal options.

- DecimalTotalNumber:

  integer specifying the number of decimals to report for 'TotalNumber',
  and the corresponding 'SD', 'Low' and 'High'. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalTotalNumber`.

- DecimalTotalWeight:

  integer specifying the number of decimals to report for
  'TotalWeightDefaults', and the corresponding 'SD', 'Low' and 'High'.
  Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalTotalWeight`.

- DecimalMeanAge:

  integer specifying the number of decimals to report for
  'MeanIndividualAge', and the corresponding 'SD', 'Low' and 'High'.
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanAge`.

- DecimalMeanWeight:

  integer specifying the number of decimals to report for
  'MeanIndividualWeight', and the corresponding 'SD', 'Low' and 'High'.
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanWeight`.

- DecimalMeanLength:

  integer specifying the number of decimals to report for
  'MeanIndividualLength', and the corresponding 'SD', 'Low' and
  'High'.`r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanLength`.

- UseDefaultUnitOptions:

  logical determining whether to use default unit options.

- UnitTotalNumber:

  unit for total catch in numbers.
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitTotalNumber`.

- UnitTotalWeight:

  unit for weight of total catch.
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitTotalWeight`.

- UnitMeanWeight:

  unit for mean weight.
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitMeanWeight`.

- UnitMeanLength:

  unit for mean length.
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitMeanLength`.

## Value

[`ReportFdaSummaryData`](ReportFdaSummaryData.md)

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis
[`ReportRecaCatchAtAge`](ReportRecaCatchAtAge.md) for reporting catch,
mean length and mean weight by age groups
[`ReportRecaCatchAtLength`](ReportRecaCatchAtLength.md) for reporting
length distributions
[`ReportRecaCatchAtLengthAndAge`](ReportRecaCatchAtLengthAndAge.md) for
reporting catch by age-group and length-group combinations.

## Examples

``` r
 catchStats <- ReportRecaCatchStatistics(RstoxFDA::RecaCatchAtAgeExample)
 catchStats$MeanAge
#>    MeanIndividualAge    SD   Low  High
#>                <num> <num> <num> <num>
#> 1:               6.9   0.1   6.7   7.1
 catchStats$MeanWeight
#>    MeanIndividualWeight    SD   Low  High
#>                   <num> <num> <num> <num>
#> 1:                2.943 0.104 2.775 3.085
 catchStats$MeanLength
#>    MeanIndividualLength    SD    Low   High
#>                   <num> <num>  <num>  <num>
#> 1:               64.827  0.71 63.707 65.855
 
 #Note that there is no error on the total weight estimate, when there are no grouping variables
 catchStats$TotalWeight
#>    TotalWeight    SD   Low  High
#>          <num> <num> <num> <num>
#> 1:          66     0    66    66
 catchStats$TotalNumber
#>    TotalNumber    SD   Low  High
#>          <num> <num> <num> <num>
#> 1:          23     1    22    24
```
