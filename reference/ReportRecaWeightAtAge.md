# Report weight at age

Tabulates summary statistics for mean weights at age from MCMC
simulations using Reca.

If 'RecaCatchAtAge' contains estimate for a set of aggregation
variables, such as area, gear, stock, etc., summary statistics will be
presented similarly.

Mean weight for plus-groups are a weighted by the relative catch-at-age
in each composite age group. For iterations where all of the plus-group
ages have a zero catch at age, this weight is not defined, and summary
statistics are obtained from the remaining iterations.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for mean weights are those listed for
quantity 'mass' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

MCMC simulations are typically obtained with
[`RunRecaModels`](RunRecaModels.md). Summary statistics are obtained
from the posterior distribution, and the interval is reported as 90%
equal-tailed credible intervals.

## Usage

``` r
ReportRecaWeightAtAge(
  RecaCatchAtAge,
  PlusGroup = integer(),
  IntervalWidth = numeric(),
  Decimals = integer(),
  Threshold = numeric(),
  Unit = RstoxData::getUnitOptions("mass", conversionRange = c(1e-04, 10))
)
```

## Arguments

- RecaCatchAtAge:

  Results from MCMC simulations ([`RecaCatchAtAge`](RecaCatchAtAge.md)).

- PlusGroup:

  If given, ages 'PlusGroup' or older are included in a plus group.

- IntervalWidth:

  The width of the reported credible interval. A value of 0.9 gives 90
  per cent credible intervals. Defaults to 0.9.

- Decimals:

  integer specifying the number of decimals to report for
  'MeanIndividualWeight', 'SD', 'Low' and 'High'. Defaults to 2.

- Threshold:

  threshold for reporting mean weight. Rows with an estimated Catch At
  Age (number of individuals) lower than this will have NA reported for
  their mean weight. Defaults to 0.

- Unit:

  unit for 'MeanIndividualWeight', 'SD', 'Low' and 'High'

## Value

[`ReportFdaWeightAtAgeData`](ReportFdaWeightAtAgeData.md)

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis

## Examples

``` r
  weightAtAge <- RstoxFDA::ReportRecaWeightAtAge(RstoxFDA::RecaCatchAtAgeExample, 
       PlusGroup = 13, Threshold = 1000, Decimals = 0, Unit = "g")
  weightAtAge
#> $MeanWeightByAge
#>     AgeGroup   Age MeanIndividualWeight    SD   Low  High
#>       <char> <num>                <num> <num> <num> <num>
#>  1:    Age 1     1                  271    67   185   388
#>  2:    Age 2     2                  632   106   468   758
#>  3:    Age 3     3                 1099    52  1013  1178
#>  4:    Age 4     4                 1421    55  1322  1510
#>  5:    Age 5     5                 1859    58  1768  1946
#>  6:    Age 6     6                 2351    67  2235  2447
#>  7:    Age 7     7                 2999    89  2844  3136
#>  8:    Age 8     8                 3544   107  3338  3697
#>  9:    Age 9     9                 4132   137  3847  4340
#> 10:   Age 10    10                 4771   193  4469  5021
#> 11:   Age 11    11                 5349   209  5006  5664
#> 12:   Age 12    12                 5940   245  5539  6362
#> 13:  Age 13+    13                 8232   562  7410  9150
#> 
#> $GroupingVariables
#> Empty data.table (0 rows and 1 cols): GroupingVariables
#> 
```
