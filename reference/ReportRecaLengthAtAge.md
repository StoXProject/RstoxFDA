# Report length at age

Tabulates summary statistics for mean length at age from MCMC
simulations using Reca. MCMC simulations are typically obtained with
[`RunRecaModels`](RunRecaModels.md). Summary statistics are obtained
from the posterior distribution, and the interval is reported as
equal-tailed credible intervals are reported.

Mean length for plus-groups are a weighted by the relative catch-at-age
in each composite age group. For iterations where all of the plus-group
ages have a zero catch at age, this weight is not defined, and summary
statistics are obtained from the remaining iterations.

If 'RecaCatchAtAge' contains estimate for a set of aggregation
variables, such as area, gear, stock, etc., summary statistics will be
presented similarly.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for mean lengths are those listed for
quantity 'length' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportRecaLengthAtAge(
  RecaCatchAtAge,
  PlusGroup = integer(),
  IntervalWidth = numeric(),
  Decimals = integer(),
  Threshold = numeric(),
  Unit = RstoxData::getUnitOptions("length", conversionRange = c(1e-07, 10))
)
```

## Arguments

- RecaCatchAtAge:

  Results from MCMC simulations ([`RecaCatchAtAge`](RecaCatchAtAge.md)).

- PlusGroup:

  If given, ages 'PlusGroup' or older are included in a plus group.

- IntervalWidth:

  The width of the reported credible interval. A value of 0.9 gives 90
  per cent credible intervals. Defaults to \`r
  stoxFunctionAttributes\$ReportRecaLengthAtAge\$functionParameterDefaults\$IntervalWidth\`.

- Decimals:

  integer specifying the number of decimals to report for
  'MeanIndividualLength', 'SD', 'Low' and 'High'. Defaults to \`r
  stoxFunctionAttributes\$ReportRecaLengthAtAge\$functionParameterDefaults\$Decimals\`.

- Threshold:

  threshold for reporting mean weight. Rows with an estimated Catch At
  Age (number of individuals) lower than this will have NA reported for
  their mean length Defaults to \`r
  stoxFunctionAttributes\$ReportRecaLengthAtAge\$functionParameterDefaults\$Threshold\`.

- Unit:

  unit for 'MeanIndividualLength', 'SD', 'Low' and 'High'

## Value

[`ReportFdaLengthAtAgeData`](ReportFdaLengthAtAgeData.md)

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis

## Examples

``` r
  lengthAtAge <- ReportRecaLengthAtAge(RstoxFDA::RecaCatchAtAgeExample, 
        PlusGroup = 13, Unit="cm", Decimals = 0)
  lengthAtAge$MeanLengthByAge
#>     AgeGroup   Age MeanIndividualLength    SD   Low  High
#>       <char> <num>                <num> <num> <num> <num>
#>  1:    Age 1     1                   28     2    25    32
#>  2:    Age 2     2                   38     4    35    41
#>  3:    Age 3     3                   47     1    45    48
#>  4:    Age 4     4                   51     1    50    52
#>  5:    Age 5     5                   56     1    55    57
#>  6:    Age 6     6                   61     1    60    62
#>  7:    Age 7     7                   67     1    65    67
#>  8:    Age 8     8                   71     1    70    72
#>  9:    Age 9     9                   75     1    73    76
#> 10:   Age 10    10                   79     1    77    80
#> 11:   Age 11    11                   82     1    81    83
#> 12:   Age 12    12                   85     1    84    87
#> 13:  Age 13+    13                   95     2    92    98
```
