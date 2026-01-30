# Report catch at age

Tabulates summary statistics for total catch (number) at age from MCMC
simulations using Reca. MCMC simulations are typically obtained with
[`RunRecaModels`](RunRecaModels.md). Summary statistics are obtained
from the posterior distribution, and the interval is reported as
equal-tailed credible intervals.

If 'RecaCatchAtAge' contains estimate for a set of aggregation
variables, such as area, gear, stock, etc., summary statistics will be
presented similarly.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for catch at age in numbers are those listed
for quantity 'cardinaltiy' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportRecaCatchAtAge(
  RecaCatchAtAge,
  PlusGroup = integer(),
  IntervalWidth = numeric(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("cardinality", conversionRange = c(1, 1e+12))
)
```

## Arguments

- RecaCatchAtAge:

  Results from MCMC simulations ([`RecaCatchAtAge`](RecaCatchAtAge.md)).

- PlusGroup:

  If given, ages 'PlusGroup' or older are included in a plus group.

- IntervalWidth:

  The width of the reported credible interval. A value of 0.9 gives 90
  per cent credible intervals. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtAge$functionParameterDefaults$IntervalWidth`

- Decimals:

  integer specifying the number of decimals to report for 'CatchAtAge',
  'SD', 'Low' and 'High'. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtAge$functionParameterDefaults$Decimals`.

- Unit:

  unit for 'CatchAtAge', 'SD', 'Low' and 'High'

## Value

[`ReportFdaCatchAtAgeData`](ReportFdaCatchAtAgeData.md)

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis and
[`ReportRecaCatchAtLength`](ReportRecaCatchAtLength.md) for reporting
length composition.

## Examples

``` r
  catchAtAgeReport <- RstoxFDA::ReportRecaCatchAtAge(RstoxFDA::RecaCatchAtAgeExample, 
        PlusGroup = 13)
  catchAtAgeReport$NbyAge
#>     AgeGroup   Age CatchAtAge     SD     Low    High
#>       <char> <num>      <num>  <num>   <num>   <num>
#>  1:    Age 1     1      21492  31754     404   95927
#>  2:    Age 2     2      25084  40541      52   70750
#>  3:    Age 3     3     238764 110070  104672  422917
#>  4:    Age 4     4     987229 234699  653819 1429625
#>  5:    Age 5     5    6254131 786326 5115794 7344917
#>  6:    Age 6     6    3784386 561252 2921262 4766634
#>  7:    Age 7     7    3638165 487433 2904993 4491507
#>  8:    Age 8     8    4442624 523941 3664388 5323354
#>  9:    Age 9     9    1125128 235577  823717 1488910
#> 10:   Age 10    10     547490 148911  345107  826186
#> 11:   Age 11    11     385556 110394  249463  610248
#> 12:   Age 12    12     495272 123316  320967  695681
#> 13:  Age 13+    13     647093 151804  447239  914943
```
