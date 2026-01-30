# Report covariance of catch at age

Tabulates the covariance between age groups of (number) at age from MCMC
simulations using Reca. MCMC simulations are typically obtained with
[`RunRecaModels`](RunRecaModels.md). Covariances are obtained from the
posterior distribution.

If 'RecaCatchAtAge' contains estimate for a set of aggregation
variables, such as area, gear, stock, etc., covariances are calculated
between age groups for each of these aggregation variables.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for catch at age in numbers are those listed
for quantity 'cardinaltiy' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportRecaCatchAtAgeCovariance(
  RecaCatchAtAge,
  PlusGroup = integer(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("cardinality", conversionRange = c(1, 1e+12))
)
```

## Arguments

- RecaCatchAtAge:

  Results from MCMC simulations ([`RecaCatchAtAge`](RecaCatchAtAge.md)).

- PlusGroup:

  If given, ages 'PlusGroup' or older are included in a plus group.

- Decimals:

  integer specifying the number of decimals to report for 'Covariance'.
  Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtAgeCovariance$functionParameterDefaults$Decimals`.

- Unit:

  unit for 'CatchAtAge'. Covariance will be provided as the square of
  this unit.

## Value

[`ReportFdaCatchAtAgeCovarianceData`](ReportFdaCatchAtAgeCovarianceData.md)

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis and
[`ReportRecaCatchAtLength`](ReportRecaCatchAtLength.md) for reporting
length composition.

## Examples

``` r
 covariances <- RstoxFDA::ReportRecaCatchAtAgeCovariance(RstoxFDA::RecaCatchAtAgeExample, 
        PlusGroup = 13)
 covariances$CovarianceNbyAge
#>      VariableId1 VariableId2   Covariance
#>           <char>      <char>        <num>
#>   1:       Age 1       Age 1   1008325267
#>   2:      Age 10       Age 1   -343051776
#>   3:      Age 11       Age 1    151223129
#>   4:      Age 12       Age 1   -187509543
#>   5:     Age 13+       Age 1    233715735
#>  ---                                     
#> 165:       Age 5       Age 9 -69406163910
#> 166:       Age 6       Age 9   5930467623
#> 167:       Age 7       Age 9  -9485655460
#> 168:       Age 8       Age 9 -28954943730
#> 169:       Age 9       Age 9  55496753071
```
