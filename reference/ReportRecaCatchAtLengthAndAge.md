# Report catch at length and age

Tabulates summary statistics for total catch (number) at all age-length
combinations from MCMC simulations using Reca. MCMC simulations are
typically obtained with [`RunRecaModels`](RunRecaModels.md). Summary
statistics are obtained from the posterior distribution, and the
interval is reported as equal-tailed credible intervals.

Different length groups than the ones reported in the argument
'RecaCatchAtAge' may be specified with the argument 'IntervalWidth'.
This will specify equi-intervalled lengthgroups with the smallest
lengthgroup starting at 0. If it does not align with the length groups
reported in 'RecaCatchAtAge' length group assignment is done to the
highest overlapping length group.

If 'RecaCatchAtAge' contains estimate for a set of aggregation
variables, such as area, gear, stock, etc., summary statistics will be
presented similarly.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for catch at length in numbers are those
listed for quantity 'cardinaltiy' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportRecaCatchAtLengthAndAge(
  RecaCatchAtAge,
  PlusGroup = integer(),
  LengthInterval = numeric(),
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

- LengthInterval:

  width of length bins in cm. If not provided, the interval in
  'RecaCatchAtAge' will be used.

- IntervalWidth:

  The width of the reported credible interval. A value of 0.9 gives 90
  per cent credible intervals. Defaults to 0.9

- Decimals:

  integer specifying the number of decimals to report for 'CatchAtAge',
  'SD', 'Low' and 'High'. Defaults to 0.

- Unit:

  unit for 'CatchAtAge', 'SD', 'Low' and 'High'

## Value

[`ReportFdaCatchAtLengthAndAgeData`](ReportFdaCatchAtLengthAndAgeData.md)

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis and
[`ReportRecaCatchAtAge`](ReportRecaCatchAtAge.md) for reporting age
composition

## Examples

``` r
  lengthAge <- ReportRecaCatchAtLengthAndAge(RstoxFDA::RecaCatchAtAgeExample, 13, 10)
  lengthAge$NbyLengthAge
#>      AgeGroup   Age      LengthGroup Length    SD    Low   High
#>        <char> <num>           <char>  <num> <num>  <num>  <num>
#>   1:    Age 1     1    〈0.0, 10.0］     10     0      0      0
#>   2:    Age 1     1   〈10.0, 20.0］     20     0      0      0
#>   3:    Age 1     1   〈20.0, 30.0］     30 23659    115  38786
#>   4:    Age 1     1   〈30.0, 40.0］     40 18349      1  51496
#>   5:    Age 1     1   〈40.0, 50.0］     50     0      0      0
#>  ---                                                           
#> 139:  Age 13+    13   〈60.0, 70.0］     70     0      0      0
#> 140:  Age 13+    13   〈70.0, 80.0］     80   368      0     46
#> 141:  Age 13+    13   〈80.0, 90.0］     90 96931  92568 379081
#> 142:  Age 13+    13  〈90.0, 100.0］    100 99512 167288 487483
#> 143:  Age 13+    13 〈100.0, 110.0］    110 62730  56932 253720
#>      CatchAtAgeLength
#>                 <num>
#>   1:                0
#>   2:                0
#>   3:            12961
#>   4:             8530
#>   5:                0
#>  ---                 
#> 139:                0
#> 140:               48
#> 141:           215306
#> 142:           297411
#> 143:           134328
```
