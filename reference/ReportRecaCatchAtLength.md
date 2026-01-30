# Report catch at length

Tabulates summary statistics for total catch (number) at length from
MCMC simulations using Reca. MCMC simulations are typically obtained
with [`RunRecaModels`](RunRecaModels.md). Summary statistics are
obtained from the posterior distribution, and the interval is reported
as equal-tailed credible intervals.

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
ReportRecaCatchAtLength(
  RecaCatchAtAge,
  IntervalWidth = numeric(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("cardinality", conversionRange = c(1, 1e+12)),
  LengthInterval = numeric()
)
```

## Arguments

- RecaCatchAtAge:

  Results from MCMC simulations ([`RecaCatchAtAge`](RecaCatchAtAge.md)).

- IntervalWidth:

  The width of the reported credible interval. A value of 0.9 gives 90
  per cent credible intervals. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtLength$functionParameterDefaults$IntervalWidth`.

- Decimals:

  integer specifying the number of decimals to report for
  'CatchAtLength', 'SD', 'Low' and 'High'. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtLength$functionParameterDefaults$Decimals`.

- Unit:

  unit for 'CatchAtLength', 'SD', 'Low' and 'High'

- LengthInterval:

  width of length bins in cm. If not provided, the interval in
  'RecaCatchAtAge' will be used.

## Value

[`ReportFdaCatchAtLengthData`](ReportFdaCatchAtLengthData.md)

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis and
[`ReportRecaCatchAtAge`](ReportRecaCatchAtAge.md) for reporting age
composition

## Examples

``` r
 catchAtLength <- RstoxFDA::ReportRecaCatchAtLength(RstoxFDA::RecaCatchAtAgeExample,
          LengthInterval = 10)
 catchAtLength$NbyLength
#>          LengthGroup Length     SD     Low    High CatchAtLength
#>               <char>  <num>  <num>   <num>   <num>         <num>
#>  1:    〈0.0, 10.0］     10      0       0       0             0
#>  2:   〈10.0, 20.0］     20      0       0       0             0
#>  3:   〈20.0, 30.0］     30  23659     115   38786         12961
#>  4:   〈30.0, 40.0］     40  40320      90   97133         30446
#>  5:   〈40.0, 50.0］     50 193010  317237  896786        596386
#>  6:   〈50.0, 60.0］     60 943351 6781295 9744108       8240461
#>  7:   〈60.0, 70.0］     70 728594 6918890 9274598       8039776
#>  8:   〈70.0, 80.0］     80 588850 3256985 5058065       4124220
#>  9:   〈80.0, 90.0］     90 162056  899154 1374071       1096964
#> 10:  〈90.0, 100.0］    100  99626  179844  495328        316871
#> 11: 〈100.0, 110.0］    110  62730   56932  253720        134328
 
```
