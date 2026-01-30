# Report FDA landings

Report landings in partitions of the fisheries.

## Usage

``` r
ReportFdaLandings(
  StoxLandingData,
  GroupingVariables = character(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("mass", conversionRange = c(1, 1e+12))
)
```

## Arguments

- StoxLandingData:

  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
  data with landings from fisheries and approriate columns added for
  identifying corresponding samples

- GroupingVariables:

  Columns of 'StoxBioticData' and 'StoxLandingData' that partitions the
  fisheries. If not provided, a single row for all landings will be
  produced.

- Decimals:

  integer specifying the number of decimals to report for
  'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportFdaLandings$functionParameterDefaults$Decimals`.

- Unit:

  unit for the weights 'LandedRoundWeight' and 'WeightOfSampledCatches'.
  Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportFdaLandings$functionParameterDefaults$Unit`.

## Value

[`ReportFdaLandingData`](ReportFdaLandingData.md)

## Details

Landings are reported partitioned on the provided 'GroupingVariables'.

Landings are sorted by decreasing weight, except if the column
'CatchDate' is used as GroupingVariable. In that case Landings are
sorted by increasing date, after sorting on other Grouping variables.

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for weights are those listed for quantity
'mass' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Examples

``` r
 landingsreport <- RstoxFDA::ReportFdaLandings(RstoxFDA::StoxLandingDataExample, 
    GroupingVariables = c("Area"), Unit="ton")
 landingsreport$FisheriesLandings
#>      Area LandedRoundWeight
#>    <char>             <num>
#> 1:     05             20366
#> 2:     03             18374
#> 3:     04             13409
#> 4:     06              7514
#> 5:     00              6740
```
