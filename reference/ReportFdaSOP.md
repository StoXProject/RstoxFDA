# Report SOP test

Report sum-of-product test (SOP-test) for catch estimates.

Mean weight at age and estimated catch (numbers) at age is used to
compute total catches and the relative difference to reported landings
are reported. Missing values (NAs) are ignored (exlcuded from sums).

The report will be generated for landings decomposed on the provided
'GroupingVariables' which must be available in both
'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData' and
'StoxLandingData'.

'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData' must be
decomposed on the same 'GroupingVariables' and must be reported for the
same age groups

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for mean lengths are those listed for
quantity 'fraction' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## Usage

``` r
ReportFdaSOP(
  ReportFdaCatchAtAgeData,
  ReportFdaWeightAtAgeData,
  StoxLandingData,
  GroupingVariables = character(),
  DecimalWeight = integer(),
  DecimalFraction = integer(),
  UnitFraction = RstoxData::getUnitOptions("fraction")
)
```

## Arguments

- ReportFdaCatchAtAgeData:

  [`ReportFdaCatchAtAgeData`](ReportFdaCatchAtAgeData.md) with estimates
  of total catch at age

- ReportFdaWeightAtAgeData:

  [`ReportFdaWeightAtAgeData`](ReportFdaWeightAtAgeData.md) with
  estimates of mean weight at age for individual fish

- StoxLandingData:

  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
  data with landings from fisheries

- GroupingVariables:

  Columns of 'StoxLandingData' that partitions the landings into groups
  SOP tests should be reported for.

- DecimalWeight:

  integer specifying the number of decimals to report for weights:
  'TotalWeightEstimated', 'LandedWeight', and 'Difference'. Defaults to
  \`r
  RstoxFDA::stoxFunctionAttributes\$ReportFdaSOP\$functionParameterDefaults\$DecimalWeight\`.

- DecimalFraction:

  integer specifying the number of decimals to report for
  'RelativeDifference'. Defaults to \`r
  RstoxFDA::stoxFunctionAttributes\$ReportFdaSOP\$functionParameterDefaults\$DecimalFraction\`.

- UnitFraction:

  unit for the RelativeDifference. E.g. '0.' for decimal notation or '%'
  for percent.

## Value

[`ReportFdaSopData`](ReportFdaSopData.md)

## See also

[`ReportRecaWeightAtAge`](ReportRecaWeightAtAge.md) and
[`ReportRecaCatchAtAge`](ReportRecaCatchAtAge.md) for some ways of
preparing 'ReportFdaWeightAtAgeData' and 'ReportFdaCatchAtAgeData'.
[`StoxLanding`](https://rdrr.io/pkg/RstoxData/man/StoxLanding.html) and
[`FilterStoxLanding`](https://rdrr.io/pkg/RstoxData/man/FilterStoxLanding.html)
for ways of preparing 'StoxLandingData'.

## Examples

``` r
 catchAtAge <- RstoxFDA::ReportRecaCatchAtAge(RstoxFDA::RecaCatchAtAgeExample)
 weightAtAge <- RstoxFDA::ReportRecaWeightAtAge(RstoxFDA::RecaCatchAtAgeExample)
 sop <- ReportFdaSOP(catchAtAge, weightAtAge, RstoxFDA::StoxLandingDataExample, 
      DecimalFraction = 6)
 sop$SopReport
#>    TotalWeightEstimated LandedWeight Difference RelativeDifference
#>                   <num>        <num>      <num>              <num>
#> 1:             66441823     66402706      39117           0.000589
```
