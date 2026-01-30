# Report FDA sampling

Report sampling of fisheries against landings in partitions of the
fisheries.

## Usage

``` r
ReportFdaSampling(
  StoxBioticData,
  StoxLandingData,
  GroupingVariables = character(),
  Decimals = integer(),
  Unit = RstoxData::getUnitOptions("mass", conversionRange = c(1, 1e+12)),
  SamplingVariables = character()
)
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  data with samples from fisheries and approriate columns added for
  identifying corresponding landings.

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
  `r RstoxFDA::stoxFunctionAttributes$ReportFdaSampling$functionParameterDefaults$Decimals`.

- Unit:

  unit for the weights 'LandedRoundWeight' and 'WeightOfSampledCatches'.
  Defaults to
  `r RstoxFDA::stoxFunctionAttributes$ReportFdaSampling$functionParameterDefaults$Unit`

- SamplingVariables:

  Columns of 'StoxBioticData' identifying sampling variables to be use
  to partition the report. See details.

## Value

[`ReportFdaSamplingData`](ReportFdaSamplingData.md)

## Details

Sampling is reported partitioned on the provided 'GroupingVariables',
which must be present in both samples (StoxBioticData) and landings
(StoxLandingData). If samples are encoded in partitions of the fisheries
with no landings. 'LandedRoundWeight' will be NA. This may be due to
recording errors or filtering errors, but it may also be due to
comparison of similar but unequal category-definitions. For instance
area are coded in landings as dominant area for a fishing trip, while
at-sea sampling will record area of fishing operation, and the catch
from that area by subsequently be landed with another area listed as
dominant area.

Note that the columns Catches and Vessels summarize the number of unique
catches and vessels in each partition / cell. So depending on which
grouping variables are added, the sum of vessels does not have to
correspond to the total sum of vessels in the fleet. In principle the
same applies to Catches, but in practice that situation does less
commonly arise.

In addition sampling may be reported partitioned on the provided
'SamplingVariables'. For instance the variable 'IndividualSex' may be
provided to see how many samples are collected for each sex. NAs will be
treated as a separate category. When 'SamplingVariables' are provided,
each partition specified by 'GroupingVariables' will be reported several
times, so that the sum of the column 'LandedRoudnWeight' will not be the
total landings. 'SamplingVariables' must be columns that are only
present in samples (StoxBioticData), not in landings (StoxLandingData).

Rounding of numbers according to the argument 'Decimals' is done with
[`round`](https://rdrr.io/r/base/Round.html), so that negative numbers
specify rounding to powers of ten, and rounding of the digit 5 is
towards the even digit.

The units considered valid for weights are those listed for quantity
'mass' in
[`StoxUnits`](https://rdrr.io/pkg/RstoxData/man/StoxUnits.html)

## See also

[`PlotSamplingOverviewCell`](PlotSamplingOverviewCell.md),
[`PlotSamplingCoverage`](PlotSamplingCoverage.md),
[`PlotSamplingVariables`](PlotSamplingVariables.md)

## Examples

``` r
  samplingreport <- RstoxFDA::ReportFdaSampling(RstoxFDA::StoxBioticDataExample, 
      RstoxFDA::StoxLandingDataExample, GroupingVariables = c("GearGroup"), Unit = "ton")
  samplingreport$FisheriesSampling
#> Key: <GearGroup>
#>    GearGroup LandedRoundWeight Catches Vessels WeightMeasurements
#>       <char>             <num>   <int>   <int>              <int>
#> 1:   D.Seine             35175      29      20                580
#> 2:   Gillnet             24135      30      30                528
#> 3:       Jig              5418      26      22                520
#> 4:    L.Line              1674       8       8                120
#>    LengthMeasurements AgeReadings WeightOfSampledCatches
#>                 <int>       <int>                  <num>
#> 1:                580         580                    343
#> 2:                568         568                     24
#> 3:                520         519                     25
#> 4:                120         119                      3
```
