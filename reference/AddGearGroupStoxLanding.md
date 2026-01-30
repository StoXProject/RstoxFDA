# Add Gear group to StoxLandingData

Adds a column to StoxLandingData with gear groups

## Usage

``` r
AddGearGroupStoxLanding(StoxLandingData, Translation)
```

## Arguments

- StoxLandingData:

  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
  data which will be annotated.

- Translation:

  Translation table
  ([`Translation`](https://rdrr.io/pkg/RstoxData/man/Translation.html)).
  See details.

## Value

[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
with column 'GearGroup' appended.

## Details

Gear groups are defined as translations from a gear code to a gear
group. Translation need to be defined for the VariableName 'Gear' (see
[`Translation`](https://rdrr.io/pkg/RstoxData/man/Translation.html)).
The provided Translation should map values ('Gear' in Translation) for
the variable 'Gear' in 'StoxBioticData' to a gear group ('NewValue' in
Translation). The gear group will be added to 'StoxLandingData' as the
column 'GearGroup'

For comparison or co-analysis with sample data, a
[`Translation`](https://rdrr.io/pkg/RstoxData/man/Translation.html)
should be defined for
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
with the same gear groups.

## See also

[`DefineTranslation`](https://rdrr.io/pkg/RstoxData/man/DefineTranslation.html)
for configuring translation to gear groups,
[`AddGearGroupStoxBiotic`](AddGearGroupStoxBiotic.md) for similar
function for sample data,
[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of 'GearGroup'
as an effect in Reca-estimation, and
[`ReportFdaSampling`](ReportFdaSampling.md) for use of 'GearGroup' as an
aggregation variable when comparing sampling with landed volume.
