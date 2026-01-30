# Add Gear group to StoxBioticData

Adds a column to StoxBioticData with gear groups

## Usage

``` r
AddGearGroupStoxBiotic(StoxBioticData, Translation)
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  data which will be annotated.

- Translation:

  Translation table
  ([`Translation`](https://rdrr.io/pkg/RstoxData/man/Translation.html)).
  See details.

## Value

StoxBioticData with column 'GearGroup' appended. See
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html).

## Details

Gear groups are defined as translations from a gear code to a gear
group. Translation need to be defined for the VariableName 'Gear' (see
[`Translation`](https://rdrr.io/pkg/RstoxData/man/Translation.html)).
The provided Translation should map values ('Gear' in Translation) for
the variable 'Gear'on the table 'Haul' in 'StoxBioticData' to a gear
group ('NewValue' in Translation). The gear group will be added to
'StoxBioticData' as the column 'GearGroup'

For comparison or co-analysis with landing data, a
[`Translation`](https://rdrr.io/pkg/RstoxData/man/Translation.html)
should be defined for
[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
with the same gear groups.

## See also

[`DefineTranslation`](https://rdrr.io/pkg/RstoxData/man/DefineTranslation.html)
for configuring translation to gear groups,
[`AddGearGroupStoxLanding`](AddGearGroupStoxLanding.md) for similar
function for landing data,
[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of 'GearGroup'
as an effect in Reca-estimation, and
[`ReportFdaSampling`](ReportFdaSampling.md) for use of 'GearGroup' as an
aggregation variable when comparing sampling with landed volume.
