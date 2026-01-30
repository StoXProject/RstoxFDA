# Add Period to StoxBioticData

Add a column 'Period' to 'Station' on StoxBioticData with a temporal
category, such as 'quarter'.

## Usage

``` r
AddPeriodStoxBiotic(
  StoxBioticData,
  TemporalDefinition,
  ColumnName = character()
)
```

## Arguments

- StoxBioticData:

  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
  data which will be annotated with period.

- TemporalDefinition:

  [`TemporalDefinition`](TemporalDefinition.md) definition of temporal
  category.

- ColumnName:

  specifies which column the area should be added to. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$AddPeriodStoxBiotic$functionParameterDefaults$ColumnName`.

## Value

StoxBioticData with column appended. See
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html).
[`DefinePeriod`](DefinePeriod.md) for configuring the temporal
definition, [`AddStratumStoxLanding`](AddStratumStoxLanding.md) for
similar function for landing data,
[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of 'Period' as
an effect in Reca-estimation, and
[`ReportFdaSampling`](ReportFdaSampling.md) for use of 'Period' as an
aggregation variable when comparing sampling with landed volume.

## Details

'Period' will be added based on the column 'DateTime' on the table
'Station' in 'StoxBioticData'.

Temporal definitions ([`TemporalDefinition`](TemporalDefinition.md)) may
be produced by [`DefinePeriod`](DefinePeriod.md)
