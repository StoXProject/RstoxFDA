# Add Period to StoxLandingData

Add a column to StoxLandingData with a temporal category, such as
quarters.

## Usage

``` r
AddPeriodStoxLanding(
  StoxLandingData,
  TemporalDefinition,
  ColumnName = character()
)
```

## Arguments

- StoxLandingData:

  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
  data which will be annotated.

- TemporalDefinition:

  [`TemporalDefinition`](TemporalDefinition.md) definition of temporal
  category.

- ColumnName:

  Name of the added column. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$AddPeriodStoxLanding$functionParameterDefaults$ColumnName`.

## Value

StoxLandingData with column appended. See
[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html).

## Details

A column with a temporal category will be added based on the column
'CatchDate' in 'StoxLandingData'. Temporal definitions
([`TemporalDefinition`](TemporalDefinition.md)) may be produced by
[`DefinePeriod`](DefinePeriod.md)

Since it may be useful to use different temporal categories for instance
for parameterization and reporting two choices are offered for the name
of the added column (see argument 'ColumnName').

## See also

[`DefinePeriod`](DefinePeriod.md) for configuring the temporal
definition, [`AddStratumStoxBiotic`](AddStratumStoxBiotic.md) for
similar function for sample data,
[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of 'Period' as
an effect in Reca-estimation, and
[`ReportFdaSampling`](ReportFdaSampling.md) for use of 'Period' as an
aggregation variable when comparing sampling with landed volume.
