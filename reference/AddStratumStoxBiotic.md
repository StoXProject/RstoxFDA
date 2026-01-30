# Adds Stratum to StoxBioticData

Adds a column 'Stratum' to StoxBioticData with the spatial strata each
row belongs to.

## Usage

``` r
AddStratumStoxBiotic(StoxBioticData, StratumPolygon, ColumnName = character())
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  data which will be annotated. Needs postions appended. See details.

- StratumPolygon:

  Definition of spatial strata. See
  [`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)

- ColumnName:

  specifies which column the area should be added to. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$AddStratumStoxBiotic$functionParameterDefaults$ColumnName`.

## Value

StoxBioticData with column appended to data.table 'Station'. See
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html).

## Details

The strata are added to the new column 'Stratum'.

## See also

[`DefineStratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/DefineStratumPolygon.html)
for configuring stratum definitions.
[`AddStratumStoxLanding`](AddStratumStoxLanding.md) for similar function
for landing data, [`PrepareRecaEstimate`](PrepareRecaEstimate.md) for
use of 'Stratum' as an effect in Reca-estimation,
[`DefineCarNeighbours`](DefineCarNeighbours.md) for obtaining a
neighbour-definition for using 'Stratum' as CAR-effect in
Reca-estimation. and [`ReportFdaSampling`](ReportFdaSampling.md) for use
of 'Stratum' as an aggregation variable when comparing sampling with
landed volume.
