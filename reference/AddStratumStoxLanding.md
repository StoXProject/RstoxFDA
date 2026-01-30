# Adds Strata to StoxLandingData

Adds a column to StoxLandingData with the spatial strata each row
belongs to.

## Usage

``` r
AddStratumStoxLanding(
  StoxLandingData,
  StratumPolygon,
  ColumnName = character()
)
```

## Arguments

- StoxLandingData:

  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
  data which will be annotated. Needs postions appended. See details.

- StratumPolygon:

  Definition of spatial strata. See
  [`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)

- ColumnName:

  specifies which column the area should be added to. See details.
  Defaults to
  `r RstoxFDA::stoxFunctionAttributes$AddStratumStoxLanding$functionParameterDefaults$ColumnName`.

## Value

StoxLandingData with column appended. See
[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html).

## Details

The strata are added to the new column specified by ColumName.

If 'ColumnName' is 'Area', The column 'Area' of StoxLandingData is
overwritten. Other columns in StoxLandingData may not be specified.

[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
does not contain columns for positions, these need to be added as
columns 'Latitude' and 'Longitude' before calling this function.
[`AddAreaPositionStoxLanding`](AddAreaPositionStoxLanding.md) may be
used to append positions, based on area codes.

## See also

[`DefineStratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/DefineStratumPolygon.html)
for configuring stratum definitions.
[`AddAreaPositionStoxLanding`](AddAreaPositionStoxLanding.md) for adding
positions to landings, [`AddStratumStoxBiotic`](AddStratumStoxBiotic.md)
for similar function for sample data,
[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of 'Stratum' as
an effect in Reca-estimation,
[`DefineCarNeighbours`](DefineCarNeighbours.md) for obtaining a
neighbour-definition for using 'Stratum' as CAR-effect in
Reca-estimation. and [`ReportFdaSampling`](ReportFdaSampling.md) for use
of 'Stratum' as an aggregation variable when comparing sampling with
landed volume.
