# Define CAR neighbours

Define which values of a categorical variable are to be considered
neighbours, when used as a CAR-variable (Conditional AutoRegressive
variable) in Reca.

## Usage

``` r
DefineCarNeighbours(
  processData,
  DefinitionMethod = c("ResourceFile", "StratumPolygon"),
  FileName = character(),
  StratumPolygon,
  UseProcessData = F
)
```

## Arguments

- processData:

  [`CarNeighbours`](CarNeighbours.md) as returned from this function

- DefinitionMethod:

  'ResourceFile' or 'StratumPolygon'. See details.

- FileName:

  path to file for resource

- StratumPolygon:

  Definition of spatial strata that neighbours should be calculated for
  ([`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)).

- UseProcessData:

  If TRUE, bypasses execution of function and returns existing
  'processData'

## Value

Area Neighbour Definition, see: [`CarNeighbours`](CarNeighbours.md).

## Details

A CAR-variable (condititional autoregressive variable) in Reca is
typically a spatial variable reflecting location of a catch.
[`CarNeighbours`](CarNeighbours.md) defines which values (e.g. areas)
are to be considered neighbouts in parameterisation. For spatial
variables this is typically configured as geographic neighbours, but
other definitions are possible. Geomtric neighbour may be calcuated from
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
if that is defines a spatial variable, by selecting the appropriate
'DefinitionMethod'.

For DefinitionMethod 'ResourceFile': Definitions are read from a tab
separated file with headers. Columns defined as:

- Column 1: 'CarValue':

  Value for the CAR-variable (key)

- Column 2: 'Neigbhour':

  Comma-separated list of neighbours (each should occur in Column 1)

The neighbour definition must be symmetric If a is among the neighbours
of b, b must also be among the neighbours of a.

For DefinitionMethod 'StratumPolygon': A
[`CarNeighbours`](CarNeighbours.md) table will be calculated from the
provided 'StratumPolygon'. Strata that are geographic neighbours
(touching each other) will be considered neighbours. runing time and
correctness of calcuation may depend on the quality and complexity of
the 'StratumPolygon'.

## See also

[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of the
definition in Reca-estimates, and
[`DefineStratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/DefineStratumPolygon.html)
for how to define a spatial variable from a strata-definition.
