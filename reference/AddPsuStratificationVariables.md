# Add Stratification columns to 'PSUSamplingParametersData'

Add additional variables to encode strata and its correspondance with
census data (e.g. landings data).

## Usage

``` r
AddPsuStratificationVariables(
  PSUSamplingParametersData,
  StratificationVariables,
  StratificationVariablesTable = data.table::data.table()
)
```

## Arguments

- PSUSamplingParametersData:

  Sampling parameters stratification variables should be added to

- StratificationVariables:

  name of variables to add

- StratificationVariablesTable:

  value-combinations for the variables to add to each stratum

## Value

[`PSUSamplingParametersData`](PSUSamplingParametersData.md)

## Details

[`PSUSamplingParametersData`](PSUSamplingParametersData.md) provide
sampling parameters by strata. Optionally, it may also contain
additional variables that encode the stratification in terms of
variables available in other data sources, such as
[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html).
This function allows such variables to be added, if not already present.

More detailed encoding of stratification is useful for encoding the
sampling frame of the design provided by 'PSUSamplingParametersData'. By
encoding all strata in terms of variables that are available in
census-data, the correspondance between sampling frame and target
population can be encoded. This information will be available in
downstream estimates (e.g.
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md)) and
allow for pragmatic inference to out-of-frame strata (via
[`ExtendAnalyticalSamplingFrameCoverage`](ExtendAnalyticalSamplingFrameCoverage.md)).

## See also

[`ReadPSUSamplingParameters`](ReadPSUSamplingParameters.md),
[`ComputePSUSamplingParameters`](ComputePSUSamplingParameters.md),
[`AssignPSUSamplingParameters`](AssignPSUSamplingParameters.md),
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md)
