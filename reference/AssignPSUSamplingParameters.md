# Assign PSU Sampling Parameters

Assigns data records to PSU Sampling Parameters, provides non-response
adjustments for selected PSUs that was not sampled, and changes
SamplingUnitId to that used to identify data records.

## Usage

``` r
AssignPSUSamplingParameters(
  PSUSamplingParametersData,
  StoxBioticData,
  SamplingUnitId = character(),
  DataRecordId = character(),
  DefinitionMethod = c("MissingAtRandom")
)
```

## Arguments

- PSUSamplingParametersData:

  [`PSUSamplingParametersData`](PSUSamplingParametersData.md) with
  sampling parameters for PSU selection

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  with data records for responding PSUs.

- SamplingUnitId:

  name of Variable in
  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  that represent the SamplingUnitId of PSUs selected for sampling

- DataRecordId:

  name of Variable in
  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  that represent records of sampled PSUs

- DefinitionMethod:

  The method for dealing with non-response, e.g. 'MissingAtRandon'

## Value

[`PSUSamplingParametersData`](PSUSamplingParametersData.md) without
non-respondent and with 'SamplingUnitId' changed

## Details

Some sampling parameters provided in
[`PSUSamplingParametersData`](PSUSamplingParametersData.md) are only
interpretable for sampling with complete response. This function adjusts
these parameters, removes non-respondents from the
[`PSUSamplingParametersData`](PSUSamplingParametersData.md), and checks
that all responding PSUs are present in data records.

After correcting for non-response, the SamplingUnitId in
[`PSUSamplingParametersData`](PSUSamplingParametersData.md) will be
replaced by an ID (argument 'DataRecordId') so that sampling units can
be brought into correspondence with how they are identified in lower
level sampling
([`IndividualSamplingParametersData`](IndividualSamplingParametersData.md))

If any respondents (rows of the SelectionTable of
PSUSamplingParametersData that does not have NA for SamplingUnitId) are
not found in 'SamplingUnitId', execution halts with an error.

Response after selection can generally be considered a process that
modifies the sampling parameters that are set by design. Typically
sample size, InclusionProbabilities and normalized SamplingWeights need
to be adjusted as non-respondents are removed, since these are depend of
the entire sample, not just the sampling unit they are assigned to.
SelectionProbabilites are by definition set for a single draw of a
single sampling unit from the population and are valid even when
response is not complete.

Treatment of non-response requires some assumption about systematic
differences between respondents and non-respondents. These assumptions
are specified via the argument 'DefinitionMethod' and the following
options are available:

- MissingAtRandom:

  A response propensity is estimated for each stratum as the fraction of
  the sample resonding, and sample size (n) and InclusionProbability are
  adjusted by multiplying with this propensity. Sampling weights are
  adjusted by dividing them with their sum over repsondents in a
  stratum.

## See also

[`ReadPSUSamplingParameters`](ReadPSUSamplingParameters.md),
[`ComputePSUSamplingParameters`](ComputePSUSamplingParameters.md),
[`AddPsuStratificationVariables`](AddPsuStratificationVariables.md),
[`ComputeIndividualSamplingParameters`](ComputeIndividualSamplingParameters.md),
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md)
