# Compute PSU Sampling Design Parameters

Compute sampling parameters for Primary Sampling Units in multi-stage
sampling.

## Usage

``` r
ComputePSUSamplingParameters(
  StoxBioticData,
  DefinitionMethod = c("AdHocStoxBiotic", "ProportionalPoissonSampling"),
  SamplingUnitId = character(),
  StratificationColumns = character(),
  StratumName = character(),
  Quota = numeric(),
  ExpectedSampleSize = numeric()
)
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  Sample data to construct design parameters from

- DefinitionMethod:

  'AdHocStoxBiotic' or 'ProportionalPoissonSampling'

- SamplingUnitId:

  name of column in 'StoxBioticData' that identifies the Primary
  Sampling Unit the design is constructed for.

- StratificationColumns:

  name of columns that are to be used to define Strata for sampling.
  (for DefinitionMethod 'AdHocStoxBiotic'). See
  [`PSUSamplingParametersData`](PSUSamplingParametersData.md)

- StratumName:

  name of the stratum sampling parameters are calculated for (for
  DefinitionMethod 'ProportionalPoissonSampling')

- Quota:

  expected total catch in sampling frame in kg (for DefinitionMethod
  'ProportionalPoissonSampling')

- ExpectedSampleSize:

  the expected sample size for Possion sampling (for DefinitionMethod
  'ProportionalPoissonSampling')

## Value

[`PSUSamplingParametersData`](PSUSamplingParametersData.md)

## Details

Computes Sampling Design Parameters from data, given some assumptions
specified by the 'DefinitionMethod'.

Primary Sampling Units (the argument 'SamplingUnitId') may be identified
by any categorical (character, factor) or ordinal (factor, integer)
variable in 'StoxBioticData', that are associated with the
'Sample'-table or any table above 'Sample' in the StoxBiotic-hierarchy.

If 'DefinitionMethod' is 'AdHocStoxBiotic', equal probability sampling
with fixed sample size will be assumed withing strata, as well as
selection with replacement and complete response. This is a reasonable
approximation if within-strata sampling is approximately simple random
selections, the sample intensitiy is low (only a small fraction of the
population is sampled), and non-response is believed to be random.
StratificationVariables may be any categorical or ordinal variables in
'StoxBioticData', that are associated with the 'Sample'-table or any
table above 'Sample' in the StoxBiotic-hierarchy, and strata will be
defined as the combination of these variables. Primary Sampling Units
must be selected withing stratum, so the function will fail there are
several strata in one PSU.

If 'DefinitionMethod' is 'ProportionalPoissonSampling', Unstratified
(singe stratum) Poission sampling with selection probabilities
proportional to catch size is assumed. 'SamplingUnitId' must be a
variable on the Haul table of 'StoxBioticData' for this option, and the
data must contain only one species (SpeciesCategory in
'StoxBioticData'). SelectionProbabilities are assigned based on the
total catch of the species in each haul. Specifically, for a haul \\i\\;
selectionprobabilites, \\p\_{i}\\ and inclusionprobabilities
\\\pi\_{i}\\ are calculated as:

\$\$p\_{i}=\frac{w\_{i}}{W}\$\$

\$\$\pi\_{i}=1-(1-p\_{i})^{n}\$\$

where:

- \\w\_{i}\\ is the sum of all catch weights in haul \\i\\
  ('CatchFractionWeight' on the 'Sample' table of 'StoxBioticData')

- \\W\\ is the expected total catch in the fishery (argument 'Quota')

- \\n\\ is the expected sample size (argument 'ExpectedSampleSize')

If proportional poisson sampling was actually used to select the sampled
records in 'StoxBioticData', sampling parameters would have been
obtained prior to sampling, and it is generally preferable to obtain
these, and import those via
[`ReadPSUSamplingParameters`](ReadPSUSamplingParameters.md).
Weight-records are sometimes corrected after sampling parameters are
calculated, and proper information about non-response can not be
recalculated after the fact.

Proportional poisson sampling also allows the sampler to combine rigour
and pragmatism, by varying sampling parameters in the course of sample
selection. For instance 'n' may be changed during the sampling period,
if non-response turns out to be higher than expected. Such flexibilities
are not provided by this function, and the approximation may be severely
compromised, if such pragmatism is not accounted for.

## See also

[`ReadPSUSamplingParameters`](ReadPSUSamplingParameters.md),
[`AddPsuStratificationVariables`](AddPsuStratificationVariables.md),
[`AssignPSUSamplingParameters`](AssignPSUSamplingParameters.md),
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md)

## Examples

``` r
 # parameters for simple random haul-selection, stratified by GearGroup
 PSUparams <- ComputePSUSamplingParameters(RstoxFDA::StoxBioticDataExample, 
  "AdHocStoxBiotic", 
  "Haul", 
  "GearGroup")
  
 # parameters for haul selection proportional to catch size.
 calculatedPps <- RstoxFDA::ComputePSUSamplingParameters(RstoxFDA::CatchLotteryExample, 
    "ProportionalPoissonSampling", 
    "serialnumber", StratumName = 
    "Nordsjo", Quota = 124*1e6, 
    ExpectedSampleSize = 110)
```
