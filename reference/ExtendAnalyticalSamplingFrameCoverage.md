# Extends estimate beyond sampling frame

Infer estimates to parts of the fishery / target population that was not
covered by sampling programs. That is strata not covered by the sampling
frame, but that are known to be populated in census data (landing data).
These are pragmatic approximations, without statistical justification.

## Usage

``` r
ExtendAnalyticalSamplingFrameCoverage(
  AnalyticalPopulationEstimateData,
  StoxLandingData,
  StratificationVariables,
  Method = c("Strict", "SetToStratum"),
  UnsampledStratum = character(),
  SourceStratum = character()
)
```

## Arguments

- AnalyticalPopulationEstimateData:

  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
  with Estimates for the sampling frame

- StoxLandingData:

  Landing data for the entire fishery / target population

- StratificationVariables:

  vector of variables in StoxLandingData that should be used to
  partition the fishery, must be Stratification Variables in
  'AnalyticalPopulationEstimateData'

- Method:

  method of inference beyond sampling frame.

- UnsampledStratum:

  name to use for unsamled stratum

- SourceStratum:

  name of the stratum to get means and frequencies for the Method
  'SetToStratum'

## Value

[`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
with parameters for unsampled stratum, and StratificationVariables
reduced to those mathced with landings

## Details

This function only infers precence of unsampled strata from census-data,
and provide options for some pragmatic approximations to substitute for
estimates for these strata. Any estimates already provided is not
changed. The function does not introduce landed weights, or other
knowledge from landings, except for the fact that strata are present in
the landings data. All inference about unkown values are taken from the
provided estimates ('AnalyticalPopulationEstimateData'). Subsequent
ratio-estimation may make use of this information to also make use of
total-weight information from landings (see
`link[RstoxFDA]{AnalyticalRatioEstimate}`).

Inference about unkown values can be done in several ways, controled by
the argument 'Method'

- Strict:

  Provide NA-values for all parameters of all domains that is not in the
  sampling frame.

- SetToStratum:

  Provide NA-values for abundance and total of all domains that is not
  in the sampling frame. Set means, frequencies, and corresponding
  variance to the same values as in a sampled strata.

For variables in 'StratificationVariables', any non-sampled landing
partition will be added as one unsampled stratum with the name provided
by 'UnsampledStratum'. Inferred statistics will be reported for all
domains for this stratum.

Inference about unsampled strata is not justified by sampling. It may be
considered applicable if the following applies:

- Other considerations indicate that frequencies and means should be
  similar between the same domains in the unsampeld stratum and the
  source stratum.

- The unsampled stratum will be agregated with sampled ones, and
  constitute a small volume compared to sampled strata. This can be
  inspected with [`ReportFdaSampling`](ReportFdaSampling.md).

## See also

[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md),
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md),
[`AggregateAnalyticalEstimate`](AggregateAnalyticalEstimate.md),
[`InterpolateAnalyticalDomainEstimates`](InterpolateAnalyticalDomainEstimates.md)
