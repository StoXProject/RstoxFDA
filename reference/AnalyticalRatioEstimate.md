# Ratio estimate to census landings

Performs ratio estimate adjustments of estimates, based on frequency and
mean weights of each domain.

## Usage

``` r
AnalyticalRatioEstimate(
  AnalyticalPopulationEstimateData,
  StoxLandingData,
  WeightVariable = character(),
  StratificationVariables = character(),
  DomainVariables = character()
)
```

## Arguments

- AnalyticalPopulationEstimateData:

  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
  with estimates of mean or total weights and frequencies or abundance
  in domains

- StoxLandingData:

  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
  with census data on total weight in each stratum

- WeightVariable:

  character() name of variable in 'AnalyticalPopulationEstimateData'
  that represent weight of individuals in grams.

- StratificationVariables:

  vector of stratification columns to include when matching estimates to
  landings. Variable must exist as Stratification Variable in
  'AnalyticalPopulationEstimateData' and in 'StoxLandingData'

- DomainVariables:

  vector of domain columns to include when matching estimates to
  landings. Variable must exist as Domain Variable in
  'AnalyticalPopulationEstimateData' and in 'StoxLandingData'

## Value

[`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
with ratio estimates.

## Details

Ratio estimates of abundance are obtained by relating abundance to
weight and utilizing census data on landed weight to potentially improve
estimates. Ratio estimation generally incurs some bias in estimation,
and analytical expressions for variances are approximate.

Landings are partitioned and assigned to domains in
'AnalyticalPopulationEstimateData' by matching column names. Column
names in 'StoxLandingData' that are also either Stratification Columns
or Domain Columns in 'AnalyticalPopulationEstimateData' can be used to
construct the 'landing partitions' that provide total weigths for the
ratio estimates.

Ratio estimation of abundance may either improve an estimate of
abundance obtained by other means, or provide an estimate of abundance
when only proportions in domains are known. This requires that landing
partitions are not covering more than one strata, although they may
cover less. That is the Stratification columns in
'StratificationColumns' must identify strata in
'AnalyticalPopulationEstimateData'

The function obtains a ratio estimate of total abundance in landings by
relating proportion in domains, mean weight in domains, and total weight
in landings to each other:

'Abundance' will be estimated as \\\widehat{qN}^{(s,d)}\\, the variable
'Frequency' as \\\widehat{qf}^{(s,d)}\\, and the variable 'Total' as
\\\widehat{qt}^{(s,d,v)}\\. These estimators and their corresponding
variances ('AbundanceCovariance', 'FrequencyCovariance', and
'TotalCovariance') are given below. Note that no revised estimate for
means are provided with this method. 'Mean' and MeanCovariance' are
unchanged. The estimates are based on estimated frequencies and ratio of
of total landings in each landing partition to the estimated mean
individual weight ('WeightVariable'):
\$\$\hat{Q}^{(s,d)}=\frac{W^{(L)}}{\sum\_{(s',d') \in
L}\hat{f}^{(s',d')}\hat{\mu}^{(s',d',\mathrm{w})}}\$\$ where
\\L=part(s,d)\\ is a partition of the landings containing the domain,
specified by the arguments 'StratificationVariables' and
'DomainVariables'. Since frequencies are normalized to strata, L cannot
contain several strata. The abundance is estimated as:
\$\$\widehat{qN}^{(s,d)} = \hat{f}^{(s,d)}\hat{Q}^{(s,d)}\$\$ And
covariances are estimated as:
\$\$\widehat{CoVar}(\widehat{qN}^{(s,d\_{1})},
\widehat{qN}^{(s,d\_{2})}) =
\hat{Q}^{(s,d\_{1})}\hat{Q}^{(s,d\_{2})}\widehat{CoVar}(\hat{f}^{(s,d\_{1})},
\hat{f}^{(s,d\_{2})})\$\$ ignoring the error in \\\hat{Q}^{(s,d)}\\. The
frequency is estimated as: \$\$\widehat{qf}^{(s,d)} =
\frac{\widehat{qN}^{(s,d)}}{\widehat{qN}^{(s)}}\$\$ And covariances are
estimated as: \$\$\widehat{CoVar}(\widehat{qf}^{(s,d\_{1})},
\widehat{qf}^{(s,d\_{2})}) =
\frac{1}{\widehat{qN}^{(s)}}\widehat{CoVar}(\widehat{qN}^{(s,d\_{1})},
\widehat{qN}^{(s,d\_{2})})\$\$ ignoring the error in
\\\widehat{qN}^{(s)}\\. Totals are estimated as:
\$\$\widehat{qt}^{(s,d,v)} = \widehat{qN}^{(s,d)}\hat{\mu}^{(s,d,v)}\$\$
And covariances are estimated as:
\$\$\widehat{CoVar}(\widehat{qt}^{(s,d\_{1},v\_{1})},
\widehat{qt}^{(s,d\_{2},v\_{2})}) =
\widehat{qN}^{(s,d\_{1})}\widehat{qN}^{(s,d\_{2})}\widehat{CoVar}(\hat{\mu}^{(s,d\_{1},v\_{1})},
\hat{\mu}^{(s,d\_{2},v\_{2})})\$\$ ignoring the error in
\\\widehat{qN}^{(s,d)}\\. Note that no revised estimate for means are
provided with this method. 'Mean' and MeanCovariance' is unchanged.

Vocabulary for equations given above:

- \\part(s,d)\\:

  The partition of the landings containing the domain \\d\\ in stratum
  \\s\\.

- \\\hat{N}^{(s,d)}\\:

  The estimated abundance in the domain \\d\\ in stratum \\s\\.
  'Abundance' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{t}^{(s,d,v)}\\:

  The estimated total of variable \\v\\ in domain \\d\\ in stratum
  \\s\\. The 'Total' for the 'Variable' \\v\\ in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{t}^{(s,d,\mathrm{w})}\\:

  The estimated total weight in domain \\d\\ in stratum \\s\\. The
  'Total' for the 'Variable' identified by the argument 'WeightVariable'
  in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{\mu}^{(s,d,v)}\\:

  The estimated mean of the variable \\v\\ in domain \\d\\ in stratum
  \\s\\. The 'Mean' for the 'Variable' \\v\\ in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{\mu}^{(s,d,\mathrm{w})}\\:

  The estimated mean weight in domain \\d\\ in stratum \\s\\. The 'Mean'
  for the 'Variable' identified by the argument 'WeightVariable' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{t}^{(L,\mathrm{w})}\\:

  The estimated total weight in the landing partition:
  \\\hat{t}^{(L)}=\sum\_{(s,d) \in L}\hat{t}^{(s,d,\mathrm{w})}\\

- \\\widehat{rN}^{(s)}\\:

  The estimated total abundance in stratum \\s\\, based on total domain
  weight estimates:
  \\\widehat{rN}^{(s)}=\sum\_{d}\widehat{rN}^{(s,d)}\\, where the sum
  runs over all domains in stratum \\s\\.

- \\\widehat{qN}^{(s)}\\:

  The estimated total abundance in stratum \\s\\, based on mean domain
  weight estimates:
  \\\widehat{qN}^{(s)}=\sum\_{d}\widehat{qN}^{(s,d)}\\, where the sum
  runs over all domains in stratum \\s\\.

- \\\widehat{CoVar}(\hat{N}^{(s,d\_{1})}, \hat{N}^{(s,d\_{2})})\\:

  The estimated covariance of abundance between the domains \\d\_{1}\\
  and \\d\_{2}\\ in stratum \\s\\. 'AbundanceCovariance' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{f}^{(s,d)}\\:

  The estimated frequency in domain \\d\\ in stratum \\s\\. 'Frequency'
  in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\widehat{CoVar}(\hat{f}^{(s,d\_{1})}, \hat{f}^{(s,d\_{2})})\\:

  The estimated covariance of frequencies between the domains \\d\_{1}\\
  and \\d\_{2}\\ in stratum \\s\\. 'FrequencyCovariance' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\widehat{CoVar}(\hat{t}^{(s,d\_{1},v\_{1})},
  \hat{f}^{(s,d\_{2},v\_{2})})\\:

  The estimated covariance of totals between the variable \\v\_{1}\\ in
  domain \\d\_{1}\\ and the variable \\v\_{2}\\ in domain \\d\_{2}\\ in
  stratum \\s\\. 'TotalCovariance' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\widehat{CoVar}(\hat{\mu}^{(s,d\_{1},v\_{1})},
  \hat{f}^{(s,d\_{2},v\_{2})})\\:

  The estimated covariance of means between the variable \\v\_{1}\\ in
  domain \\d\_{1}\\ and the variable \\v\_{2}\\ in domain \\d\_{2}\\ in
  stratum \\s\\. 'MeanCovariance' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

## See also

[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md),
[`AggregateAnalyticalEstimate`](AggregateAnalyticalEstimate.md),
[`ExtendAnalyticalSamplingFrameCoverage`](ExtendAnalyticalSamplingFrameCoverage.md),
[`InterpolateAnalyticalDomainEstimates`](InterpolateAnalyticalDomainEstimates.md)

## Examples

``` r
 PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
                                       RstoxFDA::CatchLotterySamplingExample, 
                                       RstoxFDA::CatchLotteryExample, 
                                       "serialnumber", "Haul", "MissingAtRandom")
 individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
                                       RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))
 
 # Estimate for the domain 'CountryVessel'                                      
 psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                       individualSamplingParameters, 
                                       c("IndividualRoundWeight"), 
                                       c("IndividualAge"))
 popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
 
 # perform ration estimate, assigning total weights by 'CountryVessel'
 ratioEst <- RstoxFDA::AnalyticalRatioEstimate(popEst, 
                                        RstoxFDA::CatchLotteryLandingExample, 
                                        "IndividualRoundWeight", 
                                        "CountryVessel")
 
```
