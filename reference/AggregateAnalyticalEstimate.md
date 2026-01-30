# Aggregate Analytical Estimate across strata

Aggregates estimate across strata. One can optinally exclude some strata
from being aggregated (using the argument 'RetainStrata'). Retained
strata are returned unchanged.

Aggregation of means and frequencies across strata, requires information
about strata size (abundance in strata). For sampling programs where
these cannot be directly estimated, they must be inferred before
application of this function. Otherwise means and frequencies will be
NA. Consider for example ratio-estimation by strata
([`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md))

Specifically, a new stratum \\s'\\ (named according to
'AggregateStratumName') is defined based on the strata in \\S\\, where
\\S\\ contains all strata in 'AnalyticalPopulationEstimateData', except
those explicitly excluded by the argument 'RetainStrata'. The estimated
parameters are obtained as follows:

- Abundance::

  The estimate of the total number of individuals in domain \\d\\ and
  stratum \\s'\\: \$\$\hat{N}^{(s',d)} = \sum\_{s \in S}
  \hat{N}^{(s,d)}\$\$ with co-variance:
  \$\$\widehat{CoVar}(\hat{N}^{(s',d\_{1})}, \hat{N}^{(s',d\_{2})}) =
  \sum\_{s \in S} \widehat{CoVar}(\hat{N}^{(s,d\_{1})},
  \hat{N}^{(s,d\_{2})})\$\$

  where \\\hat{N}^{(s,d)}\\ and \\\widehat{CoVar}(\hat{N}^{(s,d\_{1})},
  \hat{N}^{(s,d\_{2})})\\ are provided by the argument
  'AnalyticalPopulationEstimateData'.

- Frequency::

  The estimate of the fraction of individuals in stratum \\s'\\ that are
  in domain \\d\\: \$\$ \hat{f}^{(s',d)} =
  \frac{\hat{N}^{(s',d)}}{\hat{N}^{(s')}} \$\$ with co-variance: \$\$
  \widehat{CoVar}(\hat{f}^{(s',d\_{1})}, \hat{f}^{(s',d\_{2})}) =
  \frac{1}{(\hat{N}^{(s')})^{2}}\widehat{CoVar}(\hat{N}^{(s',d\_{1})},
  \hat{N}^{(s',d\_{2})})\$\$

  where \\\hat{N}^{(s')}\\ is the total estimated abundance in stratum,
  across all domains.

- Total::

  The estimate of the total value of some variable \\v\\ in domain \\d\\
  and stratum \\s'\\. \$\$\hat{t}^{(s',d,v)}=\sum\_{s \in
  S}\hat{t}^{(s,d,v)}\$\$ with co-variance:
  \$\$\widehat{CoVar}(\hat{t}^{(s',d\_{1},v\_{1})},
  \hat{t}^{(s',d\_{2},v\_{2})}) = \sum\_{s \in S}
  \widehat{CoVar}(\hat{t}^{(s,d\_{1},v\_{1})},
  \hat{t}^{(s,d\_{2},v\_{2})})\$\$

  where \\\hat{t}^{(s,d,v)}\\ and
  \\\widehat{CoVar}(\hat{t}^{(s,d\_{1},v\_{1})},
  \hat{t}^{(s,d\_{2},v\_{2})})\\ are provided by the argument
  'AnalyticalPopulationEstimateData'.

- Mean::

  The estimate of the mean value of some variable \\v\\ in domain \\d\\
  and stratum \\s'\\: \$\$\hat{\mu}^{(s',d,v)} =
  \frac{1}{\hat{N}^{(s',d)}}\sum\_{s \in S}
  \hat{\mu}^{(s,d,v)}\hat{N}^{(s,d)}\$\$ with co-variance:
  \$\$\widehat{CoVar}(\hat{\mu}^{(s',d\_{1},v\_{1})},
  \hat{\mu}^{(s',d\_{2},v\_{2})}) = \sum\_{s \in S}
  \widehat{CoVar}(\hat{\mu}^{(s,d\_{1},v\_{1})},
  \hat{\mu}^{(s,d\_{2},v\_{2})}) z^{(s, d\_{1}, d\_{2})}\$\$ where
  \$\$z^{(s, d\_{1},
  d\_{2})}=\frac{\hat{N}^{(s,d\_{1})}\hat{N}^{(s,d\_{2})}}{\hat{N}^{(s',d\_{1})}\hat{N}^{(s',d\_{2})}}\$\$

## Usage

``` r
AggregateAnalyticalEstimate(
  AnalyticalPopulationEstimateData,
  RetainStrata = character(),
  AggregateStratumName = character()
)
```

## Arguments

- AnalyticalPopulationEstimateData:

  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
  with estimates to aggregate.

- RetainStrata:

  strata that should not be included in the aggregation. Must correspond
  to 'Stratum' in 'AnalyticalPopulationEstimateData'

- AggregateStratumName:

  name to use for the aggregated stratum

## Value

[`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
with aggregated estimates.

## See also

[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md),
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md),
[`ExtendAnalyticalSamplingFrameCoverage`](ExtendAnalyticalSamplingFrameCoverage.md),
[`InterpolateAnalyticalDomainEstimates`](InterpolateAnalyticalDomainEstimates.md)
