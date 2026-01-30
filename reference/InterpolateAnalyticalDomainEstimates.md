# Interpolate means and frequencies for zero-abundance domains

Interpolate means and frequences for domains with no estimated abundance
(domains not sampled)

## Usage

``` r
InterpolateAnalyticalDomainEstimates(
  AnalyticalPopulationEstimateData,
  StoxLandingData,
  Method = c("Strict", "StratumMean"),
  DomainMarginVariables = character(),
  Epsilon = numeric()
)
```

## Arguments

- AnalyticalPopulationEstimateData:

  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
  with analytical estimates

- StoxLandingData:

  Landing data for the entire fishery / target population

- Method:

  method for inferring means and frequencies. See details.

- DomainMarginVariables:

  Domain Variables used for interpolation with the 'StratumMean' Method.
  Must be variables in StoxLandingData and Domain Variables in
  'AnalyticalPopulationEstimateData'. See details.

- Epsilon:

  factor for representing relative frequencies for 0-abundance domains,
  used for interpolation with the 'StratumMean' Method. See details.

## Value

[`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
with parameters for unsampled domains

## Details

This function infers parameters for unsampled domains that are present
in census data (landings). This function only infers precence of
unsampled domains from census-data, and provide options for some
pragmatic approximations to substitute for estimates for these domains.
The function does not introduce landed weights, or other knowledge from
landings, except for the fact that domains are present in the landings
data. All inference about unkown values are taken from the provided
estimates ('AnalyticalPopulationEstimateData').

The Domain-variables provided in the argument 'DomainMarginVariables'
are compared with landings ('StoxLandingData') to detect if the census
contains values and combinations for these variables that are not
present in the samples. Corresponding domains are then introduced into
the analytical estimates results according to the option for the
argument 'Method'

Subsequent ratio-estimation may make use of this information to also
make use of total-weight information from landings (see
`link[RstoxFDA]{AnalyticalRatioEstimate}`). For design based approaches,
unsampled domains have estimates of zero abundance, frequencies and
totals, and hence undefined means. Such zero-domains may be only
implicitly encoded in 'AnalyticalPopulationEstimateData', and this
function may make that encoding explicit for by use of the option
'Strict' for Method. The 'Strict' Method introduces unsampled domains
with abundance, total and frequencies of 0, and NaN means.

In order to prepare ratio-estimation (see.
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md)), it may be
desirable to infer some plausible values for means and frequencies of
unsampled domains. This is facilitated by the option 'StratumMean' for
'Method'. This method will calculate aggregate statistcs for each
stratum over the domain variables, and use this average for means and
frequencies for the marginal domains that have zero abundance. Marginal
domains are domains defined by combining statistics for all other domain
variables than those identified in DomainMarginVariables.

For instance, one may one to infer frequencies and means for unsampled
gears, as the mean of all sampled gears in a stratum. This can be
obtained by providing DomainMarginVariables='Gear', or unsampled
combinations of gears and quarters could be similarly specifyed by
DomainMarginVariables=c('Gear','Quarter'). The DomainMarginVariables
must be domain variables in 'AnalyticalPopulationEstimateData' and are
typically PSU-domains (see.
[`AnalyticalPSUEstimate`](AnalyticalPSUEstimate.md)).

Since frequencies are normalized to strata, they need to be
re-normalized after inclusion of positive frequencies in these domains.
In order to reflect their low abundance in the sampling frame, the
frequencies are scaled to a low-value provided in the argument
'Epsilon'. Epsilon indicate the precense of these domains, as deduced
from census-data, but should be chosen low enough to be considered
practically zero, to be consistent with the result of sampling-based
estimation. For this reason, abundances and totals are set to NA for
these domains, but frequencies need to present to capture for instance
age-distributions in the domain.

Subsequent ratio-estimation may provide relastic estimates of abundances
and relative frequencies between PSU-domains.

The domain estimates in 'AnalyticalPopulationEstimateData' are identied
by a stratum \\s\\ and a set of domain variables \\D\\. The domain
variables can be divided into the set \\M\\ and \\N\\, where \\M\\ are
the 'DomainMarginVariables'. Let \\(m+n)\\ denote a domain defined by
the combination of the variables \\m\\ and \\n\\. For each \\d \in N\\,
we define the stratum statistics:

- frequency:

  \$\$\widehat{f}^{(s,d)}=\sum\_{m \in M}\widehat{f}^{(s,d+m)}\$\$

  with covariance:

  \$\$\widehat{CoVar}(\widehat{f}^{(s,d\_{1})},
  \widehat{f}^{(s,d\_{2})})=\sum\_{m \in
  M}\widehat{CoVar}(\widehat{f}^{(s,d{1}+m)},
  \widehat{f}^{(s,d\_{2}+m)})\$\$

- mean:

  \$\$\widehat{\mu}^{(s,d,v)}=\frac{1}{\sum\_{m \in
  M}I(s,d+m)\widehat{f}^{(s,d+m)}}\sum\_{m \in
  M}I(s,d+m)\widehat{f}^{(s,d+m)}\widehat{\mu}^{(s,d+m,v)}\$\$ where
  \\v\\ denote the variable that the mean is estimated for, and
  \\I(s,d)\\ is an indicator functon that is 1 if a mean is defined for
  domain \\d\\ in stratum \\s\\, otherwise 0.

  with covariance:

  \$\$\widehat{CoVar}(\widehat{\mu}^{(s,d\_{1},v\_{1})},
  \widehat{\mu}^{(s,d\_{2},v\_{2})})=\frac{1}{\sum\_{m \in
  M}z^{s,d\_{1},d\_{2},m}}\sum\_{m \in
  M}z^{s,d\_{1},d\_{2},m}\widehat{CoVar}(\widehat{\mu}^{(s,d\_{1}+m,
  v\_{1})}, \widehat{\mu}^{(s,d\_{2}+m, v\_{2})})\$\$ where:
  \$\$z^{s,d\_{1},d\_{2},m}=I(s,d\_{1}+m)I(s,d\_{2}+m)\widehat{f}^{(s,d\_{1}+m)}\widehat{f}^{(s,d\_{2}+m)}\$\$

These stratum statistics are used to interpolate to zero-abundance
domains, as follows. Let \\D'\\ denote the combinations of
'DomainMarginVariables' that have zero estimated abundance. For each \\d
\in N\\ and \\n' \in D'\\, statistics are defined as:

- frequency:

  \$\$\widehat{f}^{(s,d+n')}=\epsilon\widehat{f}^{(s,d)}\$\$ where
  \\\epsilon\\ corresponds to the argument 'Epsilon'

  with covariance:

  \$\$\widehat{CoVar}(\widehat{f}^{(s,d\_{1}+n')},
  \widehat{f}^{(s,d\_{2}+n')})=\epsilon^{2}\widehat{CoVar}(\widehat{f}^{(s,d\_{1})},
  \widehat{f}^{(s,d\_{2})})\$\$

- mean:

  \$\$\widehat{\mu}^{(s,d+n',v)}=\widehat{\mu}^{(s,d,v)}\$\$

  with covariance:

  \$\$\widehat{CoVar}(\widehat{\mu}^{(s,d\_{1}+n',v\_{1})},
  \widehat{\mu}^{(s,d\_{2}+n',v\_{2})})=\widehat{CoVar}(\widehat{\mu}^{(s,d\_{1},v\_{1})},
  \widehat{\mu}^{(s,d\_{2},v\_{2})})\$\$

To renormalize frequencies, a minor adjustment is also made to the
non-zero abundance domains:

Let \\D''\\ denote the combinations of 'DomainMarginVariables' that have
positive estimated abundance. For each \\d \in N\\ and \\n'' \in D''\\,
the frequency is defined as:

\$\$\widehat{f{\*}}^{(s,d+n'')}=(1-\epsilon
r)\widehat{f}^{(s,d+n'')}\$\$ where \\r=\frac{\|D'\|}{\|D''\|}\\, the
ratio of zero-abundance domains to non-zero-abundance marginal domains.

Similarly, the covariance is:
\$\$\widehat{CoVar^{\*}}(\widehat{f}^{(s,d\_{1}+n'')},
\widehat{f}^{(s,d\_{2}+n'')}=(1-\epsilon
r)^{2}\widehat{CoVar}(\widehat{f}^{(s,d\_{1}+n'')},
\widehat{f}^{(s,d\_{2}+n'')})\$\$

## See also

[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md),
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md),
[`AggregateAnalyticalEstimate`](AggregateAnalyticalEstimate.md),
[`ExtendAnalyticalSamplingFrameCoverage`](ExtendAnalyticalSamplingFrameCoverage.md)
