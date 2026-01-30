# Estimate parameters for each Primary Sampling Unit

Estimate abundance, frequencies, totals and means for each Primary
Sampling Unit (PSU) in a multi-stage sampling design, by strata and
domains.

## Usage

``` r
AnalyticalPSUEstimate(
  StoxBioticData,
  IndividualSamplingParametersData,
  Variables = character(),
  DomainVariables = character(),
  PSUDomainVariables = character()
)
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  with the actual observations of individuals.

- IndividualSamplingParametersData:

  [`IndividualSamplingParametersData`](IndividualSamplingParametersData.md)
  with sampling parameters for individuals

- Variables:

  names of variables that means and totals should be estimated for. Must
  be numeric columns of the Individual table in 'StoxBioticData'

- DomainVariables:

  names of variables that define domains of individuals that estimates
  should be made for. Must be columns of 'Individual' or some higher
  level table in 'StoxBioticData'.

- PSUDomainVariables:

  names of variables that define groups of PSUs to be annotated on the
  results for later processing. Must be columns of 'Individual' or some
  higher level table in 'StoxBioticData', and must have a unique value
  for each PSU.

## Value

[`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md) with
estimates for each PSU of abundance, frequencies, totals and means by
stratum and domain.

## Details

Provides estimates of abundance, frequencies, totals and means by a
Horvitz-Thompson estimator. Abundance and totals are only provided if
inclusion probabilities are known, while frequencies and means may be
calculated with only sampling weights. See
[`IndividualSamplingParametersData`](IndividualSamplingParametersData.md).

Results may be combined into population level estimates with
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md). For
this reason it is also possible to provide groups of PSUs to be
annotated on the output (the argument 'PSUDomainVariables'). PSU domains
has no effect on estimation, but are merely annotated on the results for
further processing or reporting.

Sampling parameters for the selection of individuals from a catch can be
inferred for some common sub-sampling techniques with the function
[`ComputeIndividualSamplingParameters`](ComputeIndividualSamplingParameters.md).

If any strata are specified in the SampleTable of
'IndividualSamplingParametersData' but are not sampled per the
SelectionTable all estimates will be provided as NAs for this stratum.

In general unbiased estimates rely on known inclusion probabilites, and
domain definitions that coincides with stratification. When the domain
definitions are not aligned with the stratification, ratio estimates are
provided for which unbiasedness is not guaranteed.

Abundances, frequencies, totals, and means are estimated with the
formulas below. A vocabulary of notation is provided after the
equations.

- Abundance::

  The estimate of the number of individuals in stratum \\s\\ and domain
  \\d\\ at a PSU:
  \$\$\hat{N}^{(s,d)}=\sum\_{i=1}^{m}\frac{1}{\pi\_{i}}I^{s,d}\_{i}\$\$
  The inclusion probability is a function of the entire sample selection
  for a stratum. If the domain does not coincide with stratum, it must
  be considered approximate and hence this will be a ratio estimation in
  that case.

- Frequency::

  The estimate of the fraction of individuals in stratum \\s\\ that are
  in domain \\d\\ at a PSU:
  \$\$\hat{f}^{(s,d)}=\sum\_{i=1}^{m}w\_{i}I^{s,d}\_{i}\$\$

- Total::

  The estimate of the total of a variable \\v\\ in stratum \\s\\ and
  domain \\d\\ at a PSU:
  \$\$\hat{t}^{(s,d,v)}=\sum\_{i=1}^{m}\frac{y^{v}\_{i}}{\pi\_{i}}I^{s,d}\_{i}\$\$

- Mean::

  The mean value of a variable \\v\\ in stratum \\s\\ and domain \\d\\
  at a PSU:
  \$\$\hat{\mu}^{(s,d,v)}=\frac{1}{\hat{D}^{(s,d)}}\sum\_{i=1}^{m}w\_{i}y^{v}\_{i}I^{s,d}\_{i}\$\$

&nbsp;

- \\I^{(s,d)}\_{i}\\:

  The indicator function for domain \\d\\ and stratum \\s\\. Is 1 when
  \\i\\ is in stratum \\s\\ and domain \\d\\, otherwise it is zero.

- \\m\\:

  The total number of individuals sampled at PSU.

- \\\pi\_{i}\\:

  The inclusion probability of individual \\i\\ in PSU.

- \\w\_{i}\\:

  The normalized Horvitz-Thompson sample weight of an individual \\i\\.

- \\y^{v}\_{i}\\:

  The value of a variable \\v\\ observed for an individual \\i\\.

- \\\hat{D}^{(s,d)}\\:

  The estimated relative domain size of domain \\d\\ in stratum \\s\\ at
  PSU: \\\sum\_{i=1}^{m}w\_{i}I^{s,d}\_{i}\\

## See also

[`ComputeIndividualSamplingParameters`](ComputeIndividualSamplingParameters.md)
