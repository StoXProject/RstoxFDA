# Analytical estimate of population parameters

Provides analytical estimates of population parameters and their
co-variances from a multi-stage sampling design. Estimates and
co-variance estimates are provided for abundance, frequencies, totals,
and means, by domain and strata.

## Usage

``` r
AnalyticalPopulationEstimate(
  PSUSamplingParametersData,
  AnalyticalPSUEstimateData,
  MeanOfMeans = F
)
```

## Arguments

- PSUSamplingParametersData:

  [`PSUSamplingParametersData`](PSUSamplingParametersData.md) with
  sampling parameters for a sample of Primary Samplig Units.

- AnalyticalPSUEstimateData:

  [`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md) with
  estimates for each of the Primary Sampling Units in
  PSUSamplingParametersData

- MeanOfMeans:

  logical. Determines which estimators are used for frequencies and
  means. See details.

## Value

[`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md)
with estimated population parameters by stratum and domain.

## Details

Estimates are provided with the Hansen-Hurwitz estimator, given
estimates for each Primary Sampling Unit (PSU), and the design
parameters for the PSU selection. See
[`AnalyticalPSUEstimate`](AnalyticalPSUEstimate.md) for how to obtain
estimates for each PSU.

Estimation of totals and abundance require selection probabilites to be
known. In cases where only sampling weights are known, the option
'MeanOfMeans' provides estimation of means and frequencies, but not
abundance and totals. These will be set to NA in such cases.

Means may be unknown for a certain domain because the estimate of
abundance and frequencies for the domain is be zero. In this case,
unknown means are provided as NaNs, as they result from dividing by
zero. Means may also be unkown because either PSU means or sampling
parameters are unknown, or the strata is not sampled. In this case means
will be provided as NA.

The stratification incorporates both stratified estimates for each PSU
and any stratified selection of PSUs. E.g. each PSU may provide
estimates by length strata, and at the same time the selection of PSUs
may be stratified by area. In that case this function would return
estimates for each combination of length stratum and area. This requires
all PSUs to provide estimates for the same strata, and the functions
halts with error if that is not the case. See
[`LiftStrata`](LiftStrata.md) for a way to infer PSU-estimates for
strata that have zero abundance. If simpler stratification is desired,
see [`CollapseStrata`](CollapseStrata.md).

The domains will be the combination of the domain the PSU belongs to
(PSU-domains), and the domains defined in estimation for later sampling
stages and encoded in 'AnalyticalPSUEstimateData'. Consider the
arguments to [`AnalyticalPSUEstimate`](AnalyticalPSUEstimate.md) if
other later stage domains are desired. For variance estimation, it will
sometimes be necessary to distinguish the domain size of the
PSU-domains. In these cases we will refer to the PSU-domain of a domain.
For example, catches may be PSUs and a domain will be defined as 'age 5
fish caught with purse seine'. The gear (Purse seine) is a property of
the catch and is naturally specified as a PSU-domain, so the PSU-domain
of "age 5 fish caught with purse seine" is "purse seine". The domain
will be the total number of PSUs in the population that caught age 5
fish with purse seine, while the domain size of the PSU domain will be
simply all purse seine catches in the population. Hence domain sizes are
always smaller than their corresponding PSU-domain sizes.

In general unbiased estimates rely on known selection probabilities, and
domain definitions that coincides with stratification. When only
sampling weights are known, or the domain definitions are not aligned
with the stratification, ratio estimates are provided for which
unbiasedness is not guaranteed.

Only between-PSU variance is accounted for, ignoring the variance
contribution from the later sampling stages. This provides and unbiased
estimate of the co-variances when PSUs are selected completely
independently. This is the case for Poission sampling or sampling with
replacement. It is also approximately true for regular sampling without
replacement when the sampling intensity is low (Only a small fraction of
the population PSUs are sampled).

For the quantities abundance, frequency of individuals, and the total
and mean of variables, an estimate \\\hat{x}\\ and an estimate of
sampling co-variances \\\widehat{CoVar}(\hat{x},\hat{y})\\ is provided.
Variances may be extracted as
\\\widehat{Var}(\hat{x})=\widehat{CoVar}(\hat{x},\hat{x})\\. Some other
useful error statistics may be derived, such as standard error estimates
as \\\widehat{SE}(\hat{x})=\sqrt{\widehat{Var}(\hat{x})}\\ and the
coefficient of variation as
\\CV(\hat{x})=\frac{\widehat{SE}(\hat{x})}{\hat{x}}\\. All estimates are
normally distributed for large enough sample sizes (asymptotically).
When sample size is large enough, confidence intervals for estimates can
be deduced from a normal distribution with \\\mu=\hat{x}\\ and
\\\sigma=\hat{SE}({\hat{x}})\\.

Abundances, frequencies, totals, and means are estimated with the
formulas below. A vocabulary of notation is provided after the
equations. Estimates of PSU domain sizes are provided without
variance-estimations. These are also documented in the vocabulary below.

Abundance:

:   The estimate of the total number of individuals in domain \\d\\ and
    stratum \\s\\: \$\$\hat{N}^{(s,d)} = \frac{1}{n^{(s)}}
    \sum\_{i=1}^{n}
    \hat{D}^{(s,d)}\frac{\hat{N}^{(s,d)}\_{i}}{p\_{i}}I^{(s,d)}\_{i}\$\$
    with co-variance: \$\$\widehat{CoVar}(\hat{N}^{(s,d\_{1})},
    \hat{N}^{(s,d\_{2})}) =
    \frac{1}{\hat{P}^{(s,d\_{1})}\hat{P}^{(s,d\_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)}
    \sum\_{i=1}^{n} I^{(s,d\_{1})}\_{i} I^{(s,d\_{2})}\_{i}
    z\_{i}^{(s,d\_{1})} z\_{i}^{(s,d\_{2})}\$\$ \$\$z\_{i}^{(s,d)} =
    \hat{D}^{(s,d)}\frac{\hat{N}^{(s,d)}\_{i}}{p\_{i}}I^{(s,d)}\_{i} -
    \hat{N}^{(s,d)}\$\$

    Covariance can only be calculated when \\p\_{i}\\ is provided, and
    will otherwise be NA.

Frequency:

:   The estimate of the fraction of individuals in stratum \\s\\ that
    are in domain \\d\\, when MeanOfMeans is false: \$\$ \hat{f}^{(s,d)}
    = \frac{\hat{N}^{(s,d)}}{\hat{N}^{(s)}} \$\$ with co-variance: \$\$
    \widehat{CoVar}(\hat{f}^{(s,d\_{1})}, \hat{f}^{(s,d\_{2})}) =
    \frac{1}{(\hat{N}^{(s)})^{2}}\widehat{CoVar}(\hat{N}^{(s,d\_{1})},
    \hat{N}^{(s,d\_{2})})\$\$

    These are ratio estimates depending on the ratio to the estimate
    value \\\hat{N}^{(s)}\\, and the error in this estimate is ignored.
    In addition, the estimate may depend on a ratio estimate for
    \\\hat{N}^{(s,d)}\\ and \\\hat{N}^{(s,d)}\_{i}\\, as explained for
    'Abundance' and in
    [`AnalyticalPSUEstimate`](AnalyticalPSUEstimate.md).

Frequency, Mean of Means:

:   The estimate of the fraction of individuals in stratum \\s\\ that
    are in domain \\d\\, when MeanOfMeans is true: \$\$ \hat{f}^{(s,d)}
    =
    \sum\_{i=1}^{n}\frac{w\_{i}}{\hat{D}^{(s,d)}}\hat{f}^{(s,d)}\_{i}I^{(s,d)}\_{i}\$\$
    with co-variance: \$\$\widehat{CoVar}(\hat{f}^{(s,d\_{1})},
    \hat{f}^{(s,d\_{2})}) =
    \frac{1}{\hat{P}^{(s,d\_{1})}\hat{P}^{(s,d\_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)}
    \sum\_{i=1}^{n} I^{(s,d\_{1})}\_{i} I^{(s,d\_{2})}\_{i}
    z\_{i}^{s,d\_{1}} z\_{i}^{s,d\_{2}}\$\$

    \$\$z\_{i}^{(s,d)} = \hat{f}\_{i}I^{(s,d)}\_{i} -
    \hat{f}^{(s,d)}\$\$

    Note that when \\d\_{1}\\ and \\d\_{2}\\ are different PSU domains,
    the covariance is zero, and when they are the same PSU domain
    \\\hat{P}^{(s,d\_{1})}=\hat{P}^{(s,d\_{2})}\\.

    These are ratio estimates depending on the ratio ratio estimation of
    \\\hat{f}^{(s,d)}\\, \\\hat{f}^{(s,d)}\_{i}\\, \\\hat{D}^{(s,d)}\\,
    and \\\hat{P}^{(s,d)}\\, and the error in these estimates are
    ignored. See comments above (\\\hat{f}^{(s,d)}\\) and in
    [`AnalyticalPSUEstimate`](AnalyticalPSUEstimate.md)
    (\\\hat{f}^{(s,d)}\_{i}\\).

Total:

:   The estimate of the total value of some variable \\v\\ in domain
    \\d\\ and stratum \\s\\.
    \$\$\hat{t}^{(s,d,v)}=\frac{1}{n^{(s,d)}}{\sum\_{i=1}^{n}}\hat{D}^{(s,d)}\frac{\hat{t}^{(s,d,v)}\_{i}}{p\_{i}}I^{(s,d)}\_{i}\$\$
    with co-variance: \$\$\widehat{CoVar}(\hat{t}^{(s,d\_{1},v\_{1})},
    \hat{t}^{(s,d\_{2},v\_{2})}) =
    \frac{1}{\hat{P}^{(s,d\_{1})}\hat{P}^{(s,d\_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)}
    \sum\_{i=1}^{n} I^{(s,d\_{1})}\_{i} I^{(s,d\_{2})}\_{i}
    z\_{i}^{(s,d\_{1},v\_{1})} z\_{i}^{(s,d\_{2},v\_{2})} \$\$.
    \$\$z\_{i}^{(s,d,v)} =
    \hat{D}^{(s,d)}\frac{\hat{t}^{(s,d,v)}\_{i}}{p\_{i}}I^{(s,d)}\_{i} -
    \hat{t}^{(s,d,v)}\$\$

    Note that when \\d\_{1}\\ and \\d\_{2}\\ are different PSU domains,
    the covariance is zero, and when they are the same PSU domain
    \\\hat{P}^{(s,d\_{1})}=\hat{P}^{(s,d\_{2})}\\.

    In the general case \\\hat{D}^{(s,d)}\\ and \\\hat{P}^{(s,d)}\\ are
    estimated by ratio estimators, and the error in these estimates are
    ignored. When \\d\\ covers all of \\s\\, \\\hat{D}^{(s,d)}=1\\ and
    \\\hat{P}^{(s,d)}=1\\is known. In this case both expressions can be
    shown to be unbiased. These quantities can only be calculated when
    \\p\_{i}\\ is provided, and will otherwise be NA.

Mean:

:   The estimate of the mean value of some variable \\v\\ in domain
    \\d\\ and stratum \\s\\, when MeanOfMeans is false:
    \$\$\hat{\mu}^{(s,d,v)} =
    \frac{\hat{t}^{(s,d,v)}}{\hat{N}^{(s,d)}}\$\$

Mean, Mean of Means:

:   The estimate of the mean value of some variable in domain \\d\\ and
    stratum \\s\\, when MeanOfMeans is true:
    \$\$\hat{\mu}^{(s,d,v)}=\sum\_{i=1}^{n}\frac{w\_{i}}{\hat{d}^{(s,d)}}\hat{\mu}\_{i}I^{(s,d,v)}\_{i}H(\hat{N}^{(s,d)}\_{i})\$\$

Mean, Covariance:

:   In either case, the covariance of means are estimated as:
    \$\$\widehat{CoVar}(\hat{\mu}^{(s,d\_{1},v\_{1})},
    \hat{\mu}^{(s,d\_{2},v\_{2})}) = \frac{1}{(\hat{d}^{(s,d\_{1} \cap
    d\_{2})})^{2}}\frac{1}{n^{(s)}(n^{(s)}-1)} \sum\_{i=1}^{n}
    I^{(s,d\_{1})}\_{i} I^{(s,d\_{2})}\_{i} z\_{i}^{(s,d\_{1},v\_{1})}
    z\_{i}^{(s,d\_{2},v\_{2})}\$\$ \$\$z\_{i}^{(s,d,v)} =
    H(\hat{f}^{(s,d)}\_{i}) (\hat{\mu}^{(s,d,v)}\_{i} -
    \hat{\mu}^{(s,d,v)})\$\$

    These are ratio estimates depending on the ratio ratio estimation of
    \\\hat{d}^{(s,d)}\\, \\\hat{d}^{(s,d\_{1} \cap d\_{2})}\\ and
    \\\hat{\mu}^{(s,d,v)}\_{i}\\, and the error in these estimates are
    ignored.

Vocabulary for notation used above:

- \\H(x)\\:

  A step function which is 1 when \\x\>0\\, otherwise it is zero.

- \\I^{(s)}\_{i}\\:

  The indicator function for stratum \\s\\. Is 1 when \\i\\ is in
  stratum \\s\\, otherwise it is zero.

- \\I^{(s,d)}\_{i}\\:

  The indicator function for domain \\d\\ and stratum \\s\\. Is 1 when
  \\i\\ is in stratum \\s\\ and domain \\d\\, otherwise it is zero.

- \\J^{(s,d)}\_{i}\\:

  The indicator function for domain \\d\\ and stratum \\s\\. Is 1 when
  \\i\\ is in stratum \\s\\ and the PSU-domain of domain \\d\\,
  otherwise it is zero.

- \\n\\:

  Sample size, the number of PSUs sampled.

- \\n^{(s)}\\:

  Stratum sample size, the number of PSUs sampled in stratum \\s\\:
  \\n\_{s}=\sum\_{i=1}^{n}I^{(s)}\_{i}\\.

- \\n^{(s,d)}\\:

  Domain sample size, the number of PSUs sampled in domaind and stratum
  \\s\\: \\n^{(s,d)}=\sum\_{i=1}^{n}I^{(s,d)}\_{i}\\. 'Samples' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\p\_{i}\\:

  The selection probability of PSU \\i\\. 'SelectionProbability' in
  [`PSUSamplingParametersData`](PSUSamplingParametersData.md).

- \\w\_{i}\\:

  The normalized Hansen-Hurwitz sampling weight:
  \\w\_{i}=\frac{1}{p\_{i}Q\_{i}}\\,
  \\Q\_{i}=\sum\_{j=1}^{n}\frac{I^{(s(i))}\_{j}}{p\_{j}}\\, where
  \\s(i)\\ denote the strata of sample \\i\\. 'HHsamplingWeight' in
  [`PSUSamplingParametersData`](PSUSamplingParametersData.md).

- \\\hat{D}^{(s,d)}\\:

  The estimated relative domain size (fraction of PSUs) of domain \\d\\
  in stratum \\s\\:
  \\\hat{D}^{(s,d)}=\sum\_{i=1}^{n}w\_{i}I^{(s,d)}\_{i}\\.

- \\\hat{P}^{(s,d)}\\:

  The estimated relative domain size (fraction of PSUs) of the
  PSU-domain of domain \\d\\ in stratum \\s\\:
  \\\hat{P}^{(s,d)}=\sum\_{i=1}^{n}w\_{i}J^{(s,d)}\_{i}\\.
  'PSURelativeDomainSize' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{M}^{(s,d)}\\:

  The estimated domain size (number of PSUs) of the PSU-domain of domain
  \\d\\ in stratum \\s\\:
  \\\hat{M}^{(s,d)}=\sum\_{i=1}^{n}\frac{1}{p\_{i}}J^{(s,d)}\_{i}\\.
  'PSURelativeDomainSize' in
  [`AnalyticalPopulationEstimateData`](AnalyticalPopulationEstimateData.md).

- \\\hat{d}^{(s,d)}\\:

  The estimated relative domain size (fraction of PSUs) that has
  observations (positive abundance or frequency) in domain \\d\\ in
  stratum \\s\\:
  \\\hat{d}^{(s,d)}=\sum\_{i=1}^{n}w\_{i}I^{(s,d)}\_{i}H(\hat{f}^{(s,d)}\_{i})\\.

- \\\hat{d}^{(s,d\_{1} \cap d\_{2})}\\:

  The estimated relative domain size (total number of PSUs) that has
  observations (positive abundance or frequency) in both domain
  \\d\_{1}\\ and \\d\_{2}\\ in stratum \\s\\:
  \$\$\hat{d}^{(s,d)}=\sum\_{i=1}^{n}w\_{i}I^{(s,d\_{1})}\_{i}I^{(s,d\_{2})}\_{i}H(\hat{f}^{(s,d\_{1})}\_{i})H(\hat{f}^{(s,d\_{2})}\_{j})\$\$.

- \\\hat{N}^{(s)}\\:

  The estimated abundance in stratum \\s\\:
  \\\hat{N}^{(s)}=\frac{1}{n\_{s}}\sum\_{i=1}^{n}\frac{\hat{N}\_{i}}{p\_{i}}I^{(s)}\_{i}\\.

- \\\hat{N}^{(s)}\_{i}\\:

  The estimated total abundance in stratum \\s\\ at PSU \\i\\.
  'Abundance' in
  [`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md).

- \\\hat{N}^{(s,d)}\_{i}\\:

  The estimated abundance in domain \\d\\ and stratum \\s\\ at PSU
  \\i\\. 'Abundance' in
  [`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md).

- \\\hat{f}^{(s,d)}\_{i}\\:

  The estimated frequency in domain \\d\\ for stratum \\s\\ at PSU
  \\i\\. 'Frequency' in
  [`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md).

- \\\hat{t}^{(s,d,v)}\_{i}\\:

  The estimated total of some variable \\v\\ in domain \\d\\ and stratum
  \\s\\ at PSU \\i\\. 'Total' in
  [`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md).

- \\\hat{\mu}^{(s,d,v)}\_{i}\\:

  The estimated mean of some variable \\v\\ in domain \\d\\ and stratum
  \\s\\ at PSU \\i\\. 'Mean' in
  [`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md).

## See also

[`ReadPSUSamplingParameters`](ReadPSUSamplingParameters.md),
[`ComputePSUSamplingParameters`](ComputePSUSamplingParameters.md),
[`AddPsuStratificationVariables`](AddPsuStratificationVariables.md),
[`AssignPSUSamplingParameters`](AssignPSUSamplingParameters.md),
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md),
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
                                       
 psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                       individualSamplingParameters, 
                                       c("IndividualRoundWeight"), c("IndividualAge"))
 popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
 
 #tabulate abundance
 abundance <- popEst$Abundance[,list(Abundance=Abundance), by=c("Stratum", "Domain")]
 #add SE and CV
 abundance <- merge(abundance, popEst$AbundanceCovariance[Domain1==Domain2,
                   list(SE=sqrt(AbundanceCovariance)), 
                   by=list(Stratum=Stratum, Domain=Domain1)])
 abundance$CV <- abundance$SE/abundance$Abundance
 
 #need to order as numeric. Domain is always a chr:
 abundance <- abundance[order(as.numeric(abundance$Domain)),]
#> Warning: NAs introduced by coercion
#> Warning: NAs introduced by coercion
 abundance
#> Key: <Stratum, Domain>
#>                                                                          Stratum
#>                                                                           <char>
#>  1: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  2: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  3: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  4: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  5: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  6: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  7: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  8: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>  9: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#> 10: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#> 11: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#> 12: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#> 13: PSU-stratum:Nordsjo Lower-stratum:sild'G05/161722.G05/126417/Clupea harengus
#>                   Domain Abundance         SE        CV
#>                   <char>     <num>      <num>     <num>
#>  1:  All/IndividualAge:1  12166215  9382254.2 0.7711728
#>  2: All/IndividualAge:10   3752383  1227890.6 0.3272296
#>  3: All/IndividualAge:11   2318774   991482.8 0.4275892
#>  4: All/IndividualAge:12   2093864   774636.5 0.3699555
#>  5: All/IndividualAge:13   1292641   637171.1 0.4929217
#>  6:  All/IndividualAge:2 516572019 56377863.5 0.1091384
#>  7:  All/IndividualAge:3 105683962 18286002.4 0.1730253
#>  8:  All/IndividualAge:4  35563904  7662909.9 0.2154687
#>  9:  All/IndividualAge:5  21331735  4606516.7 0.2159466
#> 10:  All/IndividualAge:6  73279811 18073290.9 0.2466340
#> 11:  All/IndividualAge:7  11763521  2561880.6 0.2177818
#> 12:  All/IndividualAge:8  29726914  7749066.0 0.2606751
#> 13:  All/IndividualAge:9  12937232  3291583.2 0.2544272
```
