# Analytical Population Estimate Data

Analytical estimates for a population

Abundance

- Stratum:

  Stratum that abundance is provided for.

- Domain:

  The domain that the abundance is provided for.

- Abundance:

  The estimated number of individuals in the domain and stratum.

- Frequency:

  The estimated fraction of individuals in the domain, relative ot the
  total number in stratum.

Variables

- Stratum:

  The stratum that estimates are provided for.

- Domain:

  The domain that estimates are provided for.

- Variable:

  The variable (measurment) that estimates are provided for.

- Total:

  The estimated total value of the variable in domain and stratum.

- Mean:

  The estimated mean value fo the variable in domain and stratum.

AbundanceCovariance

- Stratum:

  The stratum that covariances are provided for.

- Domain1:

  A domain that covariances are provided for. Unique, given Stratum

- Domain2:

  A domain that covariances are provided for. Unique, given Stratum and
  Domain 1.

- AbundanceCovariance:

  The estimated covariance of abundance between Domain1 and Domain2.

- FrequencyCovariance:

  The estimated covariance of frequency between Domain1 and Domain2.

VariableCovariance

- Stratum:

  The stratum that covariances are provied for.

- Domain1:

  A domain that covariances are provided for. Unique, given Stratum

- Domain2:

  A domain that covariances are provided for. Unique, given Stratum and
  Domain 1.

- Variable1:

  A variable that covariances are provided for. Unique, given Stratum
  and domains.

- Variable2:

  A variable that covariances are provided for. Unique, given Stratum,
  domains and Variable1.

- TotalCovariance:

  The estimated covariance of total value of Variable1 in Domain1 and
  Variable2 in Domain2

- MeanCovariance:

  The estimated covariance of the mean value of Variable1 in Domain1 and
  Variable2 in Domain2

StratificationVariables

- Stratum:

  A stratum, as identified in other tables.

- \<StratificationVariables\>:

  Columns that relate the stratum to data records.

The columns \<StratificationVariables\> are optional, but if present;
their combination must identify a stratum. The Stratification Variables
assist in matching each strata to census data, such as landings. See for
instance [`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md) Unlike
[`PSUSamplingParametersData`](PSUSamplingParametersData.md),
[`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md), and,
[`IndividualSamplingParametersData`](IndividualSamplingParametersData.md),
a stratum for 'AnalyticalPopulationEstimateData' can be defined by
several rows in the 'StratificationVariables' table.

DomainVariables

- Domain:

  A domain, as identified in other tables.

- DomainVariables:

  Columns that relate the domain to data records.

SampleCount

- Stratum:

  A stratum, as identified in other tables.

- Domain:

  A domain, as identified in other tables.

- nPSU:

  The number of Primary Sampling Units observed for this domain in this
  stratum

- nIndividuals:

  The number of individuals observed for this domain in this stratum
