# PSU Sampling Design Parameters

Sampling parameters for selection of Primary Sampling Units

## Details

Encodes information about the selection of Primary Sampling Units in
multi-stage sampling, used in analytical design based estimation.
Information is encoded in three tables.

The SampleTable encodes information about the sample of sampling units:

- Stratum:

  Mandatory, chr: Identifies the stratum the sample is taken from. Treat
  unstratified sample as single-stratum sampling (provide only one
  stratum.

- N:

  Optional, num: The total number of PSUs in Stratum (total available
  for selection, not total selected)

- n:

  Optional, num: The number of PSUs selected from the Stratum

- SelectionMethod:

  Mandatory, chr: 'Poission', 'FSWR' or 'FSWOR'. The manner of selection
  for use in bootstrap or inference of inclusionProbabilities,
  selectionProbabilites, co-inclusion probabilities or co-selection
  probabilities.

- FrameDescription:

  Optional, chr: Free text field describing the sampling frame.

The SelectionTable encodes information abut the selection of sampling
units for sampling:

- Stratum:

  Mandatory, chr: Identifies the stratum the PSU is taken from.

- Order:

  Optional, num: Identifes the order of seleciton. May be necessary for
  inference when selections are not independent (e.g. FSWOR)

- SamplingUnitId:

  Optional, chr: Identifes PSU. NA encodes non-response

- InclusionProbability:

  Optional, num: The inclusion probability of the PSU

- HTsamplingWeight:

  Optional, num: The normalized Horvitz-Thompson sampling weight of the
  PSU

- SelectionProbability:

  Optional, num: The selection probability of the PSU

- HHsamplingWeight:

  Optional, num: The normalized Hansen-Hurwitz sampling weight of the
  PSU

- SelectionDescription:

  Optional, chr: Free text description of the PSU.

The StratificationVariables table encodes information about which
columns in the sampleTable are stratification variables (if any):

- Stratum:

  Mandatory, chr: Identifies the stratum. In addition the Stratum is
  identified by the combination of all other columns on this table.

- \<StratificationVariables\>:

  Mandatory if present (may not contain NAs), chr: Additional columns in
  the sampleTable that are stratification variables.

The Stratification Variables assist in matching each strata to census
data, such as landings. See for instance
[`AnalyticalRatioEstimate`](AnalyticalRatioEstimate.md) The column
'Stratum' uniquely identifies a row in the table
'StratificationVariables'

Optional columns may be NA.

The selection methods available for 'SelectionMethod' are explained
here:

- Poission:

  Poission sampling. Selection is performed randomly without
  replacement, and each selection is performed individually. Sample size
  is not fixed, and 'n' represents the expected sample size.

- FSWR:

  Fixed sample size with replacement. A random selection of a fixed
  sample size 'n' is chosen with replacement

- FSWOR:

  Fixed sample size without replacement. A random selection of a fixed
  sample size 'n' is chosen without replacement. Order of selection
  could be specified in the 'selectionTable'

&nbsp;

- The SelectionProbability is defined as::

  The probability of selecting the sampling unit when it was selected
  from the population.

- The HHsamplingWeight::

  The normalized sampling weight, or the fraction of the stratum
  represented by the sampled unit when estimating with the
  Hansen-Hurwitz strategy: 1 / (SelectionProbability\*Q) , where Q is
  the sum of the reciprocal of the SelectionProbabilites for the sampled
  units. For equal probability sampling with replacement, this is simply
  1/n, where n i sample size.

- The InclusionProbability is defined as::

  The probability of the sampling unit being included in the sample.

- The HTsamplingWeight::

  The normalized sampling weight, or the fraction of the stratum
  represented by the sample when estimating with the Horvitz-Thompson
  strategy: 1 / (InclusionProbability\*P), where P is the sum of the
  reciprocal of the InclusionProbabilites for the sampled units. For
  equal probability sampling without replacement, this is simply 1/n,
  where n is sample size.
