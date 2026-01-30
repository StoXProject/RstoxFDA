# Sampling Report data (ReportFdaSamplingData)

list with tow members:

- GroupingVariables:

  a [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with the variables used for aggregation in 'FisheriesSampling' stored
  in the column 'GroupingVariables'

- SamplingVariables:

  a [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with the variables used for partitioning samples in
  'FisheriesSampling' stored in the column 'SamplingVariables'

- FisheriesSampling:

  a [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  described below.

FisheriesSampling is a report of sampling against total landings for
partitions of a fishery. The report is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns:

- ...:

  A column for each of the provided Aggregation variables

- ...:

  A column for each of the provided Sampling variables

- LandedRoundWeight:

  Total landings in kg

- Catches:

  Number of unique catches sampled

- Vessels:

  Number of unique vessels sampled

- WeightMeasurements:

  Number of fished measured for weight

- LengthMeasurements:

  Number of fished measured for length

- AgeReadings:

  Number of fished with age determined

- WeightOfSampledCatches:

  Total weight of the sampled catches
