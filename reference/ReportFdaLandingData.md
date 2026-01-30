# Landings Report data (ReportFdaLandingData)

list with tow members:

- GroupingVariables:

  a [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with the variables used for aggregation in 'FisheriesLandings' stored
  in the column 'GroupingVariables'

- FisheriesLandings:

  a [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  described below.

FisheriesLandings is a report of landings for partitions of a fishery.
The report is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns:

- ...:

  A column for each of the provided Aggregation variables

- LandedRoundWeight:

  Total landings in kg
