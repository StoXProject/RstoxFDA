# Reca Data (RecaData)

Data and some data parameters prepared for running `eca.estimate` and
`eca.predict` via [`RunRecaEstimate`](RunRecaEstimate.md).

## Details

A list with five members:

- AgeLength:

  input needed for `eca.estimate` and `eca.predict`. List of
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)

- WeightLength:

  input needed for `eca.estimate` and `eca.predict`. List of
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)

- Landings:

  input needed for `eca.estimate` and `eca.predict`. List of
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)

- GlobalParameters:

  input needed for `eca.estimate` and `eca.predict`. see details. List
  of [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)

- CovariateMaps:

  Mapping of values for each covariate in landings and samples
  (including non-configurable catchId) to integer value used in R-ECA.
  List of
  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
