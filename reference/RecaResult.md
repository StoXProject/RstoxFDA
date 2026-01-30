# Reca Results (RecaResult)

Results from running `eca.estimate` and `eca.predict` via
[`RunRecaEstimate`](RunRecaEstimate.md).

## Details

- input:

  All input data and parameters provided to `eca.estimate` and
  `eca.predict`

- fit:

  as returned by `eca.estimate`

- prediction:

  as returned by `eca.predict`

- covariateMaps:

  list() mapping from Reca covariate encoding to values fed to
  [`PrepareRecaEstimate`](PrepareRecaEstimate.md). As in
  [`RecaData`](RecaData.md)
