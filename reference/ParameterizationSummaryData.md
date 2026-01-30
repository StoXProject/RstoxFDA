# Summary statistics for simulated parameters (ParameterizationSummaryData)

Summary statistics for (potentially multi-chained) simulated parameters,
such as MCMC simulations with Reca. 'chains' in this respect refers to
statistically independent simulations.

list with two members 'ParameterSummary' and 'RunParameters', which both
all [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)s.
'ParameterSummary' contains parameter statistics for simulations and
have the following columns:

- Parameter:

  Identifies the parameter that is summarized.

- Mean:

  Mean of the parameter

- Variance:

  Variance of the parameter

- chainId:

  Identifies the parameterization chain.

RunParameters summarizes global control parameters for each chain and
has columns:

- chainId:

  Identifies the parameterization chain.

- Iterations:

  The number of iterations for parameter simulation

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis

[`ReportRecaParameterStatistics`](ReportRecaParameterStatistics.md) for
creating ParameterizationSummaryData from Reca-simulations.
