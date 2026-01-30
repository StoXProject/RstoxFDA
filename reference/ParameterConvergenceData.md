# Convergence Report for simulated parameters (ParameterConvergenceData)

Convergence Report for multi-chained simulated parameters, such as MCMC
simulations with Reca. 'chains' in this respect refers to statistically
independent simulations.

list with one members 'ConvergenceReport', which is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
containing the following columns:

- Parameter:

  Identifies the parameter that is summarized.

- InterVariance:

  The Mean-Squared-Deviation the means of the parameter in each chain,
  to the mean across all chains

- IntraVariance:

  The mean of the within-chain variances of the parameter

- GelmanRubinR:

  Gelman-Rubins R

## Details

Gelman-Rubins R is described by Gelman and Rubin (Statistical Science,
1992): DOI: https://doi.org/10.1214/ss/1177011136

## See also

[`RunRecaModels`](RunRecaModels.md) for running Reca-analysis

[`ReportParameterConvergence`](ReportParameterConvergence.md) for
creating ParameterConvergenceData.
