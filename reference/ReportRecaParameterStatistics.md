# Report summary statistics for Reca paramters

Reports means and variances over iterations run for Reca
parameterization, which may be used as input to convergence checks.

## Usage

``` r
ReportRecaParameterStatistics(
  RecaParameterData,
  ParameterizationSummaryData,
  AppendReport = FALSE
)
```

## Arguments

- RecaParameterData:

  Simulated Reca parameters

- ParameterizationSummaryData:

  summary of Reca parameters that the results should be appended to.
  Optional.

- AppendReport:

  if true, the results are appended to another report provided by
  'ParameterizationSummaryData'

## Value

[`ParameterizationSummaryData`](ParameterizationSummaryData.md)

## Details

Multiple chains may be aggregated into one summary table, by repeatedly
applying this function with the aggregated result provided as the
argument 'ParameterizationSummaryData'. This requires that chains are
different, and that they are run for the same number of iterations.

Parameters in the summary are identified with the following notation:
\\model name\\-\\covariate name\\-\\any covariate value/level\\:\\any
Age group\\ \\parameter type\\, e.g: 'ProportionAtAgeModel-Area:47:Age:2
Intercept ' for the intercept of age 2 in area 47 in the
Proportion-At-Age model.

## See also

[`ParameterizeRecaModels`](ParameterizeRecaModels.md) for model
parameterisation
[`ReportParameterConvergence`](ReportParameterConvergence.md) for
convergence checks.
