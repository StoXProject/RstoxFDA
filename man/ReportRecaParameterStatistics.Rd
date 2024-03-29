% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxReportFunctions.R
\name{ReportRecaParameterStatistics}
\alias{ReportRecaParameterStatistics}
\title{Report summary statistics for Reca paramters}
\usage{
ReportRecaParameterStatistics(
  RecaParameterData,
  ParameterizationSummaryData,
  AppendReport = FALSE
)
}
\arguments{
\item{RecaParameterData}{Simulated Reca parameters}

\item{ParameterizationSummaryData}{summary of Reca parameters that the results should be appended to. Optional.}

\item{AppendReport}{if true, the results are appended to another report provided by 'ParameterizationSummaryData'}
}
\value{
\code{\link[RstoxFDA]{ParameterizationSummaryData}}
}
\description{
Reports means and variances over iterations run for Reca parameterization, which may be used as input to convergence checks.
}
\details{
Multiple chains may be aggregated into one summary table, by repeatedly applying this
function with the aggregated result provided as the argument 'ParameterizationSummaryData'.
This requires that chains are different, and that they are run for the same number of iterations.

Parameters in the summary are identified with the following notation:
\<model name\>-\<covariate name\>-\<any covariate value/level\>:\<any Age group\> \<parameter type\>,
e.g: 'ProportionAtAgeModel-Area:47:Age:2 Intercept ' for
the intercept of age 2 in area 47 in the Proportion-At-Age model.
}
\seealso{
\code{\link[RstoxFDA]{ParameterizeRecaModels}} for model parameterisation
\code{\link[RstoxFDA]{ReportParameterConvergence}} for convergence checks.
}
\concept{StoX-Reca functions}
\concept{StoX-functions}
\concept{convergence-checks}
