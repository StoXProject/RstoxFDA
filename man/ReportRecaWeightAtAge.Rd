% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxReportFunctions.R
\name{ReportRecaWeightAtAge}
\alias{ReportRecaWeightAtAge}
\title{Report weight at age}
\usage{
ReportRecaWeightAtAge(
  RecaCatchAtAge,
  PlusGroup = integer(),
  IntervalWidth = numeric(),
  Decimals = integer(),
  Threshold = numeric(),
  Unit = RstoxData::getUnitOptions("mass", conversionRange = c(1e-04, 10))
)
}
\arguments{
\item{RecaCatchAtAge}{Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).}

\item{PlusGroup}{If given, ages 'PlusGroup' or older are included in a plus group.}

\item{IntervalWidth}{The width of the reported credible interval. A value of 0.9 gives 90 per cent credible intervals. Defaults to 0.9.}

\item{Decimals}{integer specifying the number of decimals to report for 'MeanIndividualWeight', 'SD', 'Low' and 'High'. Defaults to 2.}

\item{Threshold}{threshold for reporting mean weight. Rows with an estimated Catch At Age (number of individuals) lower than this will have NA reported for their mean weight. Defaults to 0.}

\item{Unit}{unit for 'MeanIndividualWeight', 'SD', 'Low' and 'High'}
}
\value{
\code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}}
}
\description{
Tabulates summary statistics for mean weights at age from MCMC simulations using Reca.

If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
area, gear, stock, etc., summary statistics will be presented similarly.

Mean weight for plus-groups are a weighted by the relative catch-at-age in each composite age group.
For iterations where all of the plus-group ages have a zero catch at age, this weight is not defined,
and summary statistics are obtained from the remaining iterations.

Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.

The units considered valid for mean weights are those listed for quantity 'mass' in \code{\link[RstoxData]{StoxUnits}}

MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
Summary statistics are obtained from the posterior distribution, and
the interval is reported as 90\% equal-tailed credible intervals.
}
\examples{
  weightAtAge <- RstoxFDA::ReportRecaWeightAtAge(RstoxFDA::RecaCatchAtAgeExample, 
       PlusGroup = 13, Threshold = 1000, Decimals = 0, Unit = "g")
  weightAtAge
}
\seealso{
\code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
}
\concept{StoX-Reca functions}
\concept{StoX-functions}
