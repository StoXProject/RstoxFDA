% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxAnalyticalBaselineFunctions.R
\name{AddLengthGroupStoxBiotic}
\alias{AddLengthGroupStoxBiotic}
\title{Add length group to StoxBioticData}
\usage{
AddLengthGroupStoxBiotic(
  StoxBioticData,
  LengthInterval = numeric(),
  LengthGroupVariable = character(),
  LeftOpen = TRUE
)
}
\arguments{
\item{StoxBioticData}{\code{\link[RstoxData]{StoxBioticData}} with individuals to be grouped by length}

\item{LengthInterval}{The 'bin-size', length in cm between length-groups}

\item{LengthGroupVariable}{Name to use for the length group variable}

\item{LeftOpen}{logical, specifying whether intervals are left-open, or right-open.}
}
\description{
Adds a variable that groups individuals based on the variable 'IndividualTotalLength'.
This allows length-groups to be defined as domains in analytical estimation.

The groups are defined by the argument 'LengthInterval', which specify consecutive length groups of
equal length range ('LengthInterval'), starting with length 0. The argument 'LeftOpen' specifies
whether the intervals are open to the left (lower value) or to the right (higher value).

For example LengthInterval=5, and LeftOpen=FALSE, specifies length-groups [0,5>,[5,10>,...

Note that even though the length-groups are formatted as a character, they have a strict format that
carry numerical information for downstream functions, such as \code{\link[RstoxFDA]{ReportAnalyticalCatchAtLength}}
}
\examples{
 StoxBioticWLengthGroup <- RstoxFDA:::AddLengthGroupStoxBiotic(RstoxFDA::CatchLotteryExample, 
       LengthInterval=5, LengthGroupVariable="LengthGroup", LeftOpen=TRUE)
 table(StoxBioticWLengthGroup$Individual$LengthGroup)
}
\seealso{
\code{\link[RstoxFDA]{AnalyticalPSUEstimate}}, \code{\link[RstoxFDA]{ReportAnalyticalCatchAtLength}}
}
\concept{Analytical estimation}
