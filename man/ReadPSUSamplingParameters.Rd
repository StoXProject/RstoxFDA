% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxAnalyticalBaselineFunctions.R
\name{ReadPSUSamplingParameters}
\alias{ReadPSUSamplingParameters}
\title{Read PSU Sampling Design Parameters}
\usage{
ReadPSUSamplingParameters(FileName)
}
\arguments{
\item{FileName}{path to sampling parameters}
}
\value{
\code{\link[RstoxFDA]{PSUSamplingParametersData}}
}
\description{
Read sampling parameters for Primary Sampling Units in multi-stage sampling.
}
\details{
Reads sampling parameters from a tab delimited file with headers corresponding to those listed in
\code{\link[RstoxFDA]{PSUSamplingParametersData}}. The file format provide the data as one table, so that the information in 'sampleTable' is repeated for each entry in 'selectionTable'.
Any columns not named in \code{\link[RstoxFDA]{PSUSamplingParametersData}} are assumed to be stratification variables.
The conditions listed for the variables in \code{\link[RstoxFDA]{PSUSamplingParametersData}} are checked upon reading the data, and
execution halts with error if any are violated. Consult the examples in this documentation to see how the resource is formatted
with a stratification variable 'Species'.
}
\examples{
 # embedded example file:
 exampleFile <- system.file("testresources", 
                        "lotteryParameters", 
                        "lotteryDesignNSHstrata.txt", package="RstoxFDA")
 
 # Read example file with StoX
 PSUSamplingParametersData <- RstoxFDA::ReadPSUSamplingParameters( 
                          FileName=exampleFile)
 
 # Read example file as flat table, to illustrate formatting
 FlatSamplingParametersData <- read.csv(exampleFile, sep="\t")

}
\seealso{
\code{\link[RstoxFDA]{ComputePSUSamplingParameters}}, \code{\link[RstoxFDA]{AddPsuStratificationVariables}}, \code{\link[RstoxFDA]{AssignPSUSamplingParameters}}, \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}
}
\concept{Analytical estimation}
\concept{StoX-functions}
