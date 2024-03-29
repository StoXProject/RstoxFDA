% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxBaselineFunctions.R
\name{AddGearGroupStoxBiotic}
\alias{AddGearGroupStoxBiotic}
\title{Add Gear group to StoxBioticData}
\usage{
AddGearGroupStoxBiotic(StoxBioticData, Translation)
}
\arguments{
\item{StoxBioticData}{\code{\link[RstoxData]{StoxBioticData}} data which will be annotated.}

\item{Translation}{Translation table (\code{\link[RstoxData]{Translation}}). See details.}
}
\value{
StoxBioticData with column 'GearGroup' appended. See \code{\link[RstoxData]{StoxBioticData}}.
}
\description{
Adds a column to StoxBioticData with gear groups
}
\details{
Gear groups are defined as translations from a gear code to a gear group.
Translation need to be defined for the VariableName 'Gear' (see \code{\link[RstoxData]{Translation}}).
The provided Translation should map values ('Gear' in Translation) for the variable 'Gear'on the table 'Haul' in 'StoxBioticData' to a gear group ('NewValue' in Translation).
The gear group will be added to 'StoxBioticData' as the column 'GearGroup'

For comparison or co-analysis with landing data, a \code{\link[RstoxData]{Translation}} should be defined for \code{\link[RstoxData]{StoxLandingData}}
with the same gear groups.
}
\seealso{
\code{\link[RstoxData]{DefineTranslation}} for configuring translation to gear groups,
\code{\link[RstoxFDA]{AddGearGroupStoxLanding}} for similar function for landing data,
\code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of 'GearGroup' as an effect in Reca-estimation,
and \code{\link[RstoxFDA]{ReportFdaSampling}} for use of 'GearGroup' as an aggregation variable when comparing sampling with landed volume.
}
\concept{StoX-functions}
\concept{gear coding functions}
