% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/activityCensus-datadoc.R
\docType{data}
\name{activityCensus}
\alias{activityCensus}
\title{Activity census}
\format{
\code{\link[data.table]{data.table}} with columns
 \describe{
  \item{gearFAO}{gear (FAO ISSCFG 1980).}
  \item{gearNS}{gear (code based on NS 9400, Norwegian standard).}
  \item{targetFAO}{target species (FAO ASFIS).}
  \item{meshSize}{mesh size (mm, bar length).}
  \item{vesselLengthCategory}{length group for vessels ranges in m.}
  \item{species}{reported species (FAO ASFIS).}
  \item{wholeWeightKg}{total catch within activity group in kg.}
 }
}
\usage{
data(activityCensus)
}
\description{
Example of activity census.
}
\examples{
data(activityCensus)
sum(activityCensus$OfficialLandingsWeight)
}
\concept{logbook functions}
\keyword{datasets}
