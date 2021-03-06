% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxDataTypes.R
\name{ReportFdaSamplingData}
\alias{ReportFdaSamplingData}
\title{Sampling Report data (ReportFdaSamplingData)}
\description{
list with tow members:
 \describe{
  \item{AggregationVariables}{a \code{\link[data.table]{data.table}} with the variables used for aggregation in 'FishereisSampling' stored in the column 'AggregationVariables'}
  \item{FisheriesSampling}{a \code{\link[data.table]{data.table}} described below.}
 }
 
 FisheriesSampling is a report of sampling against total landings for partitions of a fishery.
 The report is a \code{\link[data.table]{data.table}} with columns:
 \describe{
  \item{...}{A column for each of the provided Aggregation variables}
  \item{LandedRoundWeight}{Total landings in kg}
  \item{Catches}{Number of catches sampled}
  \item{Vessels}{Number of vessels sampled}
  \item{WeightMeasurments}{Number of fished measured for weight}
  \item{LengthMeasurments}{Number of fished measured for length}
  \item{AgeReadings}{Number of fished with age determined}
  \item{WeightOfSampledCatches}{Total weight of the sampled catches}
 }
}
