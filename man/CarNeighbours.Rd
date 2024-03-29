% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxDataTypes.R
\name{CarNeighbours}
\alias{CarNeighbours}
\title{Area Neighbour Definition (CarNeighbours)}
\description{
Table (\code{\link[data.table]{data.table}})
defining neighbours for a CAR-variable (Conditional autoregressive variable).
}
\details{
\describe{
  \item{CarValue}{Values for a variable used as CAR-variable}
  \item{Neighbours}{Comma-separated list of neighbours}
 }

 The table is symmetric, so that if b is a neighbour of a. a is also a neighbour of b.
}
\concept{Data types}
