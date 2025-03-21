% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxAnalyticalBaselineFunctions.R
\name{AnalyticalPSUEstimate}
\alias{AnalyticalPSUEstimate}
\title{Estimate parameters for each Primary Sampling Unit}
\usage{
AnalyticalPSUEstimate(
  StoxBioticData,
  IndividualSamplingParametersData,
  Variables = character(),
  DomainVariables = character(),
  PSUDomainVariables = character()
)
}
\arguments{
\item{StoxBioticData}{\code{\link[RstoxData]{StoxBioticData}} with the actual observations of individuals.}

\item{IndividualSamplingParametersData}{\code{\link[RstoxFDA]{IndividualSamplingParametersData}} with sampling parameters for individuals}

\item{Variables}{names of variables that means and totals should be estimated for. Must be numeric columns of the Individual table in 'StoxBioticData'}

\item{DomainVariables}{names of variables that define domains of individuals that estimates should be made for. Must be columns of 'Individual' or some higher level table in 'StoxBioticData'.}

\item{PSUDomainVariables}{names of variables that define groups of PSUs to be annotated on the results for later processing. Must be columns of 'Individual' or some higher level table in 'StoxBioticData', and must have a unique value for each PSU.}
}
\value{
\code{\link[RstoxFDA]{AnalyticalPSUEstimateData}} with estimates for each PSU of abundance, frequencies, totals and means by stratum and domain.
}
\description{
Estimate abundance, frequencies, totals and means for each Primary Sampling Unit (PSU)
in a multi-stage sampling design, by strata and domains.
}
\details{
Provides estimates of abundance, frequencies, totals and means by a Horvitz-Thompson estimator.
Abundance and totals are only provided if inclusion probabilities are known, while frequencies and means may be calculated
with only sampling weights. See \code{\link[RstoxFDA]{IndividualSamplingParametersData}}.

Results may be combined into population level estimates with \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}. For this
reason it is also possible to provide groups of PSUs to be annotated on the output (the argument 'PSUDomainVariables').
PSU domains has no effect on estimation, but are merely annotated on the results for further processing or reporting.

Sampling parameters for the selection of individuals from a catch can be inferred for some common sub-sampling techniques
with the function \code{\link[RstoxFDA]{ComputeIndividualSamplingParameters}}.

If any strata are specified in the SampleTable of 'IndividualSamplingParametersData' but are not sampled per the SelectionTable
all estimates will be provided as NAs for this stratum.

In general unbiased estimates rely on known inclusion probabilites, and domain definitions that coincides
with stratification. When the domain definitions are not aligned
with the stratification, ratio estimates are provided for which unbiasedness is not guaranteed.

Abundances, frequencies, totals, and means are estimated with the formulas below. A vocabulary of notation is provided after the equations.
\describe{
\item{Abundance:}{
The estimate of the number of individuals in stratum \eqn{s} and domain \eqn{d} at a PSU:
\deqn{\hat{N}^{(s,d)}=\sum_{i=1}^{m}\frac{1}{\pi_{i}}I^{s,d}_{i}}
The inclusion probability is a function of the entire sample selection for a stratum.
If the domain does not coincide with stratum, it must be considered approximate and hence this will be a ratio estimation in that case.
}
\item{Frequency:}{
The estimate of the fraction of individuals in stratum \eqn{s} that are in domain \eqn{d} at a PSU:
\deqn{\hat{f}^{(s,d)}=\sum_{i=1}^{m}w_{i}I^{s,d}_{i}}
}
The sampling weight is a function of the entire sample selection for a stratum.
If the domain does not coincide with stratum, it must be considered approximate and hence this will be a ratio estimation in that case.
\item{Total:}{
The estimate of the total of a variable \eqn{v} in stratum \eqn{s} and domain \eqn{d} at a PSU:
\deqn{\hat{t}^{(s,d,v)}=\sum_{i=1}^{m}\frac{y^{v}_{i}}{\pi_{i}}I^{s,d}_{i}}
}
The inclusion probability is a function of the entire sample selection for a stratum.
If the domain does not coincide with stratum, it must be considered approximate and hence this will be a ratio estimation in that case.
\item{Mean:}{
The mean value of a variable \eqn{v} in stratum \eqn{s} and domain \eqn{d} at a PSU:
\deqn{\hat{\mu}^{(s,d,v)}=\frac{1}{\hat{D}^{(s,d)}}\sum_{i=1}^{m}w_{i}y^{v}_{i}I^{s,d}_{i}}
}
This depends explicitly on the ratio to the estimate of relative domain size. When the domain coincides with strata
this is in principle known, but in practice reported strata sizes for samples of individuals are often estimated, bringing
into question the exact computation of inclusion probabilities.
In addition, the sampling weight is a function of the entire sample selection for a stratum.
If the domain does not coincide with stratum, it must also be considered approximate and an additional ratio-estimated quantity.
}
\describe{
\item{\eqn{I^{(s,d)}_{i}}}{The indicator function for domain \eqn{d} and stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s} and domain \eqn{d}, otherwise it is zero.}
\item{\eqn{m}}{The total number of individuals sampled at PSU.}
\item{\eqn{\pi_{i}}}{The inclusion probability of individual \eqn{i} in PSU.}
\item{\eqn{w_{i}}}{The normalized Horvitz-Thompson sample weight of an individual \eqn{i}.}
\item{\eqn{y^{v}_{i}}}{The value of a variable \eqn{v} observed for an individual \eqn{i}.}
\item{\eqn{\hat{D}^{(s,d)}}}{The estimated relative domain size of domain \eqn{d} in stratum \eqn{s} at PSU: \eqn{\sum_{i=1}^{m}w_{i}I^{s,d}_{i}}}
}
}
\seealso{
\code{\link[RstoxFDA]{ComputeIndividualSamplingParameters}}
}
\concept{Analytical estimation}
