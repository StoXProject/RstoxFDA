% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxAnalyticalBaselineFunctions.R
\name{InterpolateAnalyticalDomainEstimates}
\alias{InterpolateAnalyticalDomainEstimates}
\title{Interpolate means and frequencies for zero-abundance domains}
\usage{
InterpolateAnalyticalDomainEstimates(
  AnalyticalPopulationEstimateData,
  StoxLandingData,
  Method = c("Strict", "StratumMean"),
  DomainMarginVariables = character(),
  Epsilon = numeric()
)
}
\arguments{
\item{AnalyticalPopulationEstimateData}{\code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with analytical estimates}

\item{StoxLandingData}{Landing data for the entire fishery / target population}

\item{Method}{method for inferring means and frequencies. See details.}

\item{DomainMarginVariables}{Domain Variables used for interpolation with the 'StratumMean' Method. Must be variables in StoxLandingData and Domain Variables in 'AnalyticalPopulationEstimateData'. See details.}

\item{Epsilon}{factor for representing relative frequencies for 0-abundance domains, used for interpolation with the 'StratumMean' Method. See details.}
}
\value{
\code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with parameters for unsampled domains
}
\description{
Interpolate means and frequences for domains with no estimated abundance (domains not sampled)
}
\details{
This function infers parameters for unsampled domains that are present in census data (landings).
This function only infers precence of unsampled domains from census-data, and provide options for some pragmatic approximations to substitute for estimates for these domains.
The function does not introduce landed weights, or other knowledge from landings,
except for the fact that domains are present in the landings data. All inference about unkown values are taken from the provided estimates ('AnalyticalPopulationEstimateData').

The Domain-variables provided in the argument 'DomainMarginVariables' are compared with landings ('StoxLandingData')
to detect if the census contains values and combinations for these variables that are not present in the samples.
Corresponding domains are then introduced into the analytical estimates results according to the option for the argument 'Method'

Subsequent ratio-estimation may make use of this information to also make use of total-weight information from landings (see \code{link[RstoxFDA]{AnalyticalRatioEstimate}}).
For design based approaches, unsampled domains have estimates of zero abundance, frequencies and totals, and hence undefined means.
Such zero-domains may be only implicitly encoded in 'AnalyticalPopulationEstimateData',
and this function may make that encoding explicit for by use of the option 'Strict' for Method.
The 'Strict' Method introduces unsampled domains with abundance, total and frequencies of 0, and NaN means.

In order to prepare ratio-estimation (see. \code{\link[RstoxFDA]{AnalyticalRatioEstimate}}), it may be desirable to infer some plausible values for means and frequencies of unsampled domains.
This is facilitated by the option 'StratumMean' for 'Method'. This method will calculate aggregate statistcs for each stratum over the
domain variables, and use this average for means and frequencies for the marginal domains that have zero abundance. Marginal domains are
domains defined by combining statistics for all other domain variables than those identified in DomainMarginVariables.

For instance, one may one to infer frequencies and means for unsampled gears, as the mean of all sampled gears in a stratum.
This can be obtained by providing DomainMarginVariables='Gear', or unsampled combinations of gears and quarters could be similarly
specifyed by DomainMarginVariables=c('Gear','Quarter'). The DomainMarginVariables must be domain variables in 'AnalyticalPopulationEstimateData'
and are typically PSU-domains (see. \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}).

Since frequencies are normalized to strata, they need to be re-normalized after inclusion of positive frequencies in these domains.
In order to reflect their low abundance in the sampling frame, the frequencies are scaled to a low-value provided in the argument 'Epsilon'.
Epsilon indicate the precense of these domains, as deduced from census-data, but should be chosen low enough to be considered practically zero,
to be consistent with the result of sampling-based estimation. For this reason, abundances and totals are set to NA for these domains,
but frequencies need to present to capture for instance age-distributions in the domain.

Subsequent ratio-estimation may provide relastic estimates of abundances and relative frequencies between PSU-domains.

The domain estimates in 'AnalyticalPopulationEstimateData' are identied by a stratum \eqn{s} and a set of domain variables \eqn{D}.
The domain variables can be divided into the set \eqn{M} and \eqn{N}, where \eqn{M} are the 'DomainMarginVariables'. Let \eqn{(m+n)}
denote a domain defined by the combination of the variables \eqn{m} and \eqn{n}.
For each \eqn{d \in N}, we define the stratum statistics:
\describe{
\item{frequency}{
\deqn{\widehat{f}^{(s,d)}=\sum_{m \in M}\widehat{f}^{(s,d+m)}}

with covariance:

\deqn{\widehat{CoVar}(\widehat{f}^{(s,d_{1})}, \widehat{f}^{(s,d_{2})})=\sum_{m \in M}\widehat{CoVar}(\widehat{f}^{(s,d{1}+m)}, \widehat{f}^{(s,d_{2}+m)})}
}
\item{mean}{
\deqn{\widehat{\mu}^{(s,d,v)}=\frac{1}{\sum_{m \in M}I(s,d+m)\widehat{f}^{(s,d+m)}}\sum_{m \in M}I(s,d+m)\widehat{f}^{(s,d+m)}\widehat{\mu}^{(s,d+m,v)}}
where \eqn{v} denote the variable that the mean is estimated for, and \eqn{I(s,d)} is an indicator functon that is 1
if a mean is defined for domain \eqn{d} in stratum \eqn{s}, otherwise 0.

with covariance:

\deqn{\widehat{CoVar}(\widehat{\mu}^{(s,d_{1},v_{1})}, \widehat{\mu}^{(s,d_{2},v_{2})})=\frac{1}{\sum_{m \in M}z^{s,d_{1},d_{2},m}}\sum_{m \in M}z^{s,d_{1},d_{2},m}\widehat{CoVar}(\widehat{\mu}^{(s,d_{1}+m, v_{1})}, \widehat{\mu}^{(s,d_{2}+m, v_{2})})}
where:
\deqn{z^{s,d_{1},d_{2},m}=I(s,d_{1}+m)I(s,d_{2}+m)\widehat{f}^{(s,d_{1}+m)}\widehat{f}^{(s,d_{2}+m)}}
}
}

These stratum statistics are used to interpolate to zero-abundance domains, as follows.
Let \eqn{D'} denote the combinations of 'DomainMarginVariables' that have zero estimated abundance.
For each \eqn{d \in N} and \eqn{n' \in D'}, statistics are defined as:

\describe{
\item{frequency}{
\deqn{\widehat{f}^{(s,d+n')}=\epsilon\widehat{f}^{(s,d)}}
where \eqn{\epsilon} corresponds to the argument 'Epsilon'

with covariance:

\deqn{\widehat{CoVar}(\widehat{f}^{(s,d_{1}+n')}, \widehat{f}^{(s,d_{2}+n')})=\epsilon^{2}\widehat{CoVar}(\widehat{f}^{(s,d_{1})}, \widehat{f}^{(s,d_{2})})}
}
\item{mean}{
\deqn{\widehat{\mu}^{(s,d+n',v)}=\widehat{\mu}^{(s,d,v)}}

with covariance:

\deqn{\widehat{CoVar}(\widehat{\mu}^{(s,d_{1}+n',v_{1})}, \widehat{\mu}^{(s,d_{2}+n',v_{2})})=\widehat{CoVar}(\widehat{\mu}^{(s,d_{1},v_{1})}, \widehat{\mu}^{(s,d_{2},v_{2})})}
}
}

To renormalize frequencies, a minor adjustment is also made to the non-zero abundance domains:

Let \eqn{D''} denote the combinations of 'DomainMarginVariables' that have positive estimated abundance.
For each \eqn{d \in N} and \eqn{n'' \in D''}, the frequency is defined as:

\deqn{\widehat{f{*}}^{(s,d+n'')}=(1-\epsilon r)\widehat{f}^{(s,d+n'')}}
where \eqn{r=\frac{|D'|}{|D''|}}, the ratio of zero-abundance domains to non-zero-abundance marginal domains.

Similarly, the covariance is:
\deqn{\widehat{CoVar^{*}}(\widehat{f}^{(s,d_{1}+n'')}, \widehat{f}^{(s,d_{2}+n'')}=(1-\epsilon r)^{2}\widehat{CoVar}(\widehat{f}^{(s,d_{1}+n'')}, \widehat{f}^{(s,d_{2}+n'')})}
}
\seealso{
\code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}, \code{\link[RstoxFDA]{AnalyticalRatioEstimate}}, \code{\link[RstoxFDA]{AggregateAnalyticalEstimate}}, \code{\link[RstoxFDA]{ExtendAnalyticalSamplingFrameCoverage}}
}
\concept{Analytical estimation}
