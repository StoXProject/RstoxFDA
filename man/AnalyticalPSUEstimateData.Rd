% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxDataTypes.R
\name{AnalyticalPSUEstimateData}
\alias{AnalyticalPSUEstimateData}
\title{Analytical PSU Estimate Data}
\description{
Analytical estimates for each PSU
 
 List containing the following \code{\link[data.table]{data.table}}s:
 
 Abundance
 \describe{
   \item{SampleId}{Identfier for Primary sampling unit (PSU)}
   \item{Stratum}{Identifier for stratification of individuals}
   \item{Domain}{Identifier of domains for individuals}
   \item{Abundance}{Total number in Stratum and Domain at th PSU. Unsampled strata is reported as NA}
   \item{Frequency}{Frequency in Domain within Stratum. Unsampled strata is reported as NA}
 }
 
 Variables
 \describe{
   \item{SampleId}{Identfier for Primary sampling unit (PSU)}
   \item{Stratum}{Identifier for stratification of individuals}
   \item{Domain}{Identifier of domains for individuals}
   \item{Variable}{Variable that total and mean is reported for}
   \item{Total}{Total value of variable in Stratum and Domain at the PSU. Unsampled strata is reported as NA}
   \item{Mean}{Mean value of variable in Stratum and Domain at the PSU. Unsampled strata is reported as NA}
 }
 
 DomainVariables
 \describe{
  \item{Domain}{Identifier of domains for individuals. In addition the domain is identified by the combination of any additional columns in this table}
  \item{<DomainVariables>}{Columns that relate the domains to data records.}
 }
 
 PSUDomainVariables
 \describe{
  \item{SampleId}{Identifier for Primary Sampling Unit (PSU)}
  \item{PSUDomain}{Identfier of domains for PSUs. In addition PSU-domains are identified by the combination of any additional columns in this table.}
  \item{<DomainVariables>}{Columns that relate the PSU domains to data records.}
 }
 
 StratificationVariables
 \describe{
  \item{SampleId}{Identifier for Primary Sampling Unit (PSU)}
  \item{Stratum}{Identfier of stratum for individuals at PSU. In addition strata are identified by the combination of any additional columns in this table.}
  \item{<StratificationVariables>}{Columns that relate the PSU domains to data records.}
 }
 
 The combination of the columns 'Stratum' and 'SampleId' uniquely identifies a row in the table 'StratificationVariables'
 The columns <StratificationVariables> are optional, but if present; their combination must identify a stratum for each 'SamplingId'.
 
 SampleCount
 \describe{
  \item{SampleId}{Identifier for Primary Sampling Unit (PSU)}
  \item{Stratum}{Identfier of stratum for individuals at PSU. In addition strata are identified by the combination of any additional columns in this table.}
  \item{Domain}{Identifier of domains for individuals. In addition the domain is identified by the combination of any additional columns in this table}
  \item{nIndividuals}{Number of individuals observed for the domain}
 }
}
\concept{Analytical estimation}
\concept{Data types}
