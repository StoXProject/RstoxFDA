# Analytical PSU Estimate Data

Analytical estimates for each PSU

List containing the following
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)s:

Abundance

- SampleId:

  Identfier for Primary sampling unit (PSU)

- Stratum:

  Identifier for stratification of individuals

- Domain:

  Identifier of domains for individuals

- Abundance:

  Total number in Stratum and Domain at th PSU. Unsampled strata is
  reported as NA

- Frequency:

  Frequency in Domain within Stratum. Unsampled strata is reported as NA

Variables

- SampleId:

  Identfier for Primary sampling unit (PSU)

- Stratum:

  Identifier for stratification of individuals

- Domain:

  Identifier of domains for individuals

- Variable:

  Variable that total and mean is reported for

- Total:

  Total value of variable in Stratum and Domain at the PSU. Unsampled
  strata is reported as NA

- Mean:

  Mean value of variable in Stratum and Domain at the PSU. Unsampled
  strata is reported as NA

DomainVariables

- Domain:

  Identifier of domains for individuals. In addition the domain is
  identified by the combination of any additional columns in this table

- \<DomainVariables\>:

  Columns that relate the domains to data records.

PSUDomainVariables

- SampleId:

  Identifier for Primary Sampling Unit (PSU)

- PSUDomain:

  Identfier of domains for PSUs. In addition PSU-domains are identified
  by the combination of any additional columns in this table.

- \<DomainVariables\>:

  Columns that relate the PSU domains to data records.

StratificationVariables

- SampleId:

  Identifier for Primary Sampling Unit (PSU)

- Stratum:

  Identfier of stratum for individuals at PSU. In addition strata are
  identified by the combination of any additional columns in this table.

- \<StratificationVariables\>:

  Columns that relate the PSU domains to data records.

The combination of the columns 'Stratum' and 'SampleId' uniquely
identifies a row in the table 'StratificationVariables' The columns
\<StratificationVariables\> are optional, but if present; their
combination must identify a stratum for each 'SamplingId'.

SampleCount

- SampleId:

  Identifier for Primary Sampling Unit (PSU)

- Stratum:

  Identfier of stratum for individuals at PSU. In addition strata are
  identified by the combination of any additional columns in this table.

- Domain:

  Identifier of domains for individuals. In addition the domain is
  identified by the combination of any additional columns in this table

- nIndividuals:

  Number of individuals observed for the domain
