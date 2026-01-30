# Fisheries dependent Catch At Age Covariance Report (ReportFdaCatchAtAgeCovarianceData)

Covariances from catch at age estimations. Covariances are presented for
age groups, together with any additional grouping (aggregation
variables), such as gear, area, stock etc.

list with two members 'CovarianceNbyAge' and 'Variables'.
'CovarianceNbyAge' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) which
may have the following columns:

- VariableId1:

  Identifies the one of the variables the covariance is calculated for

- VariableId2:

  Identifies the other one of the variables the covariance is calculated
  for

- Covariance:

  The covariance of catch at age in numbers between groups identified by
  'Variable1' and 'Variable2'.

&nbsp;

- VariableId:

  Identifier for variable that covariances are provided for.

- AgeGroup:

  character. The age group for the variable. May be age or plus group

- Age:

  integer. The lower age group for the variable. May be an age or lower
  limit of plus group (inclusive)

- ...:

  Any aggregation variables.

Units are configurable, and can be inspected by
[`getUnit`](https://rdrr.io/pkg/RstoxData/man/getUnit.html)
