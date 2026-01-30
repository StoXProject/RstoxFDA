# Fisheries dependent Catch At Age Report (ReportFdaCatchAtAgeData)

A list with two members: 'NbyAge' and 'GroupingVariables'.

- NbyAge:

  A [`ReportFdaData`](ReportFdaData.md) table with reported
  \<Statistic\> being 'CatchAtAge': the total catch at age in numbers.

- GroupingVariables:

  Any specified Grouping variables.

Units are configurable, and can be inspected by
[`getUnit`](https://rdrr.io/pkg/RstoxData/man/getUnit.html)

'GroupingVariables' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
a column containing the names of any aggregation variables.
