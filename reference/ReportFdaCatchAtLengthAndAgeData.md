# Fisheries dependent Catch At Age Report (ReportFdaCatchAtLengthAndAgeData)

A list with two members: 'NbyLengthAge', and 'GroupingVariables'.

- NbyLengthAge:

  A [`ReportFdaData`](ReportFdaData.md) table with reported
  \<Statistic\> being 'CatchAtAgeLength': the total catch at length and
  age in numbers.

- GroupingVariables:

  Any specified Grouping variables.

Units are configurable, and can be inspected by
[`getUnit`](https://rdrr.io/pkg/RstoxData/man/getUnit.html)

'GroupingVariables' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
a column containing the names of any aggregation variables.
