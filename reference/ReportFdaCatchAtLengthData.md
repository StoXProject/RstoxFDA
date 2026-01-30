# Fisheries dependent Catch At Age Report (ReportFdaCatchAtLengthData)

A list with two members: 'NbyLength', and 'GroupingVariables'.

- NbyLength:

  A [`ReportFdaData`](ReportFdaData.md) table with reported
  \<Statistic\> being 'CacthAtLength': the total catch at length in
  numbers.

- GroupingVariables:

  Any specified Grouping variables.

Units are configurable, and can be inspected by
[`getUnit`](https://rdrr.io/pkg/RstoxData/man/getUnit.html)

'GroupingVariables' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
a column containing the names of any aggregation variables.
