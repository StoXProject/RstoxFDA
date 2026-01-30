# Reca Weight At Age Report (ReportFdaWeightAtAgeData)

A list with two members: 'MeanWeightByAge', and 'GroupingVariables'.

- MeanWeightByAge:

  A [`ReportFdaData`](ReportFdaData.md) table with reported
  \<Statistic\> being 'MeanIndividualWeight': the mean weight

- GroupingVariables:

  Any specified Grouping variables.

Units are configurable, and can be inspected by
[`getUnit`](https://rdrr.io/pkg/RstoxData/man/getUnit.html)

'GroupingVariables' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
a column containing the names of any aggregation variables.

Note that the summary statistics are reported for summaries of mean
weights, so that e.g. SD report the standard deviation of the means, and
does not characterize the weight distribution of fish.
