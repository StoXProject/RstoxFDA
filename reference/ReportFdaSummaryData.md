# Total catch statistics (ReportFdaSummaryData)

Results from catch estimations. The results may be presented decomposed
on combinations of aggregation variables, such as gear, area, stock etc.

list with six members 'MeanAge', 'MeanWeight', 'MeanLength',
'TotalWeight', 'TotalNumber', and 'GroupingVariables'. 'MeanAge',
'MeanWeight', 'MeanLength', 'TotalWeight', 'TotalNumber' are
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)s with
the columns:

- \<Statistic\>:

  The reported statistic, either 'MeanIndividualAge',
  'MeanIndividualWeight', 'MeanIndividualLength', 'TotalWeight', or
  'TotalNumber'

- SD:

  Standard deviation for the reported statistic.

- Low:

  The lower limit of the estimated interval for the reported statistic.

- High:

  The higher limit of the estimated interval for the reported statistic.

- ...:

  Any aggregation variables. The names of these are listed in
  'GroupingVariables'

Units are configurable, and can be inspected by
[`getUnit`](https://rdrr.io/pkg/RstoxData/man/getUnit.html)

'GroupingVariables' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
a column containing the names of any aggregation variables.
