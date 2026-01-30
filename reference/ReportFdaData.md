# Age group statistics (ReportFdaData)

Results from catch at age estimations. The results may be presented
decomposed on combinations of aggregation variables, such as gear, area,
stock etc.

ReportFdaData is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) which
may have the following columns:

- AgeGroup:

  character. The age group the estimate is reported for. May be age or
  plus group

- Age:

  integer. The lower age the estimate is reported for. May be an age or
  lower limit of plus group (inclusive)

- LengthGroup:

  character. The length group the estimate is reported for.

- Length:

  numeric. The upper length of the length group.

- \<Statistic\>:

  A reported statistic

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
