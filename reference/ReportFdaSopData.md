# Sum of Products report (ReportFdaSopData)

Sum of Products report (SOP-report), comparing the total landed weight
of fish with the product of mean weight at age estimates and total
number at age estimates

list with two members 'SopReport' and 'GroupingVariables'. 'SopReport'
is a [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
with the columns:

- TotalWeightEstimated:

  Total round weight estimated in kg

- LandedWeight:

  Landed round weight reported in kg

- Difference:

  The difference between estimated and reported landed weight in kg

- RelativeDifference:

  The difference between estimated and reported landed weight relative
  to reported weight

- ...:

  Any aggregation variables. The names of these are listed in
  'GroupingVariables'

'GroupingVariables' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
a column containing the names of any aggregation variables.

The unit for RelativeDifference is configurable, and can be inspected by
[`getUnit`](https://rdrr.io/pkg/RstoxData/man/getUnit.html)
