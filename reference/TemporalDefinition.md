# Temporal Categories (TemporalDefinition)

Table
([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
defining a categorical variable for grouping data based on date.

## Details

- TemporalCategory:

  character() Value of the temporal category

- StartDay:

  integer() Day of month for first day in the temporal category
  (1-based)

- StartMonth:

  integer() Month for first day in the temporal category (1-based)

- StartYear:

  optional integer() Year for which the category is defined, omit this
  column for seasonal definitions.

Start and end of year is not implied as category delimitations when not
included. If 1st of January is not defined as the start of a category,
it is taken to be included in the last category of the preceding year.
