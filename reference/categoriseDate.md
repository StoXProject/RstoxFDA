# Get temporal categories

define a categorical variable based for a vector of dates

## Usage

``` r
categoriseDate(date, temporalType = "quarter", seasonal = T, FUN = NULL)
```

## Arguments

- date:

  POSIXct() vector of dates

- temporalType:

  character() specify the kind of temporal category to define:
  "quarter", "week" or "custom"

- seasonal:

  logical() specify whether the temporal category should be seasonal
  (e.g. week 1 in year x is the same category as week 1 in year y)

- FUN:

  function(day, month) mapping a day (1-based integer()) and month
  (1-based integer()) to a value (character()) for the categorical
  variable to be defined.

## Value

character() a vector of values for the categorical variable,
corresponding to the dates in 'date'

## Examples

``` r
 #get current quarter
 quarter <- categoriseDate(Sys.time())

 #get custom non-seasonal category
 dates <- as.POSIXct(c(
    "2018-01-17 21:37:29 CET",
    "2019-02-28 21:37:29 CET",
    "2019-12-28 21:37:29 CET"))
 inDecember <- function(day, month){if(month==12){return("Yes")};return("No")}
 categoriseDate(dates, temporalType = "custom", FUN=inDecember, seasonal = FALSE)
#> [1] "No-2018"  "No-2019"  "Yes-2019"
```
