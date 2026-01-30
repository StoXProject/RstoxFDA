# Convert logbook data

Convert [`LstLogbookData`](LstLogbookData.md) to the format provided by
[`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html). Not
all records can be filled, and some are left blank.

[`LstLogbookData`](LstLogbookData.md) contains records for fishing date,
but not exact time, which is required for conversion of time
information. In order to set start time of fishing operations, the a
time of day need to be provided via the parameter 'timestring' UTC times
are assumed.

## Usage

``` r
convertToErsData(LstLogbookData, timestring = "12:00:00")
```

## Arguments

- LstLogbookData:

  [`LstLogbookData`](LstLogbookData.md) to be converted

- timestring:

  string representing the time of day (UTC) to assume for fishing
  operations. Format: %H:%M:%S.

## Value

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
formatted as return from
[`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html).
