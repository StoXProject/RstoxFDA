# Make trip IDs

Extracts trip IDs from landings.

## Usage

``` r
makeTripIds(
  landings,
  vesselIdCol = "Radiokallesignal (seddel)",
  lastCatchCol = "Siste fangstdato"
)
```

## Arguments

- landings:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing landings records

- vesselIdCol:

  character() that identifies a column in 'landings' that contain the
  vessel id (e.g. radio call signal) of the landing vessel. Default
  compatible with
  [`readLssFile`](https://rdrr.io/pkg/RstoxData/man/readLssFile.html).

- lastCatchCol:

  character() that identifies a Date column in 'landings' that contain
  the date of last catch for each record. Default compatible with
  [`readLssFile`](https://rdrr.io/pkg/RstoxData/man/readLssFile.html).

## Value

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns 'vesselId', 'time' (POSIXct with time zone as in 'landings') and
'tripId'

## Examples

``` r
 if (FALSE) { # \dontrun{
   #make trip ids for landings
   lssfile <- "" #set appropriately
   landings <- RstoxData::readLssFile(lssfile)
   tripIds <- makeTripIds(landings)
 } # }
```
