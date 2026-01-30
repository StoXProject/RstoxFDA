# Assign trip IDs

Assigns trip IDs to landings based on vessel identifier and date of last
catch.

## Usage

``` r
appendTripIdLandings(
  landings,
  tripIds = NULL,
  vesselIdCol = "Radiokallesignal (seddel)",
  lastCatchCol = "Siste fangstdato",
  tripIdCol = "tripid"
)
```

## Arguments

- landings:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing landings records

- tripIds:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with columns 'vesselId', 'time' and 'tripId'

- vesselIdCol:

  character() that identifies a column in 'landings' that contain the
  vessel id (e.g. radio call signal) of the landing vessel. Default
  compatible with
  [`readLssFile`](https://rdrr.io/pkg/RstoxData/man/readLssFile.html).

- lastCatchCol:

  character() that identifies a POSIXct column in 'landings' that
  contain the time of last catch for each record. Default compatible
  with
  [`readLssFile`](https://rdrr.io/pkg/RstoxData/man/readLssFile.html).

- tripIdCol:

  character() that identifies the column name to append to 'landings'

## Value

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns 'vesselId' and 'time'

## Details

if 'tripIds' is NULL, trip IDs will be constructed from landings using
[`makeTripIds`](makeTripIds.md)

## Examples

``` r
 if (FALSE) { # \dontrun{
   #merge mesh size from first operation on a trip into landings
   lssfile <- "" #set appropriately
   logbfile <- "" #set appropriately
   landings <- RstoxData::readLssFile(lssfile)
   logbooks <- RstoxData::readErsFile(logbfile)
   tripIds <- makeTripIds(landings)
   logbooksWtripIds <- appendTripIdLogbooks(logbooks, tripIds)
   landingsWtripIds <- appendTripIdLandings(landings, tripIds)

   firstCatch <- logbooksWtripIds[!duplicated(logbooksWtripIds$tripid),]
   landingsWmeshSize <- merge(landingsWtripIds,
                              firstCatch[,c("tripid", "MASKEVIDDE")],
                              by="tripid",
                              all.x=T)
 } # }
```
