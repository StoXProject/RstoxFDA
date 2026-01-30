# Assing trip ID to logbooks

Assings a trip ID to logbooks based on vessel identifier and a known
last time of operation on a trip.

## Usage

``` r
appendTripIdLogbooks(
  logbooks,
  tripIds,
  timeCol = "STARTTIDSPUNKT",
  vesselIdCol = "RC",
  tripIdCol = "tripid",
  verbose = T
)
```

## Arguments

- logbooks:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing logbook records

- tripIds:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with columns 'vesselId', 'time' and 'tripId'

- timeCol:

  character() that identifies a POSIXct column in 'logbooks' that
  contain the time of operation for each logbook record. Default
  compatible with
  [`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html).

- vesselIdCol:

  character() that identifies a column in 'logbooks' that contain the
  vessel id (e.g. radio call signal) of the reporting vessel. Default
  compatible with
  [`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html).

- tripIdCol:

  character() that identifies the column name to append to 'logbooks'

- verbose:

  logical() wheter to message progress

## Value

'logbooks' with the column identified by 'tripIDcol' added

## Details

'tripid' need not be complete in the sense that vessels not found in
'tripid' will be assigned a trip ID of NA, and any fishing operations
after the last trip for a vessel will be assinged a trip ID of NA. This
function will however assign all the fishing operations of a vessel
preceeding the first trip time to the first trip.

Runnning time is proportional to the number of trips in 'logbooks', so
it is advantagous to annotate after other relevant filtering.

## Examples

``` r
 if (FALSE) { # \dontrun{
   #make trip ids from landings and assign them to logbooks
   lssfile <- "" #set appropriately
   logbfile <- "" #set appropriately
   landings <- RstoxData::readLssFile(lssfile)
   logbooks <- RstoxData::readErsFile(logbfile)
   tripIds <- makeTripIds(landings)
   logbooksWtripIds <- appendTripIdLogbooks(logbooks, tripIds)
 } # }
```
