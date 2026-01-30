# Calcualte catch partition

Partitions total catch of each species for each trip on provided
grouping variables. NAs in weights are ignored (treated as zero).

## Usage

``` r
calculateLogbookPartitionByTrip(
  logbooks,
  groupCols,
  tripCol = "tripid",
  speciesCol = "FANGSTART_FAO",
  weightCol = "RUNDVEKT"
)
```

## Arguments

- logbooks:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with logbooks

- groupCols:

  character() vector of names identifying the grouping columns in
  'logbooks'

- tripCol:

  character() identyfing the column in 'logbooks' that identify a trip

- speciesCol:

  character() identyfing the column in 'logbooks' that specify species.

- weightCol:

  character() identifying the column in 'logbboks' that specify the live
  weight of the species.

## Value

[`TripPartition`](TripPartition.md)

## Details

Default parameters are compatible
[`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html) with
tripids annotated by [`appendTripIdLogbooks`](appendTripIdLogbooks.md).

For other partitionings of logbooks, consider
[`tabulateFisheries`](tabulateFisheries.md).

## Examples

``` r
 if (FALSE) { # \dontrun{
  #make calculate fractions in each area for each trip
  lssfile <- "" #set appropriately
  logbfile <- "" #set appropriately
  landings <- RstoxData::readLssFile(lssfile)
  logbooks <- RstoxData::readErsFile(logbfile)
  tripIds <- makeTripIds(landings)
  logbooksWtripIds <- appendTripIdLogbooks(logbooks, tripIds)

  logbooksWtripIds$mainArea <- substring(logbooksWtripIds$LOKASJON_START,1,2)
  fractions <- calculateLogbookPartitionByTrip(logbooksWtripIds, "mainArea")
 } # }
```
