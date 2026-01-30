# Add columns from logbooks

Add columns to landings from corresponding logbook entries.

## Usage

``` r
addLogbookColumns(
  landings,
  logbooks,
  logbookColumns,
  tripIdCol = "tripid",
  catchIdCol,
  speciesColLand = "Art FAO (kode)",
  speciesColLog = "FANGSTART_FAO"
)
```

## Arguments

- landings:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing landings records, and columns identifying the trip and
  catch ('tripIdCol', 'catchIdCol').

- logbooks:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing logbook records, and columns identifying the trip and catch
  ('tripIdCol', 'catchIdCol').

- logbookColumns:

  character() vector that identifies columns in logbooks that are to be
  added to 'landings'

- tripIdCol:

  character() that identifies the column in 'landings' and 'logbooks'
  that contain trip ids

- catchIdCol:

  character() that identifies the column in 'landings' and 'logbooks'
  that contain catch ids

- speciesColLand:

  character() that identifies the column in 'landings' that identify
  species landed

- speciesColLog:

  character() that identifies the column in 'logbooks' that identify
  species caught.

## Details

'landings' should already imputed with information about individual
catches. The columns 'tripIdCol' and 'catchIdCol' must be set on
'landings' and 'logbooks'. This may be achieved by
[`appendTripIdLogbooks`](appendTripIdLogbooks.md),
[`appendTripIdLandings`](appendTripIdLandings.md) and
[`imputeCatchesLandings`](imputeCatchesLandings.md).

'landings' may contain records with no correspond logbook entires
('tripIdCol' contains NAs). The added columns will have NA for these
records.

## See also

[`makeTripIds`](makeTripIds.md),
[`appendTripIdLandings`](appendTripIdLandings.md),
[`appendTripIdLogbooks`](appendTripIdLogbooks.md), and
[`imputeCatchesLandings`](imputeCatchesLandings.md) for obtaining
'landings' and 'logbooks' with trip-ids and catch-ids.
