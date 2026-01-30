# Fill in values from logbooks

Fills selected columns in imputed landings from corresponding logbook
records

## Usage

``` r
sourceLogbookColumns(
  landings,
  logbooks,
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

The following columns in 'landings' may be altered. All others are left
untouched:

- Hovedområde (kode):

  set to corresponding value in logbooks

- Hovedområde:

  set to NA for landings with corresponding entries in logbooks

- Lokasjon (kode):

  set to corresponding value in logbooks

- Siste fangstdato:

  set to corresponding value in logbooks

- Redskap:

  set to corresponding value in logbooks

'landings' should already imputed with information about individual
catches. The columns 'tripIdCol' and 'catchIdCol' must be set on
'landings' and 'logbooks'. This may be achieved by
[`appendTripIdLogbooks`](appendTripIdLogbooks.md),
[`appendTripIdLandings`](appendTripIdLandings.md) and
[`imputeCatchesLandings`](imputeCatchesLandings.md).

The function sets values for gear, area codes, and date of last catch
from logbooks. Landings that does not have corresponding logbook records
are left untouched

## See also

[`makeTripIds`](makeTripIds.md),
[`appendTripIdLandings`](appendTripIdLandings.md),
[`appendTripIdLogbooks`](appendTripIdLogbooks.md), and
[`imputeCatchesLandings`](imputeCatchesLandings.md) for obtaining
'landings' and 'logbooks' with trip-ids and catch-ids.
