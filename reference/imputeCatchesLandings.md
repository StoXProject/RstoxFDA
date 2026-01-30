# Impute catches to landings

Imputes landings to represent individual catches of species from a trip,
and redistributes selected values over the imputed landings. This mimics
a situation where landings are reported for each catch rather than each
trip. This is useful to obtain a higher resolution on some grouping
variables and consolidate logbook records with landing records, while
preserving data for landings not reported in logbooks, and preserving
the total value equal to that in landings.

## Usage

``` r
imputeCatchesLandings(
  landings,
  logbooks,
  tripIdCol = "tripid",
  catchIdCol,
  speciesColLand = "Art FAO (kode)",
  speciesColLog = "FANGSTART_FAO",
  weightCol = "RUNDVEKT",
  valueColumns = c("Bruttovekt", "Produktvekt", "Rundvekt")
)
```

## Arguments

- landings:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing landings records, and a column identifying the trip that
  caught the landed catch ('tripIdCol').

- logbooks:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing logbook records, and columns identifying the trip
  ('tripIdCol') and catch ('catchIdCol').

- tripIdCol:

  character() that identifies the column in 'landings' and 'logbooks'
  that contain trip ids

- catchIdCol:

  character() that identifies the column in 'logbooks' that contain
  catch ids

- speciesColLand:

  character() that identifies the column in 'landings' that contain
  species ids

- speciesColLog:

  character() that identifies the column in 'logbooks' that contain
  species ids

- weightCol:

  character() that identifies the column in 'logbooks' that contain the
  weights to use for redistribution of value columns.

- valueColumns:

  character() vector with names of columns that should be redistributed
  over the imputed landings

## Value

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
'landings' that have artificial landings imputed, each catch identified
by the added column 'catchIdCol'.

## Details

The result of imputation have added lines for each corresponding catch
in logbooks, and 'valueColumns' redistributed to reflect the fraction of
'weightCol' that each catch contributes to the total trip catch of the
species ('speciesColLog').

Important: columns in landings that are not in 'valueColumns' will be
duplicated exactly for the imputed catches. That means that any
weight-column not in 'valueColumns' will result in duplicated total
weights for that column. That also means that any identifiers / keys may
be copied and thus be rendered non-unique in the imputed landings.

Landings for which no trip can be found in 'logbooks' are left
untouched. logbook entries with no corresponding landings are ignored.

Default values for parameters 'speciesCol' and 'valueColumns' are
suitable for data read with
[`readLssFile`](https://rdrr.io/pkg/RstoxData/man/readLssFile.html).
Default values for 'tripIdCol' corresponds to defaults for
[`makeTripIds`](makeTripIds.md),
[`appendTripIdLandings`](appendTripIdLandings.md), and
[`appendTripIdLogbooks`](appendTripIdLogbooks.md).

## See also

[`makeTripIds`](makeTripIds.md),
[`appendTripIdLandings`](appendTripIdLandings.md), and
[`appendTripIdLogbooks`](appendTripIdLogbooks.md) for obtaining
'landings' with trip-ids.
