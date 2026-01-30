# Adjust landings by logbooks

imputes landings and redistributes catch within quarter and area to
mimic sales notes for each individual fishing operation (logbook
record). Applicable to formats parsed by
[`readLssFile`](https://rdrr.io/pkg/RstoxData/man/readLssFile.html) and
[`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html)

## Usage

``` r
logbookAdjustment(
  landings,
  logbooks,
  gearCodes = character(),
  speciesColLog = "FANGSTART_FAO",
  speciesColLand = "Art FAO (kode)",
  weightColLog = "RUNDVEKT",
  valueColLand = c("Bruttovekt", "Produktvekt", "Rundvekt"),
  addColumns = character(),
  activityTypes = c("FIS", "REL", "SCR"),
  polygons = RstoxFDA::mainareaFdir2018,
  lineIdCol = "Linjenummer"
)
```

## Arguments

- landings:

  landings as returned by e.g.
  [`readLssFile`](https://rdrr.io/pkg/RstoxData/man/readLssFile.html)

- logbooks:

  logbooks as returned by e.g.
  [`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html)

- gearCodes:

  character() with NS-9400 gear-codes which logbook cleaning should be
  applied for. If not provided, cleaning is applied to all gear codes

- speciesColLog:

  character() that identifies the column in 'logbook' that contain
  information about species of catch.

- speciesColLand:

  character() that identifies the column in 'landings' that contain
  information about species of catch.

- weightColLog:

  character() that identifies the column in 'logbook' that contain the
  weights that redistribution of weights should be based on.

- valueColLand:

  character() vector that identifies the columns in 'landings' that
  contain values that are to be redistributed over imputed landings
  (weight and value columns)

- addColumns:

  character() vector that identifies columns in 'logbooks' that should
  be added to 'landings'

- activityTypes:

  character() vector with the activity types that should be utilized
  from logbook records ('AKTIVITET_KODE')

- polygons:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data.frame
  with area names in the column 'StratumName'. If provided, area
  ("Hovedområde kode") will be calculated from position, rather than
  fetched from logbook records.

- lineIdCol:

  character() that identifies the column in 'landings' that contain the
  identifier for each line on a sales note.

## Value

'landings' with catches redistributed over more sales-note lines
corresponding to logbook-catch records / fishing operations.

## Details

Imputes landings for logbook records, redistributes catches, and adds
information from logbooks. See details in utilized functions:
[`imputeCatchesLandings`](imputeCatchesLandings.md),
[`sourceLogbookColumns`](sourceLogbookColumns.md), and
[`addLogbookColumns`](addLogbookColumns.md). In particular consult
[`sourceLogbookColumns`](sourceLogbookColumns.md) to learn which columns
in the 'landings' are affected by imputation.

Logbook imputations are only applied for logbook records with the gears
in 'gearCodes' if given. If gearCodes are not given (the default) all
available logbook records are applied.

Landings for which logbook records are not available, or not applied (by
'gearCode' restriction) are retained untouched in the returned landings.

Sales-note lines identifiers are made unique after imputation and
redistribution ('lineIdCol').
