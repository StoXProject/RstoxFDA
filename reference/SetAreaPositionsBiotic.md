# Set position Biotic

Sets start position based on area codes. Appropriate when BioticData is
read from NMDbiotic.

## Usage

``` r
SetAreaPositionsBiotic(
  BioticData,
  AreaPosition,
  LocationVariable = c("None", "location"),
  System = character(),
  Overwrite = F
)
```

## Arguments

- BioticData:

  [`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) data
  for which positions should be set

- AreaPosition:

  coordinates for Area and Location codes, see
  [`AreaPosition`](AreaPosition.md)

- LocationVariable:

  Specify which column in 'BioticData' should are represented by
  'Location' in 'AreaPosition'. See details.

- System:

  identifies the area coding system used. Corresonds to the column
  'system' on 'fishstation' in 'BioticData'.

- Overwrite:

  if True any existing values in 'latitudestart' and 'longitudestart'
  will be overwritten. If False postions with both latitude and
  longitude will be kept as they were.

## Value

[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html)

## Details

Positions are filled in the columns 'latitudestart' and 'longitudestart'
on 'fishstation' whenever the value in the column 'system' on
'fishstation' is equal to the parameter 'System'.

When 'LocationVariable' is specified as 'None' area is looked up from
'AreaPosition', using the row where 'Location' is missing. When
'LocationVariable' is specified as 'location', 'area' and 'location' in
'BioticData' is looked up against 'Area' and 'Location' in
'AreaPosition'.

No modifications are done to rows that have missing values for 'system'
or 'area' (or 'location', depending on the parameter
'LocationVariable'). Any rows with areas coded in another system than
specified by the parameter 'System' will not be changed.

These columns are on the table 'fishstation' in data originating from
NMDBiotic (http://www.imr.no/formats/nmdbiotic/). For bioticdata that
does not conform to this, no modifications are done.

## See also

[`DefineAreaPosition`](DefineAreaPosition.md) for configuring
definitions of positions for area codes.
