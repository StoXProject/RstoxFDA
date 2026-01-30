# Append position to landings data

StoX function Appends a position to landings data, based on Area and
Location codes.

## Usage

``` r
AddAreaPositionStoxLanding(
  StoxLandingData,
  AreaPosition,
  LocationVariable = c("None", "Location", "Coastal")
)
```

## Arguments

- StoxLandingData:

  landing data, see
  [`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)

- AreaPosition:

  coordinates for Area and Location codes, see
  [`AreaPosition`](AreaPosition.md)

- LocationVariable:

  Specify which column in 'StoxLandingsData' should are represented by
  'Location' in 'AreaPosition'. See details.

## Value

[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
with columns for latitude and longitude appended.

## Details

Positions are appended in the new columns 'Latitude' and 'Longitude'
When 'LocationVariable' is specified as 'None' Area is looked up from
'AreaPosition', using the row where 'Location' is missing. When
'LocationVariable' is specified as 'Location', 'Area' and 'Location' in
'StoxLandingData' is looked up against 'Area' and 'Location' in
'AreaPosition'. When 'LocationVariable' is specified as 'Coastal',
'Area' and 'Costal' in 'StoxLandingData' is looked up against 'Area' and
'Location' in 'AreaPosition'.
