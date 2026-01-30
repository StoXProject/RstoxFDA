# Activity census

Example of activity census.

## Usage

``` r
data(activityCensus)
```

## Format

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns

- gearFAO:

  gear (FAO ISSCFG 1980).

- gearNS:

  gear (code based on NS 9400, Norwegian standard).

- targetFAO:

  target species (FAO ASFIS).

- meshSize:

  mesh size (mm, bar length).

- vesselLengthCategory:

  length group for vessels ranges in m.

- species:

  reported species (FAO ASFIS).

- wholeWeightKg:

  total catch within activity group in kg.

## Examples

``` r
data(activityCensus)
sum(activityCensus$OfficialLandingsWeight)
#> [1] 0
```
