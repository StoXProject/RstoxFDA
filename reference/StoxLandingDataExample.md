# Saithe landings 2021.

Example of data formatted as
[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html),
with some additional columns added with
[`AddGearGroupStoxLanding`](AddGearGroupStoxLanding.md),
[`AddStratumStoxLanding`](AddStratumStoxLanding.md), and
[`AddAreaPositionStoxLanding`](AddAreaPositionStoxLanding.md), and
[`AddStratumStoxLanding`](AddStratumStoxLanding.md), and
[`AddPeriodStoxLanding`](AddPeriodStoxLanding.md). The data contain
saithe landings by Norwegian vessels in areas
([`mainareaFdir2018`](mainareaFdir2018.md)) "00", "03", "04", "05","06",
in 2021.

## Usage

``` r
data(StoxLandingDataExample)
```

## Format

[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)

## Examples

``` r
 RstoxFDA::plotBubbleMap(RstoxFDA::StoxLandingDataExample$Landing, 
      "Area", "RoundWeight", RstoxFDA::mainareaFdir2018)
#> Spherical geometry (s2) switched off
#> Spherical geometry (s2) switched on
```
