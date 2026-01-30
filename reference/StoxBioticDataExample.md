# Data from Norwegian port sampling program.

Example of data formatted as
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html),
with some additional columns added with
[`AddGearGroupStoxBiotic`](AddGearGroupStoxBiotic.md),
[`AddStratumStoxBiotic`](AddStratumStoxBiotic.md),
[`AddPeriodStoxBiotic`](AddPeriodStoxBiotic.md), and
[`AddToStoxBiotic`](https://rdrr.io/pkg/RstoxData/man/AddToStoxBiotic.html).
The data contain saithe samples from norwegian port-sampling in 2021.

## Usage

``` r
data(StoxBioticDataExample)
```

## Format

[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)

## Examples

``` r
 RstoxFDA::plotArea(RstoxFDA::StoxBioticDataExample$Station, 
      areaDef=RstoxFDA::mainareaFdir2018, 
      latCol = "Latitude", 
      lonCol = "Longitude", 
      areaLabels = TRUE)
```
