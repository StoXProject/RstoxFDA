# Data from the Norwegian catch lottery sampling program.

Example of data formatted as
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html),
with an added column 'serialnumber' at the Haul-table and an added
column 'CountryVessel' at the Station-table which identifies the
selection in the catch sampling lottery that this data was recorded for
(see 'SamplingUnitId' in
[`CatchLotterySamplingExample`](CatchLotterySamplingExample.md))

## Usage

``` r
data(CatchLotteryExample)
```

## Format

[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)

## Details

Hauls are primary sampling units, selected by Poission sampling with
selection probabilities proportional to the catch size. Corresponding
sampling parameters are provided in
[`CatchLotterySamplingExample`](CatchLotterySamplingExample.md) The data
contain North Sea herring samples from catch lottery sampling in 2022.

## Examples

``` r
 RstoxFDA::plotArea(RstoxFDA::CatchLotteryExample$Station, 
      areaDef=RstoxFDA::mainareaFdir2018, 
      latCol = "Latitude", 
      lonCol = "Longitude", 
      areaLabels = TRUE)
```
