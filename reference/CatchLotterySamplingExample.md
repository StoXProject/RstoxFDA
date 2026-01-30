# Sampling parameters from the Norwegian catch lottery sampling program.

Example of data formatted as
[`PSUSamplingParametersData`](PSUSamplingParametersData.md) Hauls are
primary sampling units, selected by Poission sampling with selection
probabilities proportional to the catch size. The data contain sampling
parameters for North Sea herring samples from catch lottery sampling in
2022.

## Usage

``` r
data(CatchLotterySamplingExample)
```

## Format

[`PSUSamplingParametersData`](PSUSamplingParametersData.md)

## Details

The corresponding samples are provided in
[`CatchLotteryExample`](CatchLotteryExample.md)

## Examples

``` r
 #all selected PSU that where actuall sampled are provided in CatchLotteryExample
 sum(!is.na(CatchLotterySamplingExample$SelectionTable$SamplingUnitId))
#> [1] 48
 sum(CatchLotterySamplingExample$SelectionTable$SamplingUnitId %in%
       CatchLotteryExample$Haul$HaulKey)
#> [1] 48
```
