# Landings for the Norwegian North Sea Herring fisheries in 2022.

Example of data formatted as
[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)
The data set contains landings of herring caugth in 2022 and sold as
North Sea Herring, from Norwegian vessels larger than 15 m.

## Usage

``` r
data(CatchLotteryLandingExample)
```

## Format

[`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)

## Details

Samples from the Catch Lottery covering this fishery are provided in
[`CatchLotteryExample`](CatchLotteryExample.md)

## Examples

``` r
 #report destination (country landed) of landed catch in tonnes
 RstoxFDA::CatchLotteryLandingExample$Landing[,list(weightT=sum(RoundWeight)/1000), 
                                                  by=c("CountryLanding")]
#>    CountryLanding    weightT
#>            <char>      <num>
#> 1:            DNK  12777.336
#> 2:            NOR 120306.224
#> 3:            FRO      8.904
#> 4:            GBR     45.034
```
