# Norwegian municipalities (kommune)

Definitions of Norwegian municipalities (kommune), current in 2022.
providing their borders, their official name and their official code.

## Usage

``` r
data(kommuner2022)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Details

Norwegian landings data often list landing sites by identifiers for
municipalities (kommunenummer). This resource can be used to infer
location code that are associated with polygons or coordinates, such as
UN/LOCODES.

The Norwegian municipalities have been subject to revision, and other
resources may have to be obtained to completely handle data from other
years.

## Examples

``` r
# map kommune to locodes
data(kommuner2022)
data(portcodes2020)
nocodes <- portcodes2020[portcodes2020$Country=="NO" & !is.na(portcodes2020$latitude),]
locodemap <- RstoxFDA::appendAreaCode(nocodes, kommuner2022, 
        colName="kommune", 
        latName = "latitude", 
        lonName = "longitude")[,c("kommune", "Location")]
# Note that kommune does not uniquely identify locode:
locodemap[locodemap$kommune==4601,]
#>    kommune Location
#>     <char>   <char>
#> 1:    4601      BGO
#> 2:    4601      HGR
#> 3:    4601      HRV
#> 4:    4601      LVG
#> 5:    4601      SKV
#> 6:    4601      UME
```
