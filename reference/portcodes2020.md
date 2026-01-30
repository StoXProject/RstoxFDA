# UN Location codes for ports

Table of United Nations Codes for Trade and Transport Locations
(UN/LOCODE) that has a seaport. Updated ca 2020.

## Usage

``` r
data(portcodes2020)
```

## Format

[`LocodeTable`](LocodeTable.md).

## Details

There is a considerable lag between the national adaptation of new
locodes and the updates to the UN/LOCODE lists. The Norwegian
institution that coordinates new needs for LOCODES is the Norwegian
Coastal Administration (Kystverket).

## Examples

``` r
data(portcodes2020)
nocodes <- portcodes2020[portcodes2020$Country=="NO" & !is.na(portcodes2020$latitude),]
RstoxFDA::plotArea(nocodes, "latitude", "longitude", areaDef=RstoxFDA::mainareaFdir2017)
```
