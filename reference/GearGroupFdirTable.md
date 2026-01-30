# Gear groups

Example of metier table for assigning gear groups in NS9400 / Norwegian
Directorate of fisheries

## Usage

``` r
data(GearGroupFdirTable)
```

## Format

[`MetierTable`](MetierTable.md) with column 'gearcode' identifying gear
codes used in Norwegian fisheries data (derived from NS 9400)

## Examples

``` r
data(GearGroupFdirTable)
data(activityCensus)
annotated <- appendMetier(activityCensus,
                          GearGroupFdirTable,
                          "gearNS",
                          metierColName = "Hovedgruppe Redskap")
table(annotated$gearFAO, annotated$"Hovedgruppe Redskap")
#>      
#>       D.SEINE GILLNET HOOKS  MIS OTHER SEINE TRAPS TRAWL
#>   DRB       0       0     0    0     2     0     0     0
#>   FPO       0       0     0    0     0     0    12     0
#>   GEN       0    1080     0    0     0     0     0     0
#>   GN        0     571     0    0     0     0     0     0
#>   GNC       0      39     0    0     0     0     0     0
#>   GNS       0     252     0    0     0     0     0     0
#>   HAR       0       0     0    0     4     0     0     0
#>   HMP       0       0     0    0     1     0     0     0
#>   LA        0       0     0    0     0     3     0     0
#>   LHM       0       0     4    0     0     0     0     0
#>   LHP       0       0     2    0     0     0     0     0
#>   LL        0       0   358    0     0     0     0     0
#>   LLD       0       0    10    0     0     0     0     0
#>   LLS       0       0   454    0     0     0     0     0
#>   LTL       0       0     2    0     0     0     0     0
#>   OT        0       0     0    0     0     0     0    12
#>   OTB       0       0     0    0     0     0     0  2069
#>   OTM       0       0     0    0     0     0     0   282
#>   OTT       0       0     0    0     0     0     0   144
#>   PS        0       0     0    0     0   228     0     0
#>   PS1       0       0     0    0     0   558     0     0
#>   PS2       0       0     0    0     0    69     0     0
#>   PT        0       0     0    0     0     0     0    48
#>   PTB       0       0     0    0     0     0     0   324
#>   PTM       0       0     0    0     0     0     0   239
#>   SDN       9       0     0   24     0     0     0     0
#>   SPR     105       0     0    0     0     0     0     0
#>   SSC    1342       0     0    0     0     0     0     0
#>   SV      567       0     0    0     0     0     0     0
#>   SX        0       0     0    0     0     3     0     0
#>   TB        0       0     0    0     0     0     0  1057
#>   TBN       0       0     0    0     0     0     0    50
#>   TBS       0       0     0    0     0     0     0   896
#>   TM        0       0     0    0     0     0     0   333
#>   TMS       0       0     0    0     0     0     0    26
#>   TX        0       0     0    0     0     0     0    58
```
