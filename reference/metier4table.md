# MetierTable lvl 4 example

Example of metier table for assigning fishing activity to Metier level
4. This is not a universal conversion table, but an example of a table
made for a particular purpose. Some gear assignments are done with
assumptions, and considering the exact metier codes acceptable by data
recipients.

## Usage

``` r
data(metier4table)
```

## Format

[`MetierTable`](MetierTable.md) with column 'gearcode' identifying gear
codes used in Norwegian fisheries data (derived from NS 9400)

## Examples

``` r
data(metier4table)
data(activityCensus)
annotated <- appendMetier(activityCensus, metier4table, "gearNS", metierColName = "metier4")
table(annotated$gearFAO, annotated$metier4)
#>      
#>        FPO   GN  GNS  HAR  LHM   LL  LLD  LTL  MIS  OTB  OTM  OTT   PS  PTB
#>   DRB    0    0    0    0    0    0    0    0    2    0    0    0    0    0
#>   FPO   12    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   GEN    0 1080    0    0    0    0    0    0    0    0    0    0    0    0
#>   GN     0  571    0    0    0    0    0    0    0    0    0    0    0    0
#>   GNC    0   39    0    0    0    0    0    0    0    0    0    0    0    0
#>   GNS    0    0  252    0    0    0    0    0    0    0    0    0    0    0
#>   HAR    0    0    0    4    0    0    0    0    0    0    0    0    0    0
#>   HMP    0    0    0    0    0    0    0    0    1    0    0    0    0    0
#>   LA     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   LHM    0    0    0    0    4    0    0    0    0    0    0    0    0    0
#>   LHP    0    0    0    0    2    0    0    0    0    0    0    0    0    0
#>   LL     0    0    0    0    0  358    0    0    0    0    0    0    0    0
#>   LLD    0    0    0    0    0    0   10    0    0    0    0    0    0    0
#>   LLS    0    0    0    0    0  454    0    0    0    0    0    0    0    0
#>   LTL    0    0    0    0    0    0    0    2    0    0    0    0    0    0
#>   OT     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   OTB    0    0    0    0    0    0    0    0    0 2069    0    0    0    0
#>   OTM    0    0    0    0    0    0    0    0    0    0  282    0    0    0
#>   OTT    0    0    0    0    0    0    0    0    0    0    0  144    0    0
#>   PS     0    0    0    0    0    0    0    0    0    0    0    0  228    0
#>   PS1    0    0    0    0    0    0    0    0    0    0    0    0  558    0
#>   PS2    0    0    0    0    0    0    0    0    0    0    0    0   69    0
#>   PT     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   PTB    0    0    0    0    0    0    0    0    0    0    0    0    0  324
#>   PTM    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   SDN    0    0    0    0    0    0    0    0   24    0    0    0    0    0
#>   SPR    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   SSC    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   SV     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   SX     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   TB     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   TBN    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   TBS    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   TM     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   TMS    0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>   TX     0    0    0    0    0    0    0    0    0    0    0    0    0    0
#>      
#>        PTM  SSC   SX  TBN  TBS   TX
#>   DRB    0    0    0    0    0    0
#>   FPO    0    0    0    0    0    0
#>   GEN    0    0    0    0    0    0
#>   GN     0    0    0    0    0    0
#>   GNC    0    0    0    0    0    0
#>   GNS    0    0    0    0    0    0
#>   HAR    0    0    0    0    0    0
#>   HMP    0    0    0    0    0    0
#>   LA     0    0    3    0    0    0
#>   LHM    0    0    0    0    0    0
#>   LHP    0    0    0    0    0    0
#>   LL     0    0    0    0    0    0
#>   LLD    0    0    0    0    0    0
#>   LLS    0    0    0    0    0    0
#>   LTL    0    0    0    0    0    0
#>   OT     0    0    0    0    0   12
#>   OTB    0    0    0    0    0    0
#>   OTM    0    0    0    0    0    0
#>   OTT    0    0    0    0    0    0
#>   PS     0    0    0    0    0    0
#>   PS1    0    0    0    0    0    0
#>   PS2    0    0    0    0    0    0
#>   PT     0    0    0    0    0   48
#>   PTB    0    0    0    0    0    0
#>   PTM  239    0    0    0    0    0
#>   SDN    0    9    0    0    0    0
#>   SPR    0  105    0    0    0    0
#>   SSC    0 1342    0    0    0    0
#>   SV     0  567    0    0    0    0
#>   SX     0    0    3    0    0    0
#>   TB     0    0    0    0    0 1057
#>   TBN    0    0    0   50    0    0
#>   TBS    0    0    0    0  896    0
#>   TM     0    0    0    0    0  333
#>   TMS    0    0    0    0   26    0
#>   TX     0    0    0    0    0   58
```
