# Annotate metier

annotates metier to data table

## Usage

``` r
appendMetier(
  data,
  metiertable,
  gearColumn,
  targetColumn = NULL,
  meshSizeColumn = NULL,
  selectivityDeviceColumn = NULL,
  selectivityDeviceMeshSizeColumn = NULL,
  metierColName = "metier"
)
```

## Arguments

- data:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with data to be annotated

- metiertable:

  [`MetierTable`](MetierTable.md) with metier definitions, or object
  that can be read with [`readMetierTable`](readMetierTable.md)

- gearColumn:

  character() identifies the column in 'data' that encodes gear. Gear
  definition must match metiertable\$gearcode

- targetColumn:

  character(), optional, identifies the column in 'data' that encodes
  target species. Definition must match metiertable\$target

- meshSizeColumn:

  integer(), optional, identifies the column in 'data' that encodes the
  mesh size of the gear.

- selectivityDeviceColumn:

  character(), optional, identifies the column in 'data' that encodes
  selectivitydevices mounted on gear. Definition must mathc
  metiertable\$selectivityDevice

- selectivityDeviceMeshSizeColumn:

  integer(), optional, identifies the column in 'data' that encodes the
  mesh size of any mounted selectivity device.

- metierColName:

  character() name of the column that should be appended to 'data'

## Value

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
'data' with the column 'metierColName' appended (character).

## Details

Annotates data with metier, given a metier definition
([`MetierTable`](MetierTable.md)) and a selection of data-columns that
idenitfies metier-defining variables. Metier names are defined by the
column 'metier' in 'metiertable', and does not have to reflect the
information actually used for annotation. For example, metiers of the
type "OTB_DEF", indicating both gear and target assemblage, can be
configured based only on gear codes.

For the metier-defining variables 'gearcode', 'target' and
'selectivityDevice' (see [`MetierTable`](MetierTable.md)), missing
values are matches with missing values. That is, a metier that is
defined with a missing value for 'target' will be annotated to otherwise
matching data which has a missing value in the 'targetColumn'.

## Examples

``` r
 data(metier4table)
 data(activityCensus)

 # annotate metier lvl 4 on the cencus based on gear, and compare with finer gear declaration.
 annotated <- appendMetier(activityCensus,
          metier4table,
          "gearNS",
          metierColName = "metier4")
 table(annotated$metier4)
#> 
#>  FPO   GN  GNS  HAR  LHM   LL  LLD  LTL  MIS  OTB  OTM  OTT   PS  PTB  PTM  SSC 
#>   12 1690  252    4    6  812   10    2   27 2069  282  144  855  324  239 2023 
#>   SX  TBN  TBS   TX 
#>    6   50  922 1508 

 data(metier5table)
 # annotate metier lvl 5 on COD-catches based on only gear,
 # and compare with declarations for shrimp fisheries
 annotated <- appendMetier(activityCensus[activityCensus$species=="COD"],
          metier5table,
          "gearNS",
          metierColName = "metier5")
 annotatedShrimp <- annotated[annotated$targetFAO %in% c("PAN", "PRA"),]
 table(paste(annotatedShrimp$gearFAO, annotatedShrimp$targetFAO, sep="/"),
      annotatedShrimp$metier5)
#>          
#>           MIS_MIS OTB_CRU OTB_DEF PTB_DEF
#>   OTB/PRA       0       0      21       0
#>   PTB/PRA       0       0       0       1
#>   TB/PRA        1       0       0       0
#>   TBN/PAN       0       1       0       0
#>   TBS/PAN       0       2       0       0
#>   TBS/PRA       0      22       0       0
#>   TM/PRA        1       0       0       0
#>   TMS/PRA       0       1       0       0

 data(metier6table)
 # annotate metier lvl 6 on COD-catches based on gear and mesh size,
 # and compare with lvl 5 annotations from last example
 annotated <- appendMetier(activityCensus[activityCensus$species=="COD"],
          metier6table,
          "gearNS",
          meshSizeColumn = "meshSize",
          metierColName = "metier6")
 annotated <- appendMetier(annotated,
           metier5table,
           "gearNS",
           metierColName = "metier5")
 annotatedShrimp <- annotated[annotated$targetFAO %in% c("PAN", "PRA"),]
 table(annotatedShrimp$metier5, annotatedShrimp$metier6)
#>          
#>           OTB_CRU_16-31_0_0_all OTB_CRU_32-69_0_0_all OTB_DEF_>=120_0_0_all
#>   MIS_MIS                     0                     2                     0
#>   OTB_CRU                     2                    22                     1
#>   OTB_DEF                     0                    16                     5
#>   PTB_DEF                     0                     1                     0
#>          
#>           OTB_SPF_32-69_0_0_all
#>   MIS_MIS                     0
#>   OTB_CRU                     1
#>   OTB_DEF                     0
#>   PTB_DEF                     0
 
```
