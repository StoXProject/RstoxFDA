# MetierTable lvl 6 example

Example of metier table for assigning fishing activity to Metier level
6, based on gear codes and mesh-sizes. This is not a universal
conversion table, but an example of a table made for a particular
purpose. Some gear assignments are done with assumptions, and
considering the exact metier codes acceptable by data recipients.

## Usage

``` r
data(metier6table)
```

## Format

[`MetierTable`](MetierTable.md) with column 'gearcode' identifying gear
codes used in Norwegian fisheries data (derived from NS 9400)

## Details

For example it was agreed with data recipient that all bottom trawls
should be reported as OTB, so paired trawl are explicitly grouped with
otter trawls.

There are other examples of pragmatic code-mapping as well. This
particular one was explained in order to caution against indiscriminate
use. The code example below annotates an activity census of COD-catches
with FAO-gearcode declarations and target sepcies declarations, and
compares the annotated metiers with the codes used for reporting catch
where shrimp was declared as target.

## Examples

``` r
data(metier6table)
data(activityCensus)
annotated <- appendMetier(activityCensus[activityCensus$species=="COD"],
        metier6table,
        "gearNS",
        meshSizeColumn = "meshSize",
        metierColName = "metier6")
annotatedShrimp <- annotated[annotated$targetFAO %in% c("PAN", "PRA"),]
table(paste(annotatedShrimp$gearFAO, annotatedShrimp$targetFAO, sep="/"),
     annotatedShrimp$metier6)
#>          
#>           OTB_CRU_16-31_0_0_all OTB_CRU_32-69_0_0_all OTB_DEF_>=120_0_0_all
#>   OTB/PRA                     0                    16                     5
#>   PTB/PRA                     0                     1                     0
#>   TB/PRA                      0                     1                     0
#>   TBN/PAN                     0                     0                     0
#>   TBS/PAN                     0                     2                     0
#>   TBS/PRA                     2                    19                     1
#>   TM/PRA                      0                     1                     0
#>   TMS/PRA                     0                     1                     0
#>          
#>           OTB_SPF_32-69_0_0_all
#>   OTB/PRA                     0
#>   PTB/PRA                     0
#>   TB/PRA                      0
#>   TBN/PAN                     1
#>   TBS/PAN                     0
#>   TBS/PRA                     0
#>   TM/PRA                      0
#>   TMS/PRA                     0
```
