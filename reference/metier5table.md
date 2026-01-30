# MetierTable lvl 5 example

Example of metier table for assigning fishing activity to Metier level
5, based on only gear codes. This is not a universal conversion table,
but an example of a table made for a particular purpose. Some gear
assignments are done with assumptions, and considering the exact metier
codes acceptable by data recipients.

## Usage

``` r
data(metier5table)
```

## Format

[`MetierTable`](MetierTable.md) with column 'gearcode' identifying gear
codes used in Norwegian fisheries data (derived from NS 9400)

## Details

For example NS9400 has codes for nephrops and shrimp trawls, and does
not necessarilly distinguish pelagic shrimp trawls from bottom shrimp
trawls. Also the data recipient did not accept the FAO codes for
nephrops and shrimp trawls, so all of these gears are assigned to OTB,
but target assemblage was set to either "DEF" (demershal fish) or "CRU"
(crustaceans).

There are other examples of pragmatic code-mapping as well. This
particular one was explained in order to caution against indiscriminate
use. The code example below annotates an activity census of COD-catches
with FAO-gearcode declarations and target sepcies declarations, and
compares the annotated metiers with the codes used for reporting catch
where shrimp was declared as target.

## Examples

``` r
data(metier5table)
data(activityCensus)
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
```
