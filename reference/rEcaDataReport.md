# Data report for R-ECA preparation

Generates overview of samples to inform on sample availability in
potential cell definitions. Informs on which columns maybe be used as
fixed effect covariates, and how grouping of covariates is best done for
random effects.

The covariates in landings define the cells. For each cell the
covariates defining the cell is reported, before the total landed
weight, along with the number of unique occurances of covariates not in
landings (including catchId and sampleId). Lastly the number of fish
measurements for Age, Weight and Length is reported.

## Usage

``` r
rEcaDataReport(samples, landings, covariates)
```

## Arguments

- samples:

  data.table() with samples (as in [`prepRECA`](prepRECA.md)), each row
  corresponding to one sampled fish. Contains columns:

  catchId

  :   Column identifying the catch that the sample was taken from.
      Typically a haul or a landing.

  sampleId

  :   Column identifying the sample. If only one sample is taken for
      each catch. This can be set equal to catchId

  Age

  :   integer() Age of fish.

  Length

  :   numeric() Length of fish. Must be complete (no NAs)

  Weight

  :   numeric() Weight of fish.

  ...

  :   Additional columns which may be used as covariates.

- landings:

  data.table() with total landings (as in [`prepRECA`](prepRECA.md)),
  each row corresponding to one cell. Contains columns:

  LiveWeightKG

  :   numeric(). Total landings (Live/Round weight in Kg) for the cell

  ...

  :   Additional columns which may be used as covariates. These will
      define each cell.

- covariates:

  character() vector of columns to consider for covariates.

## Value

data.table() with columns

- \<Covariates in landings\>:

  one column for each. Defines the cells.

- LiveWeightKG:

  The total weight (kg) in the cell.

- LiveWeightCumFraction:

  The fraction of landings in this cell AND all the cells with higher
  total weight than this cell.

- \<Count of covariates not in landings\>:

  Count of unique values for covariate. one column for each. Column name
  is covariate name (from samples) prefixed with N

- Ncatch:

  The number of unique catches sampled in the cell.

- Nsample:

  The number of unique catch-samples in the cell.

- Nage:

  The number of age readings in the cell.

- Nweight:

  The number of fish weight measurements in the cell.

- Nlength:

  The number of fish length measurements in the cell.

## Examples

``` r
 data(catchsamples)
 catchsamples$catchId <- catchsamples$LEid
 catchsamples$sampleId <- catchsamples$SAid
 catchsamples$date <- catchsamples$LEdate
 catchsamples$Metier5 <- catchsamples$LEmetier5

 data(landings)
 landings$LiveWeightKG <- landings$OfficialLandingsWeight
 landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5

 rEcaDataReport(catchsamples, landings, c("Metier5", "VDencrCode"))
#>     Metier5 LiveWeightKG LiveWeightCumFraction NVDencrCode Ndate Ncatch Nsample
#>      <char>        <int>                 <num>       <num> <num>  <num>   <num>
#>  1: OTB_DEF     39822917             0.4175925           1     1      1       1
#>  2: LLS_DEF     26807800             0.6987055          34    16     34      35
#>  3: SSC_DEF     14299890             0.8486575          29    23     29      29
#>  4:  LX_DEF      9328848             0.9464820           0     0      0       0
#>  5: GNS_DEF      2754654             0.9753680          17     9     17      17
#>  6: LLD_DEF      2063217             0.9970034           0     0      0       0
#>  7: PTB_DEF       118092             0.9982417           0     0      0       0
#>  8: LHM_DEF        83410             0.9991164           0     0      0       0
#>  9: OTM_DEF        63339             0.9997806           0     0      0       0
#> 10: FPO_DEF         9051             0.9998755           0     0      0       0
#> 11:  PS_DEF         5267             0.9999307           0     0      0       0
#> 12: OTT_DEF         5155             0.9999848           0     0      0       0
#> 13: GND_DEF         1202             0.9999974           0     0      0       0
#> 14: MIS_DEF          251             1.0000000           0     0      0       0
#>      Nage Nweight Nlength
#>     <num>   <num>   <num>
#>  1:    29      29      29
#>  2:   962     962     962
#>  3:   721     721     721
#>  4:     0       0       0
#>  5:   338     338     338
#>  6:     0       0       0
#>  7:     0       0       0
#>  8:     0       0       0
#>  9:     0       0       0
#> 10:     0       0       0
#> 11:     0       0       0
#> 12:     0       0       0
#> 13:     0       0       0
#> 14:     0       0       0
```
