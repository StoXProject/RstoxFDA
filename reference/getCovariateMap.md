# Make Covariate Map

Constructs a covariate map for R-ECA

## Usage

``` r
getCovariateMap(covariate, samples, landings)
```

## Arguments

- covariate:

  character() name of covariate to make map for

- samples:

  data.table() the samples the covariate mapping should be defined for
  (must contain column specified by 'covariate')

- landings:

  data.table() the landings the covariate mapping should be defined for
  (may contain column specified by 'covariate')

## Value

list() where the nth member represents the RECA covariate value 'n', so
that for a covarate 'cov', the integer i is used for value a, when
covariateMaps\[\[cov\]\]\[\[i\]\] == a

## Details

RECA require all covariate to be formatted as integers. In order to
maintain connection to other formats for the covariates, they should be
converted back and forth with a pre-defined covariate map

## Examples

``` r
 data(catchsamples)
 data(landings)
 catchsamples$Metier5 <- catchsamples$LEmetier5
 landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
 getCovariateMap("Metier5", catchsamples, landings)
#> [[1]]
#> [1] "LLS_DEF"
#> 
#> [[2]]
#> [1] "SSC_DEF"
#> 
#> [[3]]
#> [1] "GNS_DEF"
#> 
#> [[4]]
#> [1] "OTB_DEF"
#> 
#> [[5]]
#> [1] "LX_DEF"
#> 
#> [[6]]
#> [1] "PS_DEF"
#> 
#> [[7]]
#> [1] "OTT_DEF"
#> 
#> [[8]]
#> [1] "PTB_DEF"
#> 
#> [[9]]
#> [1] "LHM_DEF"
#> 
#> [[10]]
#> [1] "LLD_DEF"
#> 
#> [[11]]
#> [1] "GND_DEF"
#> 
#> [[12]]
#> [1] "OTM_DEF"
#> 
#> [[13]]
#> [1] "FPO_DEF"
#> 
#> [[14]]
#> [1] "MIS_DEF"
#> 
```
