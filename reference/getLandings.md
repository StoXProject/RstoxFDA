# Formats landings for R-ECA.

Prepares a Landings object as required by `eca.predict`.

## Usage

``` r
getLandings(
  landings,
  covariates,
  covariateMaps,
  date = NULL,
  month = NULL,
  quarter = NULL
)
```

## Arguments

- landings:

  data.table() with total landings (as in [`prepRECA`](prepRECA.md), and
  [`rEcaDataReport`](rEcaDataReport.md)), each row corresponding to one
  cell. Contains columns:

  LiveWeightKG

  :   numeric(). Total landings (Live/Round weight in Kg) for the cell

  ...

  :   Additional columns to be used as covariates. These define each
      cell. Type of covariate must be sepcified in 'fixedEffects',
      'randomEffects' or 'carEffect'

- covariates:

  character() vector of covariates used in model

- covariateMaps:

  list() mapping covariate values from integers used in RECA to values
  used in 'landings'. For a covarate 'cov', the integer i is used for
  value a, when covariateMaps\[\[cov\]\]\[\[i\]\] == a

- date:

  POSIXct() vector, matching the number of rows in 'landings', date of
  catch, see details.

- month:

  integer() vector, matching the number of rows in 'landings', month of
  catch (1 for January, etc.), see details.

- quarter:

  integer() vector, vector, matching the number of rows in 'landings',
  quarter of catch (1 for Q1, etc.), see details.

## Value

Landings object as required by `eca.predict`

## Details

The parameters 'date', 'month', and 'quarter' are used to set the
temporal resolution for catch at age prediction. Provide exactly one of
these, and set the other ones to NULL. Temporal resolution need not
match any temporal covariate used. One can for example run with month,
even if Quarter is a covariate in the model.

## Examples

``` r
 data(catchsamples)
 data(landings)
 catchsamples$Metier5 <- catchsamples$LEmetier5
 landings$Metier5 <- landings$FishingActivityCategoryEuropeanLvl5
 covMap <- getCovariateMap("Metier5", catchsamples, landings)
 getLandings(landings, c("Metier5"), covMap, month=landings$Month)
#> $AgeLengthCov
#>      constant Metier5  midseason
#>         <num>   <int>      <num>
#>   1:        1      NA 0.04166667
#>   2:        1      NA 0.04166667
#>   3:        1      NA 0.04166667
#>   4:        1      NA 0.04166667
#>   5:        1      NA 0.04166667
#>  ---                            
#> 136:        1      NA 0.95833333
#> 137:        1      NA 0.95833333
#> 138:        1      NA 0.95833333
#> 139:        1      NA 0.95833333
#> 140:        1      NA 0.95833333
#> 
#> $WeightLengthCov
#>      constant Metier5  midseason
#>         <num>   <int>      <num>
#>   1:        1      NA 0.04166667
#>   2:        1      NA 0.04166667
#>   3:        1      NA 0.04166667
#>   4:        1      NA 0.04166667
#>   5:        1      NA 0.04166667
#>  ---                            
#> 136:        1      NA 0.95833333
#> 137:        1      NA 0.95833333
#> 138:        1      NA 0.95833333
#> 139:        1      NA 0.95833333
#> 140:        1      NA 0.95833333
#> 
#> $LiveWeightKG
#>   [1]     286      25  250036    1830  194018 3273227 1748348      50 4185134
#>  [10]     431   47688  396872    5392       2  476648    3346  227733 3834393
#>  [19] 2074242 9918271     576     182    1502  517766     256      59  816156
#>  [28]    1722   42750 1708199  401854       9 8097981   28918    1487  949050
#>  [37]     284      56  644914   10132    6942 1941510   85133     164 4581130
#>  [46]   18510    2436       2       6 1446977      20   86741   10093    6660
#>  [55] 2252695  159199  952184    7145    1050      31    7610 3004477     403
#>  [64]   39027    5667   43964  878267  195451 1453760      90     809    3697
#>  [73] 1194960     106   46192   33292 1089375  667459  670795 1718245    5049
#>  [82]     412   10907 2680842     312   77216    9135  250326  553660  285489
#>  [91]  599404     145   14485 2040036     217     762   68714     990   31878
#> [100]  631597  374483  474991    1655     114    2714 1124215     257     286
#> [109]   97397    2543   44809 2217342  792741      28 2872604    1576    1850
#> [118]  464053     394      12  108891    4177   71908 4586319 1469352 1613570
#> [127]     965     216   14420  269620    1124   42722     483   52854 4263132
#> [136] 1071761 3355643     460   14715  211022
#> 
```
