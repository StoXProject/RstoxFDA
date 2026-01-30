# Catch at Age result table

Produces common reported statistics for Catch-At-Age estimation.

## Usage

``` r
makeResultTableRECA(
  prediction,
  unit = "millions",
  plusGroup = NULL,
  alpha = 0.05
)
```

## Arguments

- prediction:

  as returned by `eca.predict` or [`runRECA`](runRECA.md).

- unit:

  unit of reported estimates. See details.

- plusGroup:

  Fish this age or older will be grouped in report

- alpha:

  value for percentiles.

## Value

data.table() with columns:

- age:

  Age statistics are reported for

- total:

  Estimated total catch of age group

- unit:

  Unit for total catch and standard deviation for estimated total

- sd:

  Standard deviation for estimated total

- cv:

  Coefficient of variation for estimated total

- lowerQuantile:

  Lower quantile (100\*alpha/2 percentile) for estimated total

- UpperQuantile:

  Upper quantile: (100\*(1-alpha/2) percentile) for estimated total

- alpha:

  alpha for quantiles.

## Details

parameter 'unit' supports:

- number:

  Catch at age as number of fish

- thousands:

  Catch at age as number of fish in thousands

- millions:

  Catch at age as number of fish in millions

- kg:

  Catch at age as mass in kilogrammes

- T:

  Catch at age as mass in tons

- kT:

  Catch at age as mass in kilotonnes

## Examples

``` r
 data(recaPrediction)
 makeResultTableRECA(recaPrediction, plusGroup=13)
#>        age        total     unit upperQuantile lowerQuantile alpha
#>     <char>        <num>   <char>         <num>         <num> <num>
#>  1:      2   5.23740860 millions    7.55175127  3.366104e+00  0.05
#>  2:      3  62.89020409 millions   74.67054655  5.180856e+01  0.05
#>  3:      4 147.06215922 millions  158.91098719  1.354573e+02  0.05
#>  4:      5  48.71480282 millions   57.20106384  4.137034e+01  0.05
#>  5:      6  19.57404860 millions   23.28896437  1.620716e+01  0.05
#>  6:      7   6.91958273 millions    9.25782397  4.725750e+00  0.05
#>  7:      8   2.33974128 millions    3.53527429  1.251480e+00  0.05
#>  8:      9   1.58758079 millions    2.46972328  5.355693e-01  0.05
#>  9:     10   0.00000000 millions    0.00000000  0.000000e+00  0.05
#> 10:     11   0.14879672 millions    0.53504808  4.868518e-06  0.05
#> 11:     12   0.03552055 millions    0.10746717  0.000000e+00  0.05
#> 12:    13+   0.02368909 millions    0.08808582  4.384655e-05  0.05
```
