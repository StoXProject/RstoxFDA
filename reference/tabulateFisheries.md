# Tabulate fisheries

Tabulates fisheries based on custom cell definitions

## Usage

``` r
tabulateFisheries(
  data,
  weightCol = "LiveWeightKG",
  cellCols = c("Metier5", "quarter", "Area"),
  complete = F
)
```

## Arguments

- data:

  data.frame containg fisheries data

- weightCol:

  character() column in 'data' that contains catch weights

- cellCols:

  character() vector of columns in 'data' defining cells

- complete:

  logical() whether all combinations of the columns in cellCols should
  be tabulated, even if they contain no catch.

## Value

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
the cells specified in cellCols tabulated by decreasing weight and with
the columns 'weight', 'frac' and 'cumFrac' containing the weight in each
cells and the fraction and cumulative fraction of total weight in that
cell.

## Details

A fishery will be decomposed into cells, within which total weight will
be reported.

## Examples

``` r
 data(landings)
 tabulateFisheries(landings)
#>      Metier5 quarter      Area   weight         frac   cumFrac
#>       <char>  <char>    <char>    <num>        <num>     <num>
#>   1: OTB_DEF      Q1  27.2.a.2 16657454 1.746740e-01 0.1746740
#>   2: OTB_DEF      Q4  27.2.b.2  6551725 6.870294e-02 0.2433770
#>   3: LLS_DEF      Q4    27.1.b  6292260 6.598213e-02 0.3093591
#>   4: OTB_DEF      Q2  27.2.a.2  5414982 5.678278e-02 0.3661419
#>   5: LLS_DEF      Q1  27.2.a.2  5221458 5.475345e-02 0.4208953
#>  ---                                                          
#> 189: FPO_DEF      Q1 27.3.a.20        7 7.340366e-08 0.9999999
#> 190: PTB_DEF      Q2 27.3.a.20        5 5.243119e-08 0.9999999
#> 191: LHM_DEF      Q3    27.4.b        5 5.243119e-08 1.0000000
#> 192: LHM_DEF      Q4 27.3.a.20        1 1.048624e-08 1.0000000
#> 193: OTB_DEF      Q2 27.14.b.2        0 0.000000e+00 1.0000000
```
