# Traceplot

Plots a paneled plot with traces for age groups. That is the value for
total catch at age in each iteration of `eca.predict`

## Usage

``` r
plotAgeTraces(
  prediction,
  unit = "millions",
  plusGroup = NULL,
  nclust = 4,
  iter.max = 20,
  nstart = 10,
  agecolors = NULL,
  lowerquant = 0.05,
  upperquant = 0.95,
  catlimit = 8,
  title = ""
)
```

## Arguments

- prediction:

  as returned by `eca.predict` or [`runRECA`](runRECA.md).

- unit:

  unit of traced estimates. See details.

- plusGroup:

  Fish this age or older will be grouped in one trace.

- nclust:

  the number of plots to distribute the ages and plus group on

- iter.max:

  maximal number of iterations for k-means clustering deciding which
  ages are plotted in same plot.

- nstart:

  the number of random sets chosen for the k-means clustering

- agecolors:

  named vector matching ages to colors, if null a default color scheme
  is used

- lowerquant:

  lower quantile in each age group to plot as points

- upperquant:

  upper quantile in each age group to plot as points

- catlimit:

  the upper limit for number of ages in a plot using categorical
  coloring. Plots with more than this number of ages will use a gradient
  coloring scheme

- title:

  main title for plot

## Details

The number of iterations of `eca.predict` is determined by the parameter
'nSamples' to `eca.estimate` or [`runRECA`](runRECA.md) and the
parameter 'caa.burnin' (different from parameter 'burnin') to
`eca.predict` or [`runRECA`](runRECA.md)

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

In order to trade off readability of plots with the number of panels
needed. the traces are clustered with a simple clustering algorithm and
grouped in the same plots accordingly. Adjust clustering parameters, to
get fewer or more plots.

## Examples

``` r
 data(recaPrediction)
 plotAgeTraces(recaPrediction, plusGroup=13, nclust = 6)
#> Warning: `aes_()` was deprecated in ggplot2 3.0.0.
#> ℹ Please use tidy evaluation idioms with `aes()`
#> ℹ The deprecated feature was likely used in the RstoxFDA package.
#>   Please report the issue at <https://github.com/StoXProject/RstoxFDA/issues>.
```
