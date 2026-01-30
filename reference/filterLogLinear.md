# Filter length-weight outliers

Removes fish records that fall outside an acceptable weight-length
region defined by the log-linear model

weight=alfa\\length^beta\\exp(epsilon); epsilon~N(0,sigma^2)

with alfa=exp(logalfa), logalfa and other parameters corresponding to
arguments to this function.

## Usage

``` r
filterLogLinear(
  individuals,
  logalfa,
  beta,
  sigma,
  kAl,
  kAu = kAl,
  weightCol = "IndividualRoundWeight",
  lengthCol = "IndividualTotalLength"
)
```

## Arguments

- individuals:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) of
  fish records

- logalfa:

  The alfa parameter (on a log scale) for the log-linear model

- beta:

  The beta parameter for the log-linear model

- sigma:

  The standard deviation of weight for the log-linear model

- kAl:

  Number of standard deviations (on a log scale) that defines the lower
  limit of the acceptable region

- kAu:

  Number of standard deviations (on a log scale) that defines the upper
  limit of the acceptable region

- weightCol:

  name of column in 'individuals' that contain fish weight (in a unit
  corresponding to alfa and beta). Default correspond to the Individual
  level of
  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html),
  but see details.

- lengthCol:

  name of column in 'individuals' that contain fish length (in a unit
  corresponding to alfa and beta). Default correspond to the Individual
  level of
  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)

## Value

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html), like
individuals, but with some records removed.

## Details

This function is intended to provide the same filtering that is offered
in ECA 3.x and ECA 4.x for removing outliers based on a log-linear
weight-length model, and function arguments are named to correspond to
the naming convention used in ECA.

Records are removed if their weights that outside the range from:
alfa\\L^beta\\exp(kAu\\sigma) to alfa\\L^beta\\exp(-kAl\\sigma)

any records with missing length or weight is not removed.

Note that kAl and kAu are given on a log scale, so that the acceptable
region is not symmetric around the growth curve when kAl=kAu.
