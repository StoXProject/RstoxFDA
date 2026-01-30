# Filter length-weight outliers

Removes fish records that fall outside an acceptable weight-length
region defined by the log-linear model

weight=alfa\\length^beta\\exp(epsilon); epsilon~N(0,sigma^2)

with alfa=exp(logalfa), logalfa and other parameters corresponding to
arguments to this function.

## Usage

``` r
FilterWeightLengthOutliersStoxBiotic(
  StoxBioticData,
  FilterUpwards = FALSE,
  logalfa = numeric(),
  beta = numeric(),
  sigma = numeric(),
  kAl = numeric(),
  kAu = numeric()
)
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  to be filtered

- FilterUpwards:

  Whether the filter action will propagate in the upwards direction
  (removing Samples, Stations, etc. for which all fish records are
  removed). Default to FALSE.

- logalfa:

  The alfa parameter (on a log scale) for the log-linear model. See
  details for units.

- beta:

  The beta parameter for the log-linear model. See details for units.

- sigma:

  The standard deviation of weight for the log-linear model. See details
  for units.

- kAl:

  Number of standard deviations (on a log scale) that defines the lower
  limit of the acceptable region

- kAu:

  Number of standard deviations (on a log scale) that defines the upper
  limit of the acceptable region

## Value

[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
with individuals outside the acceptable region removed.

## Details

This function is intended to provide the same filtering that is offered
in ECA 3.x and ECA 4.x for removing outliers based on a log-linear
weight-length model, and function arguments are named to correspond to
the naming convention used in ECA.

Records are removed if their weights that outside the range from:
alfa\\L^beta\\exp(kAu\\sigma) to alfa\\L^beta\\exp(-kAl\\sigma)

any records with missing length or weight is not removed.

Parameters 'logalfa', 'beta' and 'sigma' must be appropriate for weights
recorded in gram, and lengths recorded in cm.

Note that kAl and kAu are given on a log scale, so that the acceptable
region is not symmetric around the growth curve when kAl=kAu.

Some parameterizations that have been used for some stocks at IMR:

- cod.27.1-2 (Gadus morhua):

  logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl = kAu = 4

- had.27.1-2 (Melanogrammus aeglefinus):

  logalfa = -4.9620, beta = 3.0919, sigma = 0.1282, kAl = kAu = 4

- pok.27.1-2 (Pollachius virens):

  logalfa = -4.7718, beta = 3.0287, sigma = 0.1338, kAl = kAu = 4

## See also

[`FilterAgeLengthOutliersStoxBiotic`](FilterAgeLengthOutliersStoxBiotic.md)
