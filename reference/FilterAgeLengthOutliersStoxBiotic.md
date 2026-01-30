# Filter outliers

Removes fish records that fall outside an acceptable age-length region
defined by a von-Bertalanffy growth curve:

length=Linf(1-exp(-K\\age))\\exp(epsilon); epsilon~N(0,sigma^2)

with parameters corresponding to arguments to this function.

## Usage

``` r
FilterAgeLengthOutliersStoxBiotic(
  StoxBioticData,
  FilterUpwards = FALSE,
  Linf = numeric(),
  K = numeric(),
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

- Linf:

  Asymptotic length for the von-Bertalanffy growth curve (in cm)

- K:

  The growth coefficient for the von-Bertalanffy growth curve

- sigma:

  The standard deviation of length for the von-Bertalanffy growth curve
  (in cm)

- kAl:

  Number of standard deviations (on a log scale) that defines the lower
  limit of the acceptable region

- kAu:

  Number of standard deviations (on a log scale) that defines the upper
  limit of the acceptable region. Defaults to the same value as kAl.

## Value

[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
with individuals outside the acceptable region removed.

## Details

This function is intended to provide the same filtering that is offered
in ECA 3.x and ECA 4.x for removing outliers based on von-Bertalanffy
growth curves, and function arguments are named to correspond to the
naming convention used in ECA.

Records are removed if the length fall outside the range from:
Linf\\(1-exp(-K\\(AGE)))\\exp(kAu\\sigma) to
Linf\\(1-exp(-K\\(AGE)))\\exp(-kAl\\sigma)

Age is provided with a resolutation of 1/12 year, which is approximated
by adding M/12 to the integer age, where M is the number of the month of
catch (e.g. 2 for february). For this reason, 'DateTime' must be
provided for all 'Stations' in 'StoxBioticData'

any records with missing length or age is not removed.

Note that kAl and kAu are given on a log scale, so that the acceptable
region is not symmetric around the growth curve when kAl=kAu.

Some parameterizations that have been used for some stocks at IMR:

- cod.27.1-2 (Gadus morhua):

  Linf=232.98028344, K=0.05284384 sigma=0.16180306 kAl=kAu=4

- had.27.1-2 (Melanogrammus aeglefinus):

  Linf=69.7075536, K=0.2158570, sigma=0.1441575 kAl=kAu=4

- pok.27.1-2 (Pollachius virens):

  Linf=100.5282683, K=0.1392982, sigma=0.0996507 kAl=kAu=4

- pelagic stocks North Atlantic:

  Linf=37.2490509 K=0.3128122 sigma=0.009681094 kAl=kAu=4

## See also

[`FilterWeightLengthOutliersStoxBiotic`](FilterWeightLengthOutliersStoxBiotic.md)
