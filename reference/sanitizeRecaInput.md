# Input sanitation for Reca

Performs input sanitation for Reca

## Usage

``` r
sanitizeRecaInput(
  AgeLength = NULL,
  WeightLength = NULL,
  Landings = NULL,
  GlobalParameters = NULL,
  stage = c("dataprep", "parameterize", "predict")
)
```

## Arguments

- AgeLength:

  list that is to be provided as the AgeLength argument to
  `eca.estimate` or `eca.predict`

- WeightLength:

  list that is to be provided as the AgeLength argument to
  `eca.estimate` or `eca.predict`

- Landings:

  list that is to be provided as the AgeLength argument to
  `eca.estimate` or `eca.predict`

- GlobalParameters:

  list that is to be provided as the AgeLength argument to
  `eca.estimate` or `eca.predict`

- stage:

  character specifying which stage the sanitation should be performed
  for. See details.

## Details

The main interfaces to the Reca-package is `eca.estimate` and
`eca.predict`. This have strict formatting requirements, well documented
in the help pages for these functions, but they do not perform much
input sanitation. This function checks that data is formatted in
accordance with the requirements.

The input formats are shared between the `eca.estimate` and
`eca.predict` and the formatting requirements are largely overlapping.
Sanitation has therefore been conseptually divided into three stages,
and which checks are to be performed is controlled by the argument
'stage'. The three stages are:

- dataprep:

  check that data is correctly and consistently formatted, and check
  that all global parameters that have implications for data formatting
  are provided.

- parameterize:

  check that arguments are valid input to `eca.estimate`

- predict:

  check that arguments are valid input to `eca.predict`

Errors or warnings are raised on any issue, and this function does not
have a meaningful return value.

All or some inputs to the Reca-functions may be provided for sanitation,
and available checks will be run accordingly. As Reca impose some
restriction on consistency in formatting between the different
arguments, some checks require certain combination of arguments to be
provided. For instance, checks on consistent encoding of variables in
'AgeLength' and 'Landings' are only performed if both 'AgeLength' and
'Landings' are provided.
