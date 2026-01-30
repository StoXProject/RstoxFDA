# Convert RecaData

Converts [`RecaData`](RecaData.md) prepared for StoX-Reca to the input
format accepted by `eca.estimate`. This facilitates preparing input data
with a StoX-project for more cusomized programs directly interfacing
Reca.

## Usage

``` r
convertRecaData(
  RecaData,
  nSamples = as.integer(NA),
  burnin = as.integer(NA),
  thin = as.integer(NA),
  resultdir = as.character(NA),
  lgamodel = as.character(NA),
  delta.age = as.numeric(NA),
  seed = as.numeric(NA),
  fitfile = as.character(NA),
  predictfile = as.character(NA)
)
```

## Arguments

- RecaData:

  [`RecaData`](RecaData.md), as prepared by
  [`PrepareRecaEstimate`](PrepareRecaEstimate.md).

- nSamples:

  nSamples as specified in `eca.estimate`

- burnin:

  burnin as specified in `eca.estimate`

- thin:

  thin as specified in `eca.estimate`

- resultdir:

  resultdir as specified in `eca.estimate`

- lgamodel:

  lgamodel as specified in `eca.estimate`

- delta.age:

  delta.age as specified in `eca.estimate`

- seed:

  seed as specified in `eca.estimate`

- fitfile:

  fitfile as specified in `eca.estimate`

- predictfile:

  predictfile as specified in `eca.estimate`

## Value

list with 5 members:

- AgeLength:

  Data prepared for the argument AgeLength to `eca.estimate`

- WeightLength:

  Data prepared for the argument WeightLength to `eca.estimate`

- Landings:

  Data prepared for the argument Landings to `eca.estimate`

- GlobalParameters:

  Data prepared for the argument GlobalParameters to `eca.estimate`,
  Completeness of this data structure depend on whether optional
  parameters where provided.

- CovariateMaps:

  Not input to Reca. Nested list, providing mapping between integer
  encoding and character encoding of the levels of categorical
  variables.

## Details

The StoX-function interfacing Reca-parameterisation
([`RunRecaModels`](RunRecaModels.md)) accepts a data format
[`RecaData`](RecaData.md) that is not exactly compatible with the format
accepted by `eca.estimate`. This inconvenience is introduced to make
sure that output from [`PrepareRecaEstimate`](PrepareRecaEstimate.md)
adheres to restrictions in the StoX-user interface, so that the results
are inspectable there.

Reca-input include model parameters and options, but
[`PrepareRecaEstimate`](PrepareRecaEstimate.md) only sets parameters
that have implications for data formatting, other parameters are
subsequently set in for instance [`RunRecaModels`](RunRecaModels.md).
The optional arguments to this function can be used to set those
parameters that is handled by [`RunRecaModels`](RunRecaModels.md) in the
StoX-workflow.

Reca requires categorical variables to be encoded as consecutive
integers, starting with 1. In order to keep track of how this correspond
to other representations, this function provides the mapping in the list
'CovariateMaps'. CovariateMaps is not an input to Reca, but only
provided to assist book-keeping.
