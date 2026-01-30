# Reca Parameter Data (RecaParameterData)

Data and some data parameters prepared for running various report
functions that invoke `eca.predict`.

## model fit

For inspection or analysis of model fit, the lists 'FitProportionAtAge',
'FitLengthGivenAge' and 'FitWeightGivenLength' is of interest. For
stock-splitting analysis, the lists FitLengthGivenAgeCC and
FitWeightGivenLengthCC will be added as well, corresponding to one of
the stocks. These lists correspond to the three Reca-models and contain:

- LogLikeliehood:

  A [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  tabulating the logarithm of the likeliehood of the parameter set for
  each iteration

- ...:

  A [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  for each of the model effects (e.g. covariates).

In addition to configurable covariates, the models always contain a
constant effect (named 'constant'), a catch or haul effect (named
'catchSample') and effects for fish measurements (named 'fish'). Where
relevant the following parameters may be tabulated for each effect:

- Age:

  Identifying the age the effect applies to

- Level:

  Identifying the value or level of the covariate the effect applies to

- Iteration:

  Identifying the iteration the fit is provided for

- AgeIndex:

  Age identifier used internally in Reca

- LevelIndex:

  Level identifier used internally in Reca

- Slope:

  The value of the regression slope

- tau_Slope:

  The value of tau parameter for the regression slope

- ar_Slope:

  The value of regression slope of a the autoregressive coefficient
  associated with the effect

- Intercept:

  The value of the regression intercept

- tau_Intercept:

  The value of tau parameter for the regression intercept

- ar_Intercept:

  The value of the regression intercept of a autoregressive coefficient
  associated with the effect

Consult Hirst et.al. 2005 for description of the parameters

@section other data: The lists 'AgeLength', 'WeightLength', 'Landings',
'GlobalParameters' and 'CovariateMaps' may be passed to `eca.predict` in
functions consuming output from this function. All in all the following
lists can be accessed on RecaParameterData objects:

- FitProportionAtAge:

  list of data tables with parameters for for the Proportion-at-age
  model

- FitLengthGivenAge:

  list of data tables with parameters for for the Length-given-age model

- FitWeightGivenLength:

  list of data tables with parameters for for the Weight-given-length
  model

- AgeLength:

  input needed for `eca.estimate` and `eca.predict`

- WeightLength:

  input needed for `eca.estimate` and `eca.predict`

- Landings:

  input needed for `eca.estimate` and `eca.predict`

- GlobalParameters:

  input needed for `eca.estimate` and `eca.predict`. see details

- CovariateMaps:

  Mapping of values for each covariate in landings and samples
  (including non-configurable catchId) to integer value used in R-ECA.
