# Run Reca Models

Runs prediction (catch-at-age estimate) for parameterized Reca models.

## Usage

``` r
RunRecaModels(
  RecaParameterData,
  StoxLandingData,
  GroupingVariables = character(),
  TemporalResolution = c("Quarter", "Month"),
  Caa.burnin = numeric(),
  CollapseLength = TRUE
)
```

## Arguments

- RecaParameterData:

  Parameters for Reca models.

- StoxLandingData:

  Landings data
  ([`StoxLandingData`](https://rdrr.io/pkg/RstoxData/man/StoxLandingData.html)).

- GroupingVariables:

  character vector identifying columns in 'StoxLandingData' that results
  should be provided for.

- TemporalResolution:

  Code for temporal resolution in landings: "Month" or "Quarter".
  Defaults to
  `r RstoxFDA::stoxFunctionAttributes$RunRecaModels$functionParameterDefaults$TemporalResolution`.
  Regulates temporal resolution for calculating fractional ages of fish.
  Not to be confused with any temporal covariate.

- Caa.burnin:

  see documentation for `eca.predict`. Defaults to
  `r RstoxFDA::stoxFunctionAttributes$RunRecaModels$functionParameterDefaults$Caa.burnin`.

- CollapseLength:

  indicates whether length groups should be collapsed in result.
  Defaults to TRUE. See details.

## Value

[`RecaCatchAtAge`](RecaCatchAtAge.md)

## Details

Parameters may be obtained with
[`ParameterizeRecaModels`](ParameterizeRecaModels.md). If the
function-parameter 'GroupingVariables' is provided, predictions will be
provided for corresponding partitions of landings. The parameter
'StoxLandingData' may differ from the landings used in parameterisation
(passed to [`ParameterizeRecaModels`](ParameterizeRecaModels.md)), as
long as all not additional values / levels for the model covariates /
effects are introduced.

If The models are configured for stock-splitting analysis. The variable
'Stock' will be added to 'GroupingVariables' in the return value
([`RecaCatchAtAge`](RecaCatchAtAge.md))

If the 'GroupingVariables' specify a very large number of partitions of
the landings, this function may exhaust available computer memory.

### CollapseLength

By default length groups are collapsed into one length group in the
result. This does not facilitate reporting length resolved data, such as
length distributions. The full age-length prediction may be extracted by
setting the parameter 'CollapseLength' to FALSE. If this is used in
combination with several grouping variables, there is some risk of
exhausting available computer memory.

## See also

[`ParameterizeRecaModels`](ParameterizeRecaModels.md) for model
parameterisation, [`ReportRecaCatchAtAge`](ReportRecaCatchAtAge.md),
[`ReportRecaLengthAtAge`](ReportRecaLengthAtAge.md),
[`ReportRecaWeightAtAge`](ReportRecaWeightAtAge.md) for compiling
reports of predictions / estimates.
