# Run Reca.

This function is deprecated and are being kept for testing purposes. It
is replaced by [`ParameterizeRecaModels`](ParameterizeRecaModels.md) and
[`RunRecaModels`](RunRecaModels.md)

## Usage

``` r
RunRecaEstimate(
  RecaData,
  Nsamples = integer(),
  Burnin = integer(),
  Thin = integer(),
  Lgamodel = c("log-linear", "non-linear"),
  Resultdir = character(),
  Delta.age = numeric(),
  Seed = numeric(),
  Caa.burnin = numeric()
)
```

## Arguments

- RecaData:

  [`RecaData`](RecaData.md) as returned from
  [`PrepareRecaEstimate`](PrepareRecaEstimate.md)

- Nsamples:

  number of MCMC samples that will be made available for `eca.predict`.
  See documentation for `eca.estimate`,

- Burnin:

  number of MCMC samples run and discarded by `eca.estimate` before any
  samples are saved. See documentation for `eca.estimate`.

- Thin:

  controls how many iterations are run between each samples saved.
  Defaults to 0. This may be set to account for autocorrelation
  introduced by Metropolis-Hastings simulation. see documentation for
  `eca.estimate`

- Lgamodel:

  The length age relationship to use for length-age fits (options:
  "log-linear", "non-linear": Schnute-Richards model). See documentation
  for `eca.estimate`.

- Resultdir:

  a directory where Reca may store temp-files `eca.estimate` and
  `eca.predict`. . If not given a temporary directory will be created.
  See documentation for `eca.estimate`.

- Delta.age:

  see documentation for `eca.estimate`. Defaults to 0.001.

- Seed:

  see documentation for `eca.estimate`. Defaults to random seed.

- Caa.burnin:

  see documentation for `eca.predict`. Defaults to 0.

## Value

[`RecaResult`](RecaResult.md) results from Reca run.

## Details

`eca.estimate` performs Markov-chain Monte Carlo (MCMC) simulations to
determine maximum likelihood of parameters for the given samples.

`eca.predict` samples the posterior distributions of parameters
estimated in `eca.estimate`, in order to obtain proportions of catches
and fish parameters. Using these parameters and the given total
landings, predictions of distribution of catch-parameter distributions
will be calculated.

If resultdir is NULL, a temporary directory will be created for its
purpose. This will be attempted removed after execution. If removal is
not successful a warning will be issued which includes the path to the
temporary directory.
