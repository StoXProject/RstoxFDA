# Run R-ECA

Runs `eca.estimate` and `eca.predict`. Not to be confused with
[`RunRecaEstimate`](RunRecaEstimate.md), which is primarily intended for
including in Stox projects.

## Usage

``` r
runRECA(
  RecaObj,
  nSamples,
  burnin,
  lgamodel = "log-linear",
  fitfile = "fit",
  predictfile = "pred",
  resultdir = NULL,
  thin = 10,
  delta.age = 0.001,
  seed = NULL,
  caa.burnin = 0
)
```

## Arguments

- RecaObj:

  [`RecaData`](RecaData.md) as returned from [`prepRECA`](prepRECA.md)
  or [`PrepareRecaEstimate`](PrepareRecaEstimate.md)

- nSamples:

  number of MCMC samples that will be made available for `eca.predict`.
  See documentation for `eca.estimate`,

- burnin:

  number of MCMC samples run and discarded by `eca.estimate` before any
  samples are saved. See documentation for `eca.estimate`.

- lgamodel:

  The length age relationship to use for length-age fits (options:
  "log-linear", "non-linear": Schnute-Richards model). See documentation
  for `eca.estimate`.

- fitfile:

  name of output files in resultdir. See documentation for
  `eca.estimate`.

- predictfile:

  name of output files in resultdir. See documentation for
  `eca.predict`.

- resultdir:

  a directory where Reca may store temp-files `eca.estimate` and
  `eca.predict`. . If NULL, a temporary directory will be created. See
  documentation for `eca.estimate`.

- thin:

  controls how many iterations are run between each samples saved. This
  may be set to account for autocorrelation introduced by
  Metropolis-Hastings simulation. see documentation for `eca.estimate`

- delta.age:

  see documentation for `eca.estimate`

- seed:

  see documentation for `eca.estimate`

- caa.burnin:

  see documentation for `eca.predict`

## Value

[`RecaResult`](RecaResult.md) results from Reca run.

## Details

`eca.estimate` performs Markov-chain Monte Carlo (MCMC) simulations to
determine maximum likelihood of parameters for the given samples.

`eca.predict` samples the posterior distributions of parameters
estimated in `eca.estimate`, in order to obtain proportinos of catches
and fish parameters. Using these parameters and the given total
landings, predictions of distribution of catch-parameter distributions
will be calculated.

If resultdir is NULL, a temporary directory will be created for its
purpose. This will be attempted removed after execution. If removal is
not successful a warning will be issued which includes the path to the
temporary directory.

## Examples

``` r
 data(recaDataExample)

 # run (produce recaPrediction as in data(recaPrediction))
 if (FALSE) recaPrediction <- runRECA(recaDataExample, 500, 5000)$prediction # \dontrun{}
```
