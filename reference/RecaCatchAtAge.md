# Reca Results (RecaCatchAtAge)

Posterior distribution of total catch at age and weight and length
parameters.

## Details

a list of data tables:

- CatchAtAge:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  tabulating the estimated catch-at-age by length group for each Reca
  iteration (MCMC sample)

- MeanLength:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  tabulating the mean length in cm by age for each Reca iteration (MCMC
  sample)

- MeanWeight:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  tabulating the mean weight in g by age for each Reca iteration (MCMC
  sample)

- GroupingVariables:

  [`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with any variables that catch-at-age estimates are partitioned on in
  the column 'GroupingVariables'. These may correspond to variables in
  the landings, or maye be the variable 'Stock' if stock-splitting
  analysis have been perfomred.

In addition to columns for the variables in 'GroupingVariables', the
data tables 'CatchAtAge', 'MeanLength', and 'MeanWeight' have the
following variables:

- Age:

  Age in number of years.

- Iteration:

  The Reca iteration (MCMC sample) that estimates are calculated for

'CatchAtAge' also have the variables

- Length:

  Upper limit of length group in cm

- CatchAtAge:

  The total catch at age in numbers

'MeanLength' has the variable:

- MeanIndividualLength:

  Mean Length at age in cm

'MeanWeight' has the variable:

- MeanIndividualWeight:

  Mean weight at age in g
