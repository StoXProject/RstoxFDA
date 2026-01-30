# Collapse strata in sampling design

Transforms a sampling design to an equivalent one with simpler
stratification

## Usage

``` r
CollapseStrata(IndividualSamplingParametersData, RetainStrata = character())
```

## Arguments

- IndividualSamplingParametersData:

  [`IndividualSamplingParametersData`](IndividualSamplingParametersData.md)
  with sampling parameters for sample selection

- RetainStrata:

  character() with the names of stratification variables to retain.
  Stratification variables not specified here will be removed.

## Value

[`IndividualSamplingParametersData`](IndividualSamplingParametersData.md)
with simplified stratification

## Details

The sampling information in
[`IndividualSamplingParametersData`](IndividualSamplingParametersData.md)
allows for specification of several stratification variables. This
function facilitates removal of some of these stratificaiton vairables,
redefining strata to a coarser stratification scheme that still has
known stratum sizes.

If all stratification variables are removed, all samples will be
assigned to a single stratum named 'All'.
