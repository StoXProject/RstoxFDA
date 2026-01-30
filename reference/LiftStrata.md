# Unify strata for AnalyticalPSUEstimateData

Populates unreported strata with zeroes and NaNs.

## Usage

``` r
LiftStrata(AnalyticalPSUEstimateData)
```

## Arguments

- AnalyticalPSUEstimateData:

  [`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md) with
  estimates for all strata for each PSU that has positive abundance.

## Value

[`AnalyticalPSUEstimateData`](AnalyticalPSUEstimateData.md) with zeroes
and NaNs inferred for strata that have zero abundance in a PSU.

## Details

AnalyticalPSUEstimateData may be provided with stratification for each
Primary Sampling Unit. These estimates cannot be combined into
population level estimates unless the same strata are provided for all
Primary Sampling Units (PSUs). See
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md). This
may not be the case for two reasons. The stratification may be local to
each PSU, and no common stratification exists between PSUs. For
instance, stratified sub-sampling of a catch may have taken place where
the criteria for stratification was only used for that particular catch.
Otherwise, some strata may not be estimated for some samples, simply
because they did not exist there. For instance stratification by length
groups can use a common length stratification scheme for all PSUs, but
not all of these strata will exist for each catch.

If strata are omitted because they do not exist for a particular Sample,
Abundance, Frequencies and Totals can be inferred to be zero, while
Means are undefined. Application of this functions introduces zeros
(Abundance, Frequency and Total) and NaN (Means) for all missing strata
for all samples in 'AnalyticalPSUEstimateData'

In the case where strata definitions are local, the stratification are
only useful for the calculation of sampling parameters. If this is the
case consider using the function [`CollapseStrata`](CollapseStrata.md)
before producing estimates for each sample.
