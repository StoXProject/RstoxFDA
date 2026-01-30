# List Difference

List the difference between a NMDbiotic v 3.x data set and a StoxBiotic
data set. Appropriate when BioticData is read from NMDbiotic.

## Usage

``` r
ListBioticDifference(StoxBioticData, BioticData)
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  data set.

- BioticData:

  [`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) that
  has been read from a NMDbiotic v 3.x file

## Value

[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) with
data that is not found in 'StoxBioticData'.

## Details

Lists all missions, fishstations, catchsamples and individuals from a
NMDbiotic v 3.x data set that is not present in a given StoxBiotic data
set.

This may be used to summarize the effect of filters and data
conversions.
