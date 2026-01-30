# Set short gear codes biotic

Set short gear codes for all stations. Appropriate when BioticData is
read from NMDbiotic.

## Usage

``` r
SetShortGearBiotic(BioticData)
```

## Arguments

- BioticData:

  [`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) data
  for which short gear codes should be set

## Value

[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html)

## Details

Set the column 'gear' on the table 'fishstation' in
[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) to the
first two characters / digits, for all rows where the gear code
currently consist of four characters.

The gear codes in NMDBiotic are conventionally organised hierarchically,
so that the two first characters denote a gear group. The two
character/digit system is more convenient to work with in many
circumstances.

'gear' is on the table 'fishstation' in data originating from NMDBiotic
(http://www.imr.no/formats/nmdbiotic/). For bioticdata that does not
conform to this, no modifications are done.

## See also

[`StoxBiotic`](https://rdrr.io/pkg/RstoxData/man/StoxBiotic.html) For
converting
[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) to
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html).
