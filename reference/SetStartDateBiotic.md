# Set startdate Biotic

Set start date to stop date. Appropriate when BioticData is read from
NMDbiotic.

## Usage

``` r
SetStartDateBiotic(BioticData, Overwrite = F)
```

## Arguments

- BioticData:

  [`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) data
  for which time should be set

- Overwrite:

  if True any existing values in stationstartdate will be overwritten.

## Value

[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html)

## Details

Set the column 'stationstartdate' on the table 'fishstation' in
[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) to the
value of 'stationstopdate'.

Setting stationstartdate facilitates conversion to
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
using [`StoxBiotic`](https://rdrr.io/pkg/RstoxData/man/StoxBiotic.html),
and is applicable if daily resolution of date and time is not critical
for subsequent analysis.

These columns are on the table 'fishstation' in data originating from
NMDBiotic (http://www.imr.no/formats/nmdbiotic/). For bioticdata that
does not conform to this, no modifications are done.

## See also

[`StoxBiotic`](https://rdrr.io/pkg/RstoxData/man/StoxBiotic.html) For
converting
[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) to
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html).
