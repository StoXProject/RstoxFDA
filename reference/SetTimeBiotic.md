# Set time Biotic

This function is deprecated, and may be replaced by
[`TranslateBiotic`](https://rdrr.io/pkg/RstoxData/man/TranslateBiotic.html),
with VariableName: stationstarttime, and TranslationTable:
`[{"stationstarttime":"function(stationstarttime) is.na(stationstarttime)","NewValue":"12:00:00.000Z"}]`

## Usage

``` r
SetTimeBiotic(BioticData, Time = character(), Overwrite = F)
```

## Arguments

- BioticData:

  [`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) data
  for which time should be set

- Time:

  character encoding time. Defaults to 12:00:00Z if not given, otherwise
  provide UTC-time formatted as
  [`strptime`](https://rdrr.io/r/base/strptime.html)-string:
  `%H:%M:%SZ, e.g. 12:00:00Z`

- Overwrite:

  if True any existing values in stationstarttime will be overwritten.

## Value

[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html)

## Details

Set start time to a fixed time for all stations. Appropriate when
BioticData is read from NMDbiotic.

Set the column 'stationstarttime' on the table 'fishstation' in
[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) to a
fixed time.

Setting a fixed time to stationstarttime facilitates conversion to
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
using [`StoxBiotic`](https://rdrr.io/pkg/RstoxData/man/StoxBiotic.html),
and is applicable if hourly resolution of date and time is not necessary
for subsequent analysis.

'stationstarttime' is on the table 'fishstation' in data originating
from NMDBiotic (http://www.imr.no/formats/nmdbiotic/). For bioticdata
that does not conform to this, no modifications are done.

## See also

[`StoxBiotic`](https://rdrr.io/pkg/RstoxData/man/StoxBiotic.html) For
converting
[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) to
[`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html).
