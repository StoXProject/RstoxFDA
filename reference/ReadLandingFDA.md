# Read landing files

This function reads multiple landing files (sales-notes) to a list with
a list of tables for each file. It extends upon
[`ReadLanding`](https://rdrr.io/pkg/RstoxData/man/ReadLanding.html) in
that it supports additional input formats.

## Usage

``` r
ReadLandingFDA(
  FileNames,
  Format = c("landingerv2", "lss", "FDIR.2021"),
  FileEncoding = c("Default", "UTF-8", "Latin-1"),
  ForceUnique = FALSE
)
```

## Arguments

- FileNames:

  The paths of the landing files.

- Format:

  The file format of the landing files.

- FileEncoding:

  encoding for the files that should be read. If not given the default
  encoding for each format is used.

- ForceUnique:

  Manipulate the field 'Linjenummer' with arbitrary changes to ensure
  that key columns uniquely identify rows.

## Details

Norwegian sales-notes data are archived and curated by the Norwegian
Directorate of Fisheries (FDIR). Data is made public or is transferred
to the Institute of Marine Research (IMR) in various formats. Some of
these formats are also archived by the Norwegian Marine Datacenter (NMD)
at IMR.

Some of the supported formats are missing columns supported by
[`LandingData`](https://rdrr.io/pkg/RstoxData/man/LandingData.html),
these columns are set to NA. Likewise some formats have additional
columns, not supported by
[`LandingData`](https://rdrr.io/pkg/RstoxData/man/LandingData.html),
these are ignored.

Occasionally landing sets contain data that where rows are not uniquely
identified by the key columns in that format. In these cases a warning
is issued, and it is important to handle those duplicates to avoid
problems in later processing. Uniqueness of keys are checked for in some
typical downstream StoX processes, such as
[`StoxLanding`](https://rdrr.io/pkg/RstoxData/man/StoxLanding.html), so
the problem may potentially disappear after filtering. Otherwise, the
parameter 'ForceUnique' may be considered, if one is confident these
records does in fact represent separate landings.

Formats may be one of the following

- landingerv2:

  XML-files given in the namespace
  http://www.imr.no/formats/landinger/v2. Default encoding: UTF-8
  (enforced by format)

- lss:

  Lss format, used for official deliveries from Fdir to IMR (2005-).
  Default encoding: Latin-1 (iso-8859-1)

- FDIR.2021:

  Sales notes from FDIRs open data sets, as formatted in their 2021
  release. Default encoding: UTF-8.

Files in the format 'landingerv2' and 'lss' may be obtained from the NMD
landings API at IMR.

The lss format has been using various naming conventions. Data is read
by column index, and strict checking of column names is not performed.
