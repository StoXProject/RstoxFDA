# Parses logbooks (lst)

Parses logbooks from tabular format (.lst) delivered by Directorate of
Fisheries (FDIR). This format is not matched with WMS records and
contains less detail than the format read by
[`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html)

## Usage

``` r
readLstFile(filename, encoding = "Latin-1")
```

## Arguments

- filename:

  file to read the logbook records from

- encoding:

  encoding of the file identified by filename, must be accepted by
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html).

## Value

LstLogbookData

## Details

A variant of .lst does not provide headers. These are read with assumed
standard order of headers, and a warning is issued.
