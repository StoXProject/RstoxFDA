# Reads landings archive

Reads aggregated sales notes from archive format deliver by FDIR to IMR.
E.g. sluttseddel_1978_2004_medVerdi.csv

## Usage

``` r
readFdirLandingsArchive(filename, encoding = "Latin-1")
```

## Arguments

- filename:

  file to read the archive from

- encoding:

  encoding of the file identified by filename, must be accepted by
  [`fread`](https://rdrr.io/pkg/data.table/man/fread.html).

## Value

[`LandingsArchiveData`](LandingsArchiveData.md)
