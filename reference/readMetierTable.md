# Read metier table

Reads a table of metier definitions.

## Usage

``` r
readMetierTable(filename, encoding = "UTF8")
```

## Arguments

- filename:

  character() path to file that contains metier definitions. See details
  for format.

- encoding:

  The character encoding of the file identified by 'filename'

## Value

[`MetierTable`](MetierTable.md) containing metier definitions.

## Details

The file identified by 'filename' must be a tab-separated file, and must
provided headers which must match column names in
[`MetierTable`](MetierTable.md)

gearnotation: The file format allows a shorthand notation for metiers
that are defined the same way for different unmeshed gearcodes or
targets. These may be written on one line, with the different gear codes
separated by commas or different targets separated by commas.

Optional columns may be omitted. They will be interpreted as NA.
Comments may be provided on lines with a leading '#'. Logical values
('meshedGear' and 'meshedSelectivityDevice') should be encoded with 'T'
for true and 'F' for false.
