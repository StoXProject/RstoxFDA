# Convert codes.

Apply conversion table, perform approriate checks and return result.

## Usage

``` r
convertCodes(code, conversionTable, strict = T)
```

## Arguments

- code:

  character() with original codes

- conversionTable:

  list() mapping code to converted code.

- strict:

  logical() If true, execution halts at incomplete conversion tables. If
  false unmapped codes are set to NA.

## Value

character() with converted codes

## Details

By default. This will stop with error if any codes can not be converted,
or if any entries are NA. Require all codes (original and converted) to
be character().

## Examples

``` r
 gearConversion <- list()
 gearConversion["TBS"] <- "OTB"
 gearConversion["TBN"] <- "OTB"
 gearConversion["OTB"] <- "OTB"
 convertCodes(c("TBS", "TBN", "OTB"), gearConversion)
#> [1] "OTB" "OTB" "OTB"
```
