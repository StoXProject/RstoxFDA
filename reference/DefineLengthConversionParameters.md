# Define Length Conversion Parameters

Define length conversion parameters for approximating one type of length
measurement from measurments of another type, based on a linear
regression fit between measurement types. E.g. calculating 'total
length' from measured 'fork length'.

## Usage

``` r
DefineLengthConversionParameters(
  processData,
  DefinitionMethod = c("ResourceFile"),
  FileName = character(),
  UseProcessData = F
)
```

## Arguments

- processData:

  [`LengthConversionTable`](LengthConversionTable.md) as returned from
  this function

- DefinitionMethod:

  'ResourceFile'. See details.

- FileName:

  path to resource file

- UseProcessData:

  If TRUE, bypasses execution of function and returns existing
  'processData'

## Value

[`LengthConversionTable`](LengthConversionTable.md)

## Details

For DefinitionMethod 'ResourceFile': Definitions are read from a tab
separated, UTF-8 encoded file with headers. Columns defined as:

- Column 1: 'Description':

  Free-text description of the product

- Column 2: 'Species':

  Identifier for the species that the conversion applies to

- Column 3: 'MeasurmentType':

  Identifier for the type of length measurement of the measured length

- Column 4: 'Alpha':

  scalar value representing the intercept (in cm) of a linear regression
  fit between length measurements.

- Column 5: 'Beta':

  scalar value representing the slope of a linear regression fit between
  length measurements.

See also [`LengthConversionTable`](LengthConversionTable.md)

## See also

[`ConvertLengthBiotic`](ConvertLengthBiotic.md) for applying length
conversion to data
