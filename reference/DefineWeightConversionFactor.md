# Define Weight Conversion Factors

Define weight conversion factors for calculating approximate weights for
a product type (e.g. round fish) from the weight of other fish products,
such as gutted fish.

## Usage

``` r
DefineWeightConversionFactor(
  processData,
  DefinitionMethod = c("ResourceFile", "FDIR.VIII.2022"),
  FileName = character(),
  UseProcessData = F
)
```

## Arguments

- processData:

  [`WeightConversionTable`](WeightConversionTable.md) as returned from
  this function

- DefinitionMethod:

  'ResourceFile' or 'FDIR.VIII.2022'. See details.

- FileName:

  path to resource file

- UseProcessData:

  If TRUE, bypasses execution of function and returns existing
  'processData'

## Value

[`WeightConversionTable`](WeightConversionTable.md)

## Details

For DefinitionMethod 'ResourceFile': Definitions are read from a tab
separated, UTF-8 encoded file with headers. Columns defined as:

- Column 1: 'Description':

  Free-text description of the product

- Column 2: 'Species':

  Identifier for the species that the conversion applies to

- Column 3: 'ProductType':

  Identifier for the type of product that the conversion applies to

- Column 4: 'WeightFactor':

  scalar value that weights for the given 'ProductType' can be
  multiplied with to approximate the desired product type (w.g. round
  fish).

For DefinitionMethod 'FDIR.VIII.2022', the table
[`FDIR.factors.VIII.2022`](FDIR.factors.VIII.2022.md) will be used

Missing values for WeightFactor are interpreted as NA, and will result
in NA for weights after conversion.

## See also

[`ConvertWeightBiotic`](ConvertWeightBiotic.md) for applying weight
conversion to data.
