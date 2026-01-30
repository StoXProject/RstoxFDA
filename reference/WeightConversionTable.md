# Weight Conversion Table (WeightConversionTable)

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
factors for approximating the weight of a desired product type (e.g.
round fish) from weights of other fish products. Contains the columns:

- 'Description':

  Free-text description of the product type

- 'Species':

  Identifier for the species that the conversion applies to

- 'ProductType':

  Identifier for the type of product that the conversion applies to

- 'WeightFactor':

  scalar value that weights for the given 'ProductType' can be
  multiplied with to approximate desired product type (e.g. round fish).

NA is allowed for 'WeightFactor', which will result in NA for weights
after conversion
