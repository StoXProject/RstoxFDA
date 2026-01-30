# Define Age Error Matrix

Defines probabilities for misreading ages.

## Usage

``` r
DefineAgeErrorMatrix(
  processData,
  DefinitionMethod = c("ResourceFile"),
  FileName = character(),
  UseProcessData = F
)
```

## Arguments

- processData:

  [`AgeErrorMatrix`](AgeErrorMatrix.md) as returned from this function

- DefinitionMethod:

  'ResourceFile'. See details.

- FileName:

  path to resource file

- UseProcessData:

  If TRUE, bypasses execution of function and returns existing
  'processData'

## Value

Age Error Matrix, see: [`AgeErrorMatrix`](AgeErrorMatrix.md).

## Details

Defines probabilities for misreading ages, that is assumed probabilites
for determining a fish to be of age 'x' given that it actually is age
'y'.

Definitions are read from a tab separated file with headers and row
names in first column. All row and column names should be integers
representing ages. The matrix encodes the probability of observing an
age (rows), given true age (columns), and columns must sum to 1.

## See also

[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of age-error
matrices in Reca-estimation
