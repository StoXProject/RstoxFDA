# Define Stock Splitting Parameters

Defines parameters for the stock-splitting analysis in Reca, including
parameters for misclassifying when determining stock membership of a
specimen.

## Usage

``` r
DefineStockSplittingParameters(
  processData,
  DefinitionMethod = c("ResourceFile", "FunctionParameters"),
  FileName = character(),
  StockNameCC = character(),
  StockNameS = character(),
  ProbabilityType1As1 = numeric(),
  ProbabilityType5As1 = numeric(),
  ProbabilityType2As2 = numeric(),
  ProbabilityType4As2 = numeric(),
  ProbabilityType2As4 = numeric(),
  ProbabilityType4As4 = numeric(),
  ProbabilityType1As5 = numeric(),
  ProbabilityType5As5 = numeric(),
  UseProcessData = F
)
```

## Arguments

- processData:

  data.table() as returned from this function

- DefinitionMethod:

  'ResourceFile' or 'FunctionParameters', see details.

- FileName:

  path to resource file

- StockNameCC:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- StockNameS:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType1As1:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType5As1:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType2As2:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType4As2:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType2As4:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType4As4:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType1As5:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- ProbabilityType5As5:

  Variable for [`StockSplittingParameters`](StockSplittingParameters.md)

- UseProcessData:

  If TRUE, bypasses execution of function and returns existing
  'processData'

## Value

[`StockSplittingParameters`](StockSplittingParameters.md).

## Details

For DefinitionMethod 'ResourceFile', definitions are read from a UTF-8
encoded tab separated file with headers and one row with headers
corresponding to column names in
[`StockSplittingParameters`](StockSplittingParameters.md). see
[`StockSplittingParameters`](StockSplittingParameters.md) for further
explanation on the coding system.

For DefinitionMethod 'FunctionParameters' defintions are constructed
from parameters to this function.

## See also

[`PrepareRecaEstimate`](PrepareRecaEstimate.md) for use of
stock-splitting parameters in Reca-estimation.
