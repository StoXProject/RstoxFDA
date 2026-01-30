# Read PSU Sampling Design Parameters

Read sampling parameters for Primary Sampling Units in multi-stage
sampling.

## Usage

``` r
ReadPSUSamplingParameters(FileName)
```

## Arguments

- FileName:

  path to sampling parameters

## Value

[`PSUSamplingParametersData`](PSUSamplingParametersData.md)

## Details

Reads sampling parameters from a tab delimited file with headers
corresponding to those listed in
[`PSUSamplingParametersData`](PSUSamplingParametersData.md). The file
format provide the data as one table, so that the information in
'sampleTable' is repeated for each entry in 'selectionTable'. Any
columns not named in
[`PSUSamplingParametersData`](PSUSamplingParametersData.md) are assumed
to be stratification variables. The conditions listed for the variables
in [`PSUSamplingParametersData`](PSUSamplingParametersData.md) are
checked upon reading the data, and execution halts with error if any are
violated. Consult the examples in this documentation to see how the
resource is formatted with a stratification variable 'Species'.

## See also

[`ComputePSUSamplingParameters`](ComputePSUSamplingParameters.md),
[`AddPsuStratificationVariables`](AddPsuStratificationVariables.md),
[`AssignPSUSamplingParameters`](AssignPSUSamplingParameters.md),
[`AnalyticalPopulationEstimate`](AnalyticalPopulationEstimate.md)

## Examples

``` r
 # embedded example file:
 exampleFile <- system.file("testresources", 
                        "lotteryParameters", 
                        "lotteryDesignNSHstrata.txt", package="RstoxFDA")
 
 # Read example file with StoX
 PSUSamplingParametersData <- RstoxFDA::ReadPSUSamplingParameters( 
                          FileName=exampleFile)
 
 # Read example file as flat table, to illustrate formatting
 FlatSamplingParametersData <- read.csv(exampleFile, sep="\t")
```
