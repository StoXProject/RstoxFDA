# Add length group to StoxBioticData

Adds a variable that groups individuals based on the variable
'IndividualTotalLength'. This allows length-groups to be defined as
domains in analytical estimation.

The groups are defined by the argument 'LengthInterval', which specify
consecutive length groups of equal length range ('LengthInterval'),
starting with length 0. The argument 'LeftOpen' specifies whether the
intervals are open to the left (lower value) or to the right (higher
value).

For example LengthInterval=5, and LeftOpen=FALSE, specifies
length-groups \[0,5\>,\[5,10\>,...

Note that even though the length-groups are formatted as a character,
they have a strict format that carry numerical information for
downstream functions, such as
[`ReportAnalyticalCatchAtLength`](ReportAnalyticalCatchAtLength.md)

## Usage

``` r
AddLengthGroupStoxBiotic(
  StoxBioticData,
  LengthInterval = numeric(),
  LengthGroupVariable = character(),
  LeftOpen = TRUE
)
```

## Arguments

- StoxBioticData:

  [`StoxBioticData`](https://rdrr.io/pkg/RstoxData/man/StoxBioticData.html)
  with individuals to be grouped by length

- LengthInterval:

  The 'bin-size', length in cm between length-groups

- LengthGroupVariable:

  Name to use for the length group variable

- LeftOpen:

  logical, specifying whether intervals are left-open, or right-open.

## See also

[`AnalyticalPSUEstimate`](AnalyticalPSUEstimate.md),
[`ReportAnalyticalCatchAtLength`](ReportAnalyticalCatchAtLength.md)

## Examples

``` r
 StoxBioticWLengthGroup <- RstoxFDA:::AddLengthGroupStoxBiotic(RstoxFDA::CatchLotteryExample, 
       LengthInterval=5, LengthGroupVariable="LengthGroup", LeftOpen=TRUE)
 table(StoxBioticWLengthGroup$Individual$LengthGroup)
#> 
#> (15,20] (20,25] (25,30] (30,35] 
#>      67    2387     604     273 
```
