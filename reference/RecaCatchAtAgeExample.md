# Saithe landings 2021.

Example of data formatted as `RecaCatchAtAgeExample`. Data is obtained
by running Reca with the data in
[`StoxBioticDataExample`](StoxBioticDataExample.md), and
[`StoxLandingDataExample`](StoxLandingDataExample.md). In order to
control the size of the example data, Reca was configured with a rather
low length resolution of 5 cm.

## Usage

``` r
data(RecaCatchAtAgeExample)
```

## Format

[`RecaCatchAtAge`](RecaCatchAtAge.md)

## Examples

``` r
 RstoxFDA::ReportRecaCatchAtAge(RstoxFDA::RecaCatchAtAgeExample, PlusGroup = 13)
#> $NbyAge
#>     AgeGroup   Age CatchAtAge     SD     Low    High
#>       <char> <num>      <num>  <num>   <num>   <num>
#>  1:    Age 1     1      21492  31754     404   95927
#>  2:    Age 2     2      25084  40541      52   70750
#>  3:    Age 3     3     238764 110070  104672  422917
#>  4:    Age 4     4     987229 234699  653819 1429625
#>  5:    Age 5     5    6254131 786326 5115794 7344917
#>  6:    Age 6     6    3784386 561252 2921262 4766634
#>  7:    Age 7     7    3638165 487433 2904993 4491507
#>  8:    Age 8     8    4442624 523941 3664388 5323354
#>  9:    Age 9     9    1125128 235577  823717 1488910
#> 10:   Age 10    10     547490 148911  345107  826186
#> 11:   Age 11    11     385556 110394  249463  610248
#> 12:   Age 12    12     495272 123316  320967  695681
#> 13:  Age 13+    13     647093 151804  447239  914943
#> 
#> $GroupingVariables
#> Empty data.table (0 rows and 1 cols): GroupingVariables
#> 
```
