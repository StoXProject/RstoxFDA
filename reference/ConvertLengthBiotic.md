# Convert lengths Biotic

Convert lengths to approximate values for a desired length measurement.
Typically 'total length'.

## Usage

``` r
ConvertLengthBiotic(
  BioticData,
  LengthConversionTable,
  TargetLengthMeasurement = character()
)
```

## Arguments

- BioticData:

  [`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) for
  which lengths should be converted

- LengthConversionTable:

  [`LengthConversionTable`](LengthConversionTable.md) with parameters
  for converting lengths to 'TargetLengthMeasurement'.

- TargetLengthMeasurement:

  The desired length measurement. Typically the code for 'total length'.

## Value

[`BioticData`](https://rdrr.io/pkg/RstoxData/man/BioticData.html) with
converted lengths.

## Details

Different length measurements may be defined for fish, such as 'total
length', 'standard length' or 'fork length'. For partially processed
fish, only some measurements may be available, such as 'head-length'.
When desired length measurment is not measured, 'total length' or some
other length measurement may be approximated by a regression model.

For records where the variable 'catchcategory' on the table
'catchsample' matches the 'Species' variable in 'LengthConversionTable'
this function converts lengths ('length' on the 'Individual'-table) to
desired measurement type ('TargetLengthMeasurement') by matching the
'lengthmeasurement' variable on the corresponding 'catchsample'-table to
'MeasurementType' in 'LengthConversionTable' and applying the formula
L_desired = 0.01 \\ Alpha + Beta \\ L_MeasurementType. The factor of
0.01 is applied because lengths in Biotic is defined i m, while 'Alpha'
in 'LengthConversionTable' is defined in cm.

After conversion the 'lengthmeasurement'-variable will be changed to
reflect the converted length. That is, they will be set to
'TargetLengthMeasurement'.

These variables are on these tables in data originating from NMDBiotic
(http://www.imr.no/formats/nmdbiotic/). For bioticdata that does not
conform to this, no modifications are done.

## See also

[`DefineLengthConversionParameters`](DefineLengthConversionParameters.md)
for configuration of length parameters.
