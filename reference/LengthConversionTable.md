# Length Conversion Table (LengthConversionTable)

Length conversion parameters realting different length measurements.

## Details

Length conversion factors relating different length measurements, such
as 'standard length' and 'fork length' based on a linear regression fit
between length measurements: L1 = alpha + beta \\ L2, where L1 and L2
are different length measurements and 'alpha' and 'beta' are
species-specific coefficients.

[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns:

- 'Description':

  character: Free-text description of the product

- 'Species':

  character: Identifier for the species that the conversion applies to

- 'MeasurmentType':

  character: Identifier for the type of length measurement for the
  independent variable (L2 above)

- 'Alpha':

  numeric: scalar value representing the intercept (in cm) of a linear
  regression fit between length measurements.

- 'Beta':

  numeric: scalar value representing the slope of a linear regression
  fit between length measurements.
