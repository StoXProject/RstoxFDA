# Catch at Age plot

Plots estimated total catch at age for each age group. Constructs equal
tailed credibility intervals (The probability mass above and below the
interval is approximatelty the same).

## Usage

``` r
plotCatchAtAge(
  prediction,
  unit = "millions",
  plusGroup = NULL,
  credibility = 0.95,
  title = NULL
)
```

## Arguments

- prediction:

  as returned by `eca.predict` or [`runRECA`](runRECA.md).

- unit:

  unit of reported estimates. See details.

- plusGroup:

  Fish this age or older will be grouped in plot

- credibility:

  The desired credibility for credibility intervals.

- title:

  Title for plot

## Details

parameter 'unit' supports:

- number:

  Catch at age as number of fish

- thousands:

  Catch at age as number of fish in thousands

- millions:

  Catch at age as number of fish in millions

- kg:

  Catch at age as mass in kilogrammes

- T:

  Catch at age as mass in tons

- kT:

  Catch at age as mass in kilotonnes

## Examples

``` r
 data(recaPrediction)
 plotCatchAtAge(recaPrediction)
```
