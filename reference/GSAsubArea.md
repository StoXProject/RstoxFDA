# GFCM sub areas (mixed area codes)

General Fisheries Commission for the Mediterranean - GFCM area coding
system (FAO Major Fishing Area 37) Polygons are defined on for
Divisions, and the Geographic Subareas are annotated in the column
F_SUBAREA.

## Usage

``` r
data(GSAsubArea)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Details

Polygons are defined in WGS84 coordinates (unprojected).

The data contains the following columns with rows for each area:

- F_AREA:

  FAO Major fishing Area (37)

- F_SUBAREA:

  The GSAs (Geographical subareas)

- F_DIVISION:

  The GFCM divisions, subdividing the GSAs.

## Examples

``` r
 # plot divisions
 data(GSAsubArea)
 plotArea(areaDef=GSAsubArea)

 
 # plot subareas
 RstoxFDA::plotArea(areaDef=GSAsubArea, areaNameCol = "F_SUBAREA")
```
