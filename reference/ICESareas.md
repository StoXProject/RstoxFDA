# ICES areas (mixed area codes from 2018)

Definition for FAO Northeast Atlantic (Major Fishing Area 27) area
coding system used in ICES data calls, as they have been defined from
2018 inclusive. Polygons are defined on the finest available level of
aggregation, either Subarea, Division, Subdivision or Unit.

## Usage

``` r
data(ICESareas)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Details

In addition to columns identifying the polygons in FAO nomenclature, a
column with the area in square kilometers is included. Polygons are
derived from shapefiles provided by ICES web-portals, and has been
edited with some simplifications. Notably detailed coast-lines have been
removed, in favor of drawing area borders on land-mass.

Polygons are defined in WGS84 coordinates (unprojected).

The data contains the following columns with rows for each area:

- Area_km2:

  Area of ICES area in squared kilometers

- Area_Full:

  Code for full ICES area, including the FAO area code (27).

- Area_27:

  Code for full ICES area, excluding the FAO area code (27).

The data also contains columns for the individual components of the full
ICES area code:
\<Major_FA\>.\<SubArea\>.\<Division\>.\<SubDivision\>.\<Unit\>

## See also

[`ICESsubArea`](ICESsubArea.md), [`ICESdivision`](ICESdivision.md),
[`ICESsubDivision`](ICESsubDivision.md), and [`ICESunit`](ICESunit.md)
for polygon files separating the different levels of ICES areas.

## Examples

``` r
 # plot ICES areas
 data(ICESareas)
 plotArea(areaDef=ICESareas)
```
