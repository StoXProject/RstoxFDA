# NAFO areas

Definition for FAO Northwest Atlantic (Major Fishing Area 21) area
coding system. NAFO areas as used in Norwegian fisheries reporting. A
coding convention for reporting these areas as an extention of the main
areas defined by the Norwegian Directorate of Fisheries identifies the
areas in the columns 'StratumName' and 'homr'. International convetion
is identified in the column 'nafo_names'.

## Usage

``` r
data(NAFOareas)
```

## Format

[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with area names
identified in the column 'StratumName'. See
[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
(v.2).

## Details

Polygons are defined for all Divisions of Major Fishing Area 21, except
Subarea 21.0, Subarea 21.5 and subarea 21.6. A single polygon is defined
for Subarea 21.0 and 21.5, and two for subarea 21.6. For subarea 21.6,
one polygon comprise Division 21.6.A-C, and one 21.6.D-H

For Division 21.3P and 21.4V a polygon is defined for each subdivision.

Polygons are defined in WGS84 coordinates (unprojected).

## Examples

``` r
 # combine NAFO and mainarea and plot
 library(sf) #use sfs rbind
#> Linking to GEOS 3.13.0, GDAL 3.8.5, PROJ 9.5.1; sf_use_s2() is TRUE
 combo <- rbind(RstoxFDA::NAFOareas[,"StratumName"], 
    RstoxFDA::mainareaFdir2018[,"StratumName"])
 RstoxFDA::plotArea(areaDef = combo)


 # conversion table Norwegian and international convention
 sf::st_drop_geometry(RstoxFDA::NAFOareas[,c("homr", "nafo_names")])
#>    homr nafo_names
#> 1    64        0AB
#> 2    65         1A
#> 3    66         1B
#> 4    67         1C
#> 5    68         1D
#> 6    69         1E
#> 7    75         1F
#> 8    86         4R
#> 9    76         2G
#> 10   77         2H
#> 11   78         2J
#> 12   79         3K
#> 13   80         3L
#> 14   81         3M
#> 15   82         3N
#> 16   83         3O
#> 17   84        3Pn
#> 18   85        3Ps
#> 19   87         4S
#> 20   88         4T
#> 21   89        4Vn
#> 22   90        4Vs
#> 23   91         4W
#> 24   94       6ABC
#> 25   95     6DEFGH
#> 26   92         4X
#> 27   93        5YZ
```
