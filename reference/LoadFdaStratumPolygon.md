# Load common area definitions

Loads standard area definitions often used in analysis of fisheries.

## Usage

``` r
LoadFdaStratumPolygon(
  processData,
  StrataSystem = c("FDIR.2017", "FDIR.2018", "ICES.2018", "ICES.SubArea.2018",
    "ICES.Division.2018", "ICES.SubDivision.2018", "ICES.Unit.2018",
    "ICES.Rectangles.2018", "NAFO", "NAFO.FDIR.2017", "NAFO.FDIR.2018"),
  UseProcessData = F
)
```

## Arguments

- processData:

  [`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
  as returned from this function

- StrataSystem:

  the strata system to load

- UseProcessData:

  If TRUE, bypasses execution of function and returns existing
  'processData'

## Value

[`StratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/StratumPolygon.html)
with the desired strata definition.

## Details

This function has the same effect as
[`DefineStratumPolygon`](https://rdrr.io/pkg/RstoxBase/man/DefineStratumPolygon.html),
but loads the polygons from the RstoxFDA package, rather than requiring
them as a resource file.

The options available are:

- "FDIR.2017":

  Main areas defined by the Norwegian Directorate of Fisheries, as they
  where defined to 2017 inclusive. Derived from
  [`mainareaFdir2017`](mainareaFdir2017.md).

- "FDIR.2018":

  Main areas defined by the Norwegian Directorate of Fisheries, as they
  have been defined from 2018 inclusive. Derived from
  [`mainareaFdir2018`](mainareaFdir2018.md).

- "ICES.2018":

  ICES areas as they have been defined from 2018 inclusive. Areas are
  provided with finest available spatial resolution (Sub-area, division,
  sub-division or unit), and the full naming convention is used (e.g.
  27.3.d.27 or 27.4.b). Derived from [`ICESareas`](ICESareas.md).

- "ICES.SubArea.2018":

  ICES areas as they have been defined from 2018 inclusive. Areas are
  provided as 'sub-areas' where they are defined, and the full naming
  convention is used (e.g. 27.3). Derived from
  [`ICESsubArea`](ICESsubArea.md).

- "ICES.Division.2018":

  ICES areas as they have been defined from 2018 inclusive. Areas are
  provided as 'divisions' where they are defined, and the full naming
  convention is used (e.g. 27.3.d). Derived from
  [`ICESdivision`](ICESdivision.md).

- "ICES.SubDivision.2018":

  ICES areas as they have been defined from 2018 inclusive. Areas are
  provided as 'sub-divisions' where they are defined, and the full
  naming convention is used (e.g. 27.3.d.27). Derived from
  [`ICESsubDivision`](ICESsubDivision.md).

- "ICES.Unit.2018":

  ICES areas as they have been defined from 2018 inclusive. Areas are
  provided as 'unit' where they are defined, and the full naming
  convention is used (e.g. 27.3.d.28.1). Derived from
  [`ICESunit`](ICESunit.md).

- "ICES.Rectangles.2018":

  ICES rectangles (e.g. 15C2) as they have been defined from 2018
  inclusive. Derived from [`ICESrectangles`](ICESrectangles.md).

- "NAFO":

  NAFO areas. NAFO naming convention is used. Derived from
  [`NAFOareas`](NAFOareas.md).

- "NAFO.FDIR.2017":

  NAFO areas combined with FDIR.2017. The naming convention of the
  Norwegian Directorate of fisheries is used.

- "NAFO.FDIR.2018":

  NAFO areas combined with FDIR.2018. The naming convention of the
  Norwegian Directorate of fisheries is used.

FDIR.2017 corresponds to the area coding referred to as system 2 at IMR.
FDIR.2018 corresponds to the area coding referred to as system 10 at
IMR.
