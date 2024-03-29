#' ICES areas (mixed area codes from 2018)
#'
#' Definition for FAO Northeast Atlantic (Major Fishing Area 27) area coding system used in ICES data calls,
#' as they have been defined from 2018 inclusive.
#' Polygons are defined on the finest available level of aggregation, either Subarea, Division, Subdivision or Unit.
#'
#' In addition to columns identifying the polygons in FAO nomenclature, a column with the area in square kilometers is included.
#' Polygons are derived from shapefiles provided by ICES web-portals, and has been edited with some simplifications.
#' Notably detailed coast-lines have been removed, in favor of drawing area borders on land-mass.
#'
#' Polygons are defined in WGS84 coordinates (unprojected).
#' 
#' The data contains the following columns with rows for each area:
#' \describe{
#'  \item{Area_km2}{Area of ICES area in squared kilometers}
#'  \item{Area_Full}{Code for full ICES area, including the FAO area code (27).}
#'  \item{Area_27}{Code for full ICES area, excluding the FAO area code (27).}
#' }
#' 
#' The data also contains columns for the individual components of the full ICES area code:
#' <Major_FA>.<SubArea>.<Division>.<SubDivision>.<Unit>
#'
#' @docType data
#'
#' @usage data(ICESareas)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#' 
#' @concept area code polygons
#' @keywords datasets
#' 
#' @seealso \code{\link[RstoxFDA]{ICESsubArea}}, \code{\link[RstoxFDA]{ICESdivision}}, \code{\link[RstoxFDA]{ICESsubDivision}}, and \code{\link[RstoxFDA]{ICESunit}}
#' for polygon files separating the different levels of ICES areas.
#'
#' @examples
#'  # plot ICES areas
#'  data(ICESareas)
#'  plotArea(areaDef=ICESareas)
"ICESareas"
