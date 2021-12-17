#' ICES areas
#'
#' Definition for FAO Northeast Atlantic (Major Fishing Area 27) area coding system used in ICES data calls.
#' Polygons are defined on different levels of aggregation, either Subarea, Division, Subdivision or Unit.
#'
#' In addtion to columns idenitfying the polygons in FAO nomenclature, a column with the area in square kilometers is included.
#' Polygons are derived from shapefiles provided by ICES web-portals.
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
#' The data also contains columns for the individul components of the full ICES area code:
#' <Major_FA>.<SubArea>.<Division>.<SubDivision>.<Unit>
#'
#' @docType data
#'
#' @usage data(ICESareas)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @keywords datasets
#'
#' @examples
#'  # plot ICES areas
#'  data(ICESareas)
#'  sp::plot(ICESareas)
"ICESareas"
