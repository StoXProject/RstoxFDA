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
