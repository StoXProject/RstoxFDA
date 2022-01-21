#' ICES areas
#'
#' Definition ICES Divisions as they have been defined from 2018 inclusive.
#' 
#' Polygons are derived from shapefiles provided by ICES web-portals, and has been edited with some simplifications.
#' Notably detailed coast-lines have been removed, in favor of drawing area borders on land-mass.
#'
#' Polygons are defined in WGS84 coordinates (unprojected).
#' 
#'
#' @docType data
#'
#' @usage data(ICESdivision)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @keywords datasets
#'
#' @examples
#'  # plot ICES areas
#'  data(ICESdivision)
#'  plotArea(areaDef=ICESdivision)
"ICESdivision"
