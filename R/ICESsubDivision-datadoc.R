#' ICES sub-divisions (area codes from 2018)
#'
#' Definition ICES SubDivisions as they have been defined from 2018 inclusive.
#' 
#' Polygons are derived from shapefiles provided by ICES web-portals, and has been edited with some simplifications.
#' Notably detailed coast-lines have been removed, in favor of drawing area borders on land-mass.
#'
#' Polygons are defined in WGS84 coordinates (unprojected).
#' 
#'
#' @docType data
#'
#' @usage data(ICESsubDivision)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @concept area code polygons
#' @keywords datasets
#'
#' @examples
#'  # plot ICES areas
#'  data(ICESsubDivision)
#'  plotArea(areaDef=ICESsubDivision)
"ICESsubDivision"
