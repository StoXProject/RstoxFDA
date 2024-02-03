#' ICES Divisions (area codes from 2018)
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
#' @format \code{\link[sf]{sf}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}} (v.2).
#'
#' @concept area code polygons
#' @keywords datasets
#'
#' @examples
#'  # plot ICES areas
#'  data(ICESdivision)
#'  plotArea(areaDef=ICESdivision)
"ICESdivision"
