#' Main areas (FDIR to 2017 incl.)
#'
#' Definition for area coding system defined by the Norwegian directorate of Fisheries up until 2017 (inclusive).
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' @docType data
#'
#' @usage data(mainareaFdir2017)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @concept area code polygons
#' @keywords datasets
#'
#' @examples
#'  RstoxFDA::plotAreaComparison(RstoxFDA::mainareaFdir2017, 
#'      RstoxFDA::mainareaFdir2018, 
#'      xlim=c(0,20), ylim=c(50,60))
"mainareaFdir2017"
