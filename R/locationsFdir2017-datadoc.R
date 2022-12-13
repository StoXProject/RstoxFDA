#' Location codes (FDIR to 2017 incl.)
#'
#' Definition for location coding system defined by the Norwegian directorate of Fisheries up until 2017 (inclusive).
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' @docType data
#'
#' @usage data(locationsFdir2017)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with location names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @concept area code polygons
#' @keywords datasets
#'
#' @examples
#'  # compare locations and ICES rectangles in Barents Sea
#'  RstoxFDA::plotAreaComparison(
#'     RstoxFDA::ICESrectangles, 
#'     RstoxFDA::locationsFdir2017, 
#'     xlim=c(20,30), 
#'     ylim=c(70,75), 
#'     linetype2 = "dotted", 
#'     polygonColor1 = "green", 
#'     areaLabels2 = TRUE)
"locationsFdir2017"
