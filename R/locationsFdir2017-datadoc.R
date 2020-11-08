#' Main areas
#'
#' Definition for location coding system defined by the Norwegian directorate of Fisheries up until 2017 (inclusive).
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' @docType data
#'
#' @usage data(locationsFdir2017)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with location names identified in the column 'polygonName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @keywords datasets
#'
#' @examples
#' data(locationsFdir2017)
#' data(locationsFdir2018)
#' sp::plot(locationsFdir2018, ylim=c(57,60), xlim=c(0,25), border="blue")
#' sp::plot(locationsFdir2017, add=TRUE)
"locationsFdir2017"
