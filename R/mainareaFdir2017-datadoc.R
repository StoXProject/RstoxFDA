#' Main areas
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
#' @keywords datasets
#'
#' @examples
#' data(mainareaFdir2017)
#' data(mainareaFdir2018)
#' sp::plot(mainareaFdir2017)
#' sp::plot(mainareaFdir2018, border="blue", add=TRUE)
"mainareaFdir2017"
