#' Main areas
#'
#' Definition for area coding system defined by the Norwegian directorate of Fisheries (2018 revision).
#' This revision has been in use for their fishery statistics as of 2018 (inclusive).
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' @docType data
#'
#' @usage data(mainareaFdir2018)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @keywords datasets
#'
#' @examples
#'  data(mainareaFdir2018)
#'  sp::plot(mainareaFdir2018)
"mainareaFdir2018"
