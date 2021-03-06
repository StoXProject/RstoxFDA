#' ICES rectangles
#'
#' Definitions for ICES rectangles.
#' ICES rectangles are defined by grids made up from integer longitudes, integer latitudes, and a bisection between each pair of latitudes.
#' They derive their name from being rectangular in mercator projection.
#'
#' The data contains the following columns with rows for each rectange:
#' \describe{
#'  \item{ICESNAME}{name of the rectangle in standard ICES notation}
#'  \item{SOUTH}{southern border of rectangle (latitude)}
#'  \item{WEST}{western border of rectangle (longitude)}
#'  \item{NORTH}{northern border of rectangle (latitdue)}
#'  \item{EAST}{eastern border of rectangle (longitude)}
#'  \item{AREA_KM2}{area of rectangle in square kilometers}
#'  \item{Ecoregion}{The main ecoregion the rectangle covers. May cover more at borders between ecoregions.}
#' }
#'
#' Polygons are derived from shapefiles provided by ICES web-portals.
#'
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' @docType data
#'
#' @usage data(ICESrectangles)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with rectange names identified in the column 'polygonName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @keywords datasets
#'
#' @examples
#'  # plot ICES areas alongside main areas
#'  data(ICESrectangles)
#'  sp::plot(ICESrectangles)
"ICESrectangles"
