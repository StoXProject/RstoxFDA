#' GFCM sub areas (mixed area codes)
#'
#' General Fisheries Commission for the Mediterranean - GFCM area coding system (FAO Major Fishing Area 37) 
#' Polygons are defined on for Divisions, and the Geographic Subareas are annotated in the column F_SUBAREA.
#'
#' Polygons are defined in WGS84 coordinates (unprojected).
#' 
#' The data contains the following columns with rows for each area:
#' \describe{
#'  \item{F_AREA}{FAO Major fishing Area (37)}
#'  \item{F_SUBAREA}{The GSAs (Geographical subareas)}
#'  \item{F_DIVISION}{The GFCM divisions, subdividing the GSAs.}
#' }
#' 
#'
#' @docType data
#'
#' @usage data(GSAsubArea)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#' 
#' @concept area code polygons
#' @keywords datasets
#' 
#'
#' @examples
#'  # plot divisions
#'  data(GSAsubArea)
#'  plotArea(areaDef=GSAsubArea)
#'  
#'  # plot subareas
#'  RstoxFDA::plotArea(areaDef=GSAsubArea, areaNameCol = "F_SUBAREA")
"GSAsubArea"
