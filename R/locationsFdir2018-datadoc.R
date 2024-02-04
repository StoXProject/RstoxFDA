#' Location codes (FDIR from 2018 incl.)
#'
#' Definition for location coding system defined by the Norwegian directorate of Fisheries  (2018 revision).
#' This revision has been in use for their fishery statistics as of 2018 (inclusive).
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' @docType data
#'
#' @usage data(locationsFdir2018)
#'
#' @format \code{\link[sf]{sf}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}} (v.2).
#'
#' @concept area code polygons
#' @keywords datasets
#'
#' @examples
#'  RstoxFDA::plotAreaComparison(RstoxFDA::mainareaFdir2017, 
#'      RstoxFDA::mainareaFdir2018, 
#'      xlim=c(0,20), ylim=c(50,60))
"locationsFdir2018"
