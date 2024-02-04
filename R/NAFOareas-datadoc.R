#' NAFO areas
#'
#' Definition for FAO Northwest Atlantic (Major Fishing Area 21) area coding system. NAFO areas as used in Norwegian fisheries reporting.
#' A coding convention for reporting these areas as an extention of the main areas defined by the Norwegian Directorate of Fisheries
#' identifies the areas in the columns 'StratumName' and 'homr'. International convetion is identified in the column 'nafo_names'.
#'
#' Polygons are defined for all Divisions of Major Fishing Area 21, except Subarea 21.0, Subarea 21.5 and subarea 21.6.
#' A single polygon is defined for Subarea 21.0 and 21.5, and two for subarea 21.6.
#' For subarea 21.6, one polygon comprise Division 21.6.A-C, and one 21.6.D-H
#'
#' For Division 21.3P and 21.4V a polygon is defined for each subdivision.
#'
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' @docType data
#'
#' @usage data(NAFOareas)
#'
#' @format \code{\link[sf]{sf}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}} (v.2).
#'
#' @concept area code polygons
#' @keywords datasets
#'
#' @examples
#'  # combine NAFO and mainarea and plot
#'  combo <- rbind(RstoxFDA::NAFOareas[,"StratumName"], 
#'     RstoxFDA::mainareaFdir2018[,"StratumName"])
#'  RstoxFDA::plotArea(areaDef = combo)
#'
#'  # conversion table Norwegian and international convention
#'  sf::st_drop_geometry(RstoxFDA::NAFOareas[,c("homr", "nafo_names")])
"NAFOareas"
