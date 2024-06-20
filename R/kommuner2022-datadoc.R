#' Norwegian municipalities (kommune)
#'
#' Definitions of Norwegian municipalities (kommune), current in 2022.
#' providing their borders, their official name and their official code.
#' 
#' Norwegian landings data often list landing sites by identifiers for municipalities (kommunenummer).
#' This resource can be used to infer location code that are associated with polygons or coordinates,
#' such as UN/LOCODES.
#' 
#' The Norwegian municipalities have been subject to revision, and other resources may have to be obtained
#' to completely handle data from other years.
#'
#' @docType data
#'
#' @usage data(kommuner2022)
#'
#' @format \code{\link[sf]{sf}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}} (v.2).
#'
#' @concept area code polygons
#' @keywords datasets
#'
#' @examples
#' # map kommune to locodes
#' data(kommuner2022)
#' data(portcodes2020)
#' nocodes <- portcodes2020[portcodes2020$Country=="NO" & !is.na(portcodes2020$latitude),]
#' locodemap <- RstoxFDA::appendAreaCode(nocodes, kommuner2022, 
#'         colName="kommune", 
#'         latName = "latitude", 
#'         lonName = "longitude")[,c("kommune", "Location")]
#' # Note that kommune does not uniquely identify locode:
#' locodemap[locodemap$kommune==4601,]
"kommuner2022"
