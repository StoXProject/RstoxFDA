#' UN Location codes for ports
#'
#' Table of United Nations Codes for Trade and Transport Locations (UN/LOCODE) that has a seaport.
#' Updated ca 2020.
#' 
#' There is a considerable lag between the national adaptation of new locodes and the updates to the UN/LOCODE lists.
#' The Norwegian institution that coordinates new needs for LOCODES is the Norwegian Coastal Administration (Kystverket).
#'
#' @docType data
#'
#' @usage data(portcodes2020)
#'
#' @format \code{\link[RstoxFDA]{LocodeTable}}.
#'
#' @family area code polygons
#' @keywords datasets
#'
#' @examples
#' data(portcodes2020)
#' nocodes <- portcodes2020[portcodes2020$Country=="NO" & !is.na(portcodes2020$latitude),]
#' RstoxFDA::plotArea(nocodes, "latitude", "longitude", areaDef=RstoxFDA::mainareaFdir2017)
"portcodes2020"
