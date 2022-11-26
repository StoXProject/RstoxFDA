#' Saithe landings 2021.
#'
#' Example of data formatted as \code{\link[RstoxFDA]{RecaCatchAtAgeExample}}.
#' Data is obtained by running Reca with the data in \code{\link[RstoxFDA]{StoxBioticDataExample}}, and \code{\link[RstoxFDA]{StoxLandingDataExample}}.
#' In order to control the size of the example data, Reca was configured with a rather low length resolution of 5 cm.
#'
#' @docType data
#'
#' @usage data(RecaCatchAtAgeExample)
#'
#' @format \code{\link[RstoxFDA]{RecaCatchAtAge}}
#'
#' @keywords datasets
#' @concept StoX-Reca functions
#'
#' @examples
#'  RstoxFDA::ReportRecaCatchAtAge(RstoxFDA::RecaCatchAtAgeExample, PlusGroup = 13)
"RecaCatchAtAgeExample"

