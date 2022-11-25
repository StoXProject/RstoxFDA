#' Data from Norwegian port sampling program.
#'
#' Example of data formatted as \code{\link[RstoxData]{StoxBioticData}}, with some
#' additional columns added with \code{\link[RstoxFDA]{AddGearGroupStoxBiotic}}, \code{\link[RstoxFDA]{AddStratumStoxBiotic}}, \code{\link[RstoxFDA]{AddPeriodStoxBiotic}}, and \code{\link[RstoxData]{AddToStoxBiotic}}.
#' The data contain saithe samples from norwegian port-sampling in 2021.
#'
#' @docType data
#'
#' @usage data(StoxBioticDataExample)
#'
#' @format \code{\link[RstoxData]{StoxBioticData}}
#'
#' @keywords datasets
#' @concept StoX-Reca functions
#'
#' @examples
#'  RstoxFDA::plotArea(RstoxFDA::StoxBioticDataExample$Station, 
#'       areaDef=RstoxFDA::mainareaFdir2018, 
#'       latCol = "Latitude", 
#'       lonCol = "Longitude", 
#'       areaLabels = TRUE)
"StoxBioticDataExample"

