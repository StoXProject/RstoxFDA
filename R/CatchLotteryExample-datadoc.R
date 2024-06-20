#' Data from the Norwegian catch lottery sampling program.
#'
#' Example of data formatted as \code{\link[RstoxData]{StoxBioticData}}, with an added column 'serialnumber' at the Haul-table and an added column 'CountryVessel' at the Station-table
#' which identifies the selection in the catch sampling lottery that this data was recorded for (see 'SamplingUnitId' in \code{\link[RstoxFDA]{CatchLotterySamplingExample}})
#' 
#' Hauls are primary sampling units, selected by Poission sampling with selection probabilities proportional to the catch size.
#' Corresponding sampling parameters are provided in \code{\link[RstoxFDA]{CatchLotterySamplingExample}}
#' The data contain North Sea herring samples from catch lottery sampling in 2022.
#'
#' @docType data
#'
#' @usage data(CatchLotteryExample)
#'
#' @format \code{\link[RstoxData]{StoxBioticData}}
#'
#' @keywords datasets
#' @concept Analytical estimation
#'
#' @examples
#'  RstoxFDA::plotArea(RstoxFDA::CatchLotteryExample$Station, 
#'       areaDef=RstoxFDA::mainareaFdir2018, 
#'       latCol = "Latitude", 
#'       lonCol = "Longitude", 
#'       areaLabels = TRUE)
"CatchLotteryExample"

