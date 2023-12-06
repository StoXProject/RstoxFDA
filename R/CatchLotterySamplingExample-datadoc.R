#' Sampling parameters from the Norwegian catch lottery sampling program.
#'
#' Example of data formatted as \code{\link[RstoxFDA]{PSUSamplingParametersData}}
#' Hauls are primary sampling units, selected by Poission sampling with selection probabilities proportional to the catch size.
#' The data contain sampling parameters for North Sea herring samples from catch lottery sampling in 2022.
#' 
#' The corresponding samples are provided in \code{\link[RstoxFDA]{CatchLotteryExample}}
#'
#' @docType data
#'
#' @usage data(CatchLotterySamplingExample)
#'
#' @format \code{\link[RstoxData]{StoxBioticData}}
#'
#' @keywords datasets
#' @concept Analytical estimation
#'
#' @examples
#'  #all selected PSU that where actuall sampled are provided in CatchLotteryExample
#'  sum(!is.na(CatchLotterySamplingExample$SelectionTable$SamplingUnitId))
#'  sum(CatchLotterySamplingExample$SelectionTable$SamplingUnitId %in% CatchLotteryExample$Haul$HaulKey)
"CatchLotterySamplingExample"

