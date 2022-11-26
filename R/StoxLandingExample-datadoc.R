#' Saithe landings 2021.
#'
#' Example of data formatted as \code{\link[RstoxData]{StoxLandingData}}, with some
#' additional columns added with \code{\link[RstoxFDA]{AddGearGroupStoxLanding}}, \code{\link[RstoxFDA]{AddStratumStoxLanding}}, and \code{\link[RstoxFDA]{AddAreaPositionStoxLanding}}, and \code{\link[RstoxFDA]{AddStratumStoxLanding}}, and \code{\link[RstoxFDA]{AddPeriodStoxLanding}}.
#' The data contain saithe landings by Norwegian vessels in areas (\code{\link[RstoxFDA]{mainareaFdir2018}}) "00", "03", "04", "05","06", in 2021.
#'
#' @docType data
#'
#' @usage data(StoxLandingDataExample)
#'
#' @format \code{\link[RstoxData]{StoxLandingData}}
#'
#' @keywords datasets
#' @concept StoX-Reca functions
#'
#' @examples
#'  RstoxFDA::plotBubbleMap(RstoxFDA::StoxLandingDataExample$Landing, 
#'       "Area", "RoundWeight", RstoxFDA::mainareaFdir2018)
"StoxLandingDataExample"

