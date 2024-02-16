#' Landings for the Norwegian North Sea Herring fisheries in 2022.
#'
#' Example of data formatted as \code{\link[RstoxData]{StoxLandingData}}
#' The data set contains landings of herring caugth in 2022 and sold as North Sea Herring, from Norwegian vessels larger than 15 m.
#' 
#' Samples from the Catch Lottery covering this fishery are provided in \code{\link[RstoxFDA]{CatchLotteryExample}}
#'
#' @docType data
#'
#' @usage data(CatchLotteryLandingsExample)
#'
#' @format \code{\link[RstoxData]{StoxLandingData}}
#'
#' @keywords datasets
#' @concept Analytical estimation
#'
#' @examples
#'  #report destination (country landed) of landed catch in tonnes
#'  RstoxFDA::CatchLotteryLandingExample$Landing[,list(weightT=sum(RoundWeight)/1000), by=c("CountryLanding")]
"CatchLotteryLandingExample"

