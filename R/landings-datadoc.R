#' Landings of Haddock
#'
#' Aggregated landings of Haddock by Norwegian vessels in 2018 as reported in salesnotes.
#' Gear-codes are converted from national standard and should be considered approximate.
#'
#' @docType data
#'
#' @usage data(landings)
#'
#' @format RDB CL-table version 1.3 (\href{https://www.ices.dk/marine-data/Documents/RDB/RDB Exchange Format.pdf}{RDBES exchange format v 1.13}). Some data types may differ from specification. Notably species, which is formatted as character().
#'
#' @keywords datasets
#'
#' @examples
#' data(landings)
#' sum(landings$OfficialLandingsWeight)
"landings"
