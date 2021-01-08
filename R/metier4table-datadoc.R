#' MetierTable lvl 4
#'
#' Example of metier table for assigning fishing activity to Metier level 4.
#' This is not a universal conversion table, but an example of a table made for a particular purpose.
#' Some gear assignments are done with assumptions, and considering the exact metier codes acceptable by data recipients.
#'
#' @docType data
#'
#' @usage data(metier4table)
#'
#' @format \code{\link[RstoxFDA]{MetierTable}} with column 'gearcode' identifying gear codes used in Norwegian fisheries data (derived from NS 9400)
#'
#' @keywords datasets
#'
#' @examples
#' data(metier4table)
#' data(activityCensus)
#' annotated <- appendMetier(activityCensus, metier4table, "gearNS", metierColName = "metier4")
#' table(annotated$gearFAO, annotated$metier4)
"metier4table"
