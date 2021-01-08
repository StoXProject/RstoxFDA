#' Gear groups
#'
#' Example of metier table for assigning gear groups in NS9400 / Norwegian Directorate of fisheries
#'
#' @docType data
#'
#' @usage data(GearGroupFdirTable)
#'
#' @format \code{\link[RstoxFDA]{MetierTable}} with column 'gearcode' identifying gear codes used in Norwegian fisheries data (derived from NS 9400)
#'
#' @keywords datasets
#'
#' @examples
#' data(GearGroupFdirTable)
#' data(activityCensus)
#' annotated <- appendMetier(activityCensus,
#'                           GearGroupFdirTable,
#'                           "gearNS",
#'                           metierColName = "Hovedgruppe Redskap")
#' table(annotated$gearFAO, annotated$"Hovedgruppe Redskap")
"GearGroupFdirTable"
