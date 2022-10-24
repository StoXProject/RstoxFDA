#' MetierTable lvl 5 example
#'
#' Example of metier table for assigning fishing activity to Metier level 5, based on only gear codes.
#' This is not a universal conversion table, but an example of a table made for a particular purpose.
#' Some gear assignments are done with assumptions, and considering the exact metier codes acceptable by data recipients.
#'
#' For example NS9400 has codes for nephrops and shrimp trawls, and does not necessarilly distinguish pelagic shrimp trawls from bottom shrimp trawls.
#' Also the data recipient did not accept the FAO codes for nephrops and shrimp trawls,
#' so all of these gears are assigned to OTB, but target assemblage was set to either "DEF" (demershal fish) or "CRU" (crustaceans).
#'
#' There are other examples of pragmatic code-mapping as well. This particular one was explained in order to caution against indiscriminate use.
#' The code example below annotates an activity census of COD-catches with FAO-gearcode declarations and target sepcies declarations, and compares the annotated metiers with the codes used for reporting catch where shrimp was declared as target.
#'
#' @docType data
#'
#' @usage data(metier5table)
#'
#' @format \code{\link[RstoxFDA]{MetierTable}} with column 'gearcode' identifying gear codes used in Norwegian fisheries data (derived from NS 9400)
#'
#' @keywords datasets
#' @family gear coding functions
#'
#' @examples
#' data(metier5table)
#' data(activityCensus)
#' annotated <- appendMetier(activityCensus[activityCensus$species=="COD"],
#'          metier5table,
#'          "gearNS",
#'          metierColName = "metier5")
#' annotatedShrimp <- annotated[annotated$targetFAO %in% c("PAN", "PRA"),]
#' table(paste(annotatedShrimp$gearFAO, annotatedShrimp$targetFAO, sep="/"),
#'       annotatedShrimp$metier5)
"metier5table"
