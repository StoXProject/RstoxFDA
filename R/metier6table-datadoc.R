#' MetierTable lvl 6 example
#'
#' Example of metier table for assigning fishing activity to Metier level 6, based on gear codes and mesh-sizes.
#' This is not a universal conversion table, but an example of a table made for a particular purpose.
#' Some gear assignments are done with assumptions, and considering the exact metier codes acceptable by data recipients.
#'
#' For example it was agreed with data recipient that all bottom trawls should be reported as OTB, so paired trawl are explicitly grouped with otter trawls.
#'
#' There are other examples of pragmatic code-mapping as well. This particular one was explained in order to caution against indiscriminate use.
#' The code example below annotates an activity census of COD-catches with FAO-gearcode declarations and target sepcies declarations, and compares the annotated metiers with the codes used for reporting catch where shrimp was declared as target.
#'
#' @docType data
#'
#' @usage data(metier6table)
#'
#' @format \code{\link[RstoxFDA]{MetierTable}} with column 'gearcode' identifying gear codes used in Norwegian fisheries data (derived from NS 9400)
#'
#' @keywords datasets
#' @concept gear coding functions
#'
#' @examples
#' data(metier6table)
#' data(activityCensus)
#' annotated <- appendMetier(activityCensus[activityCensus$species=="COD"],
#'         metier6table,
#'         "gearNS",
#'         meshSizeColumn = "meshSize",
#'         metierColName = "metier6")
#' annotatedShrimp <- annotated[annotated$targetFAO %in% c("PAN", "PRA"),]
#' table(paste(annotatedShrimp$gearFAO, annotatedShrimp$targetFAO, sep="/"),
#'      annotatedShrimp$metier6)
"metier6table"
