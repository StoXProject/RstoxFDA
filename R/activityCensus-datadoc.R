#' Activity census
#'
#' Example of activity census.
#'
#'
#' @docType data
#'
#' @usage data(activityCensus)
#'
#' @format \code{\link[data.table]{data.table}} with columns
#'  \describe{
#'   \item{gearFAO}{gear (FAO ISSCFG 1980).}
#'   \item{gearNS}{gear (code based on NS 9400, Norwegian standard).}
#'   \item{targetFAO}{target species (FAO ASFIS).}
#'   \item{meshSize}{mesh size (mm, bar length).}
#'   \item{vesselLengthCategory}{length group for vessels ranges in m.}
#'   \item{species}{reported species (FAO ASFIS).}
#'   \item{wholeWeightKg}{total catch within activity group in kg.}
#'  }
#'
#' @keywords datasets
#'
#' @examples
#' data(activityCensus)
#' sum(activityCensus$OfficialLandingsWeight)
"activityCensus"
