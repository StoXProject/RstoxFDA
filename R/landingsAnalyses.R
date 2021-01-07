
#' Tabulate fisheries
#' @description Tabulates fisheries based on custom cell definitions
#' @details
#'  A fishery will be decomposed into cells, within which total weight will be reported.
#' @param data data.frame containg fisheries data
#' @param weightCol character() column in 'data' that contains catch weights
#' @param cellCols charcter() vector of columns in 'data' defining cells
#' @param complete logical() whether all combinations of the columns in cellCols should be tabulated, even if they contain no catch.
#' @return \code{\link[data.table]{data.table}} with the cells specified in cellCols tabulated by decreasing weight and with the columns 'weight', 'frac' and 'cumFrac' containing the weight in each cells and the fraction and cumulative fraction of total weight in that cell.
#' @examples
#'  data(landings)
#'  tabulateFisheries(landings)
#' @export
tabulateFisheries <- function(data, weightCol="LiveWeightKG", cellCols=c("Metier5", "quarter", "Area"), complete=F){

  if (any(c("weight", "cumFrac") %in% cellCols)){
    stop("Column names 'weight' and 'cumFrac' are reserved. Cannot be used as cell column.")
  }
  if (!all(cellCols %in% names(data))){
    stop("Some cell columns are not columns of 'data'")
  }

  aggVars <- list()
  for (v in cellCols){
    stopifnot(is.character(v))
    aggVars[[v]] <- data[[v]]
  }

  tab <- stats::aggregate(list(weight=data[[weightCol]]), by=aggVars, FUN=function(x){sum(x)}, drop=!complete)
  tab$weight[is.na(tab$weight)] <- 0
  tab <- tab[order(tab$weight, decreasing = T),]
  tab$frac <- tab$weight / sum(tab$weight)
  tab$cumFrac <- cumsum(tab$weight) / sum(tab$weight)

  return(data.table::as.data.table(tab))

}

#' Make trip IDs
#' @description
#'  Extracts trip IDs from landings.
#' @param landings \code{\link[data.table]{data.table}} containing landings records
#' @param vesselIdCol character() that identifies a column in 'landings' that contain the vessel id (e.g. radio call signal) of the landing vessel. Default compatible with \code{\link[RstoxData]{readLssFile}}.
#' @param lastCatchCol character() that identifies a Date column in 'landings' that contain the date of last catch for each record. Default compatible with \code{\link[RstoxData]{readLssFile}}.
#' @return \code{\link[data.table]{data.table}} with columns 'vesselId', 'time' and 'tripId'
#' @examples
#'  \dontrun{
#'    #make trip ids for landings
#'    lssfile <- "" #set appropriately
#'    landings <- RstoxData::readLssFile(lssfile)
#'    tripIds <- makeTripIds(landings)
#'  }
#' @export
#' @export
makeTripIds <- function(landings, vesselIdCol="Radiokallesignal (seddel)", lastCatchCol="Siste fangstdato"){

  if (!all(c(vesselIdCol, lastCatchCol) %in% names(landings))){
    stop("Columns identified by 'vesselIdCol' or 'lastCatchCol' not found in 'landings.")
  }

  tripIds <- unique(landings[,.SD, .SDcols=c(vesselIdCol, lastCatchCol)])
  names(tripIds) <- c("vesselId", "time")
  tripIds$time <- as.Date(tripIds$time)
  tripIds$tripId <- paste(tripIds$vesselId, tripIds$time, sep="/")
  return(tripIds)
}

#' Assign trip IDs
#' @description
#'  Assigns trip IDs to landings based on vessel identifier and date of last catch.
#' @details
#'  if 'tripIds' is NULL, trip IDs will be constructed from landings using \code{\link[RstoxFDA]{makeTripIds}}
#' @param landings \code{\link[data.table]{data.table}} containing landings records
#' @param tripIds \code{\link[data.table]{data.table}} with columns 'vesselId', 'time' and 'tripId'
#' @param vesselIdCol character() that identifies a column in 'landings' that contain the vessel id (e.g. radio call signal) of the landing vessel. Default compatible with \code{\link[RstoxData]{readLssFile}}.
#' @param lastCatchCol character() that identifies a POSIXct column in 'landings' that contain the time of last catch for each record. Default compatible with \code{\link[RstoxData]{readLssFile}}.
#' @param tripIdCol character() that identifies the column name to append to 'landings'
#' @return \code{\link[data.table]{data.table}} with columns 'vesselId' and 'time'
#' @examples
#'  \dontrun{
#'    #merge mesh size from first operation on a trip into landings
#'    lssfile <- "" #set appropriately
#'    logbfile <- "" #set appropriately
#'    landings <- RstoxData::readLssFile(lssfile)
#'    logbooks <- RstoxData::readErsFile(logbfile)
#'    tripIds <- makeTripIds(landings)
#'    logbooksWtripIds <- appendTripIdLogbooks(logbooks, tripIds)
#'    landingsWtripIds <- appendTripIdLandings(landings, tripIds)
#'
#'    firstCatch <- logbooksWtripIds[!duplicated(logbooksWtripIds$tripid),]
#'    landingsWmeshSize <- merge(landingsWtripIds,
#'                               firstCatch[,c("tripid", "MASKEVIDDE")],
#'                               by="tripid",
#'                               all.x=T)
#'  }
#' @export
appendTripIdLandings <- function(landings, tripIds=NULL, vesselIdCol="Radiokallesignal (seddel)", lastCatchCol="Siste fangstdato", tripIdCol="tripid"){

  if (!all(c(vesselIdCol, lastCatchCol) %in% names(landings))){
    stop("Columns identified by 'vesselIdCol' or 'lastCatchCol' not found in 'landings.")
  }

  if (is.null(tripIds)){
    tripIds <- makeTripIds(landings, vesselIdCol, lastCatchCol)
  }

  if (tripIdCol %in% names(landings)){
    stop("The column", tripIdCol,"already exist in 'landings'.")
  }
  if (!all(c(lastCatchCol, vesselIdCol) %in% names(landings))){
    stop("The columns identified by 'datecol' or 'vesselIdCol' are not found in 'landings'.")
  }
  if (!all(c("vesselId", "time", "tripId") %in% names(tripIds))){
    stop("The columns 'vesselId', 'time' or 'tripId' are not found in 'tripIds'.")
  }
  names(tripIds)[names(tripIds)=="tripId"] <- tripIdCol

  stopifnot(!any("dolcTemp" %in% names(landings)))
  landings$dolcTemp <- as.Date(landings[[lastCatchCol]])
  output <- merge(landings, tripIds, by.x=c(vesselIdCol, "dolcTemp"), by.y=c("vesselId", "time"), all.x=T)
  output$dolcTemp <- NULL
  return(output)

}
