
#' Assing trip ID to logbooks
#' @description
#'  Assings a trip ID to logbooks based on vessel identifier and a known last time of operation on a trip.
#' @details
#'  'tripid' need not be complete in the sense that vessels not found in 'tripid' will be assigned a trip ID of NA,
#'  and any fishing operations after the last trip for a vessel will be assinged a trip ID of NA.
#'  This function will however assign all the fishing operations of a vessel preceeding the first trip time to the first trip.
#'
#'  Runnning time is proportional to the number of trips in 'logbooks', so it is advantagous to annotate after other relevant filtering.
#' @param logbooks \code{\link[data.table]{data.table}} containing logbook records
#' @param tripIds \code{\link[data.table]{data.table}} with columns 'vesselId', 'time' and 'tripId'
#' @param timeCol character() that identifies a POSIXct column in 'logbooks' that contain the time of operation for each logbook record. Default compatible with \code{\link[RstoxData]{readErsFile}}.
#' @param vesselIdCol character() that identifies a column in 'logbooks' that contain the vessel id (e.g. radio call signal) of the reporting vessel. Default compatible with \code{\link[RstoxData]{readErsFile}}.
#' @param tripIdCol character() that identifies the column name to append to 'logbooks'
#' @param verbose logical() wheter to message progress
#' @return 'logbooks' with the column identified by 'tripIDcol' added
#' @examples
#'  \dontrun{
#'    #make trip ids from landings and assign them to logbooks
#'    lssfile <- "" #set appropriately
#'    logbfile <- "" #set appropriately
#'    landings <- RstoxData::readLssFile(lssfile)
#'    logbooks <- RstoxData::readErsFile(logbfile)
#'    tripIds <- makeTripIds(landings)
#'    logbooksWtripIds <- appendTripIdLogbooks(logbooks, tripIds)
#'  }
#' @family logbook functions
#' @export
appendTripIdLogbooks <- function(logbooks, tripIds, timeCol="STARTTIDSPUNKT", vesselIdCol="RC", tripIdCol="tripid", verbose=T){

  if (nrow(logbooks) == 0){
    logbooks[[tripIdCol]] <- character()
    return(logbooks)
  }
  
  if (tripIdCol %in% names(logbooks)){
    stop("The column ", tripIdCol," already exist in 'logbooks'.")
  }
  if (!all(c(timeCol, vesselIdCol) %in% names(logbooks))){
    stop("The columns identified by 'timeCol' or 'vesselIdCol' are not found in 'logbooks'.")
  }
  if (!all(c("vesselId", "time") %in% names(tripIds))){
    stop("The columns 'vesselId' or 'time' are not found in 'tripIds'.")
  }

  if (!is.POSIXct(logbooks[[timeCol]])){
    stop("The column 'timeCol' must be of class POSIXct")
  }
  
  tripIds <- tripIds[tripIds$vesselId %in% logbooks[[vesselIdCol]]]

  logbooks[[tripIdCol]] <- as.character(NA)

  #put tripID on all operation before the trip end time in descending order by time
  tripIds <- tripIds[order(tripIds$vesselId, tripIds$time, decreasing = T),]
  
  #
  # comparing dates without time is treatead as 00:00:00
  # as.POSIXct("2018-02-02 00:00:01") > as.POSIXct("2018-02-02")
  # add one day to tripId$time and compare with strictly less than
  #
  logdate <- logbooks[[timeCol]]
  
  #add a day and compare by strictly less, instead of less than or equal to, because POSIXct treats date-only as having time 00:00
  tripIds$time <- tripIds$time + as.difftime(1, units="days")
  #convert time to UTC
  tripIds$time <- as.POSIXct(format(tripIds$time, tz="UTC",usetz=TRUE), tz="UTC")
  
  for (i in 1:nrow(tripIds)){
    if (verbose & (i %% 1000 == 0)){
      message(paste("Prossessing trip", i, "/", nrow(tripIds), "(", tripIds$tripId[i],")"))
    }

    tripid <- tripIds$tripId[i]
    logbooks[[tripIdCol]][logbooks[[vesselIdCol]]==tripIds$vesselId[i] & logdate<tripIds$time[i]] <- tripid
  }

  if (any(is.na(logbooks[[tripIdCol]]))){
    warning("Not all entries were assigned a trip id. Some vessels may be missing from 'tripIds' or some vessels may have logbook records after the last trip in 'tripIds'")
  }

  return(logbooks)

}

#' Calcualte catch partition
#' @description
#'  Partitions total catch of each species for each trip on provided grouping variables.
#'  NAs in weights are ignored (treated as zero).
#' @details
#'  Default parameters are compatible \code{\link[RstoxData]{readErsFile}} with tripids annotated by \code{\link[RstoxFDA]{appendTripIdLogbooks}}.
#'
#'  For other partitionings of logbooks, consider \code{\link[RstoxFDA]{tabulateFisheries}}.
#' @param logbooks \code{\link[data.table]{data.table}} with logbooks
#' @param groupCols character() vector of names identifying the grouping columns in 'logbooks'
#' @param tripCol character() identyfing the column in 'logbooks' that identify a trip
#' @param speciesCol character() identyfing the column in 'logbooks' that specify species.
#' @param weightCol character() identifying the column in 'logbboks' that specify the live weight of the species.
#' @return \code{\link[RstoxFDA]{TripPartition}}
#' @examples
#'  \dontrun{
#'   #make calculate fractions in each area for each trip
#'   lssfile <- "" #set appropriately
#'   logbfile <- "" #set appropriately
#'   landings <- RstoxData::readLssFile(lssfile)
#'   logbooks <- RstoxData::readErsFile(logbfile)
#'   tripIds <- makeTripIds(landings)
#'   logbooksWtripIds <- appendTripIdLogbooks(logbooks, tripIds)
#'
#'   logbooksWtripIds$mainArea <- substring(logbooksWtripIds$LOKASJON_START,1,2)
#'   fractions <- calculateLogbookPartitionByTrip(logbooksWtripIds, "mainArea")
#'  }
#' @family logbook functions
#' @importFrom data.table .SD
#' @export
calculateLogbookPartitionByTrip <- function(logbooks, groupCols, tripCol="tripid", speciesCol="FANGSTART_FAO", weightCol="RUNDVEKT"){

  if (!all(groupCols %in% names(logbooks))){
    stop("Not all columns in 'groupCols' found in 'logbooks'")
  }
  if (!(tripCol %in% names(logbooks))){
    stop("Column 'tripCol' not found in 'logbooks'")
  }
  if (!(speciesCol %in% names(logbooks))){
    stop("Column 'speciesCol' not found in 'logbooks'")
  }
  if (!(weightCol %in% names(logbooks))){
    stop("Column 'weightCol' not found in 'logbooks'")
  }

  if (any(is.na(logbooks[[tripCol]]))){
    stop("Column 'tripCol' in 'logbooks' have some missing values.")
  }
  if (any(is.na(logbooks[[speciesCol]]))){
    stop("Column 'speciesCol' in 'logbooks' have some missing values.")
  }
  for (co in groupCols){
    if (any(is.na(logbooks[[co]]))){
      stop("Column '", co, "' in 'logbooks' have some missing values.")
    }
  }

  if (nrow(logbooks) == 0){
    output <- list()
    output$fractions <- data.table::data.table(tripid=character(), species=character(), groupid=character(), fraction=numeric())
    d<-rep(list(character()),length(groupCols)+1)
    names(d) <- c("groupid", groupCols)
    output$groupDefinition <- data.table::as.data.table(d)
    return(output)
  }
  
  totals <- data.table::as.data.table(stats::aggregate(list(totalw=logbooks[[weightCol]]), by=list(tripid=logbooks[[tripCol]], species=logbooks[[speciesCol]]), FUN=function(x){sum(x, na.rm=T)}))

  logbooks$groupid <- ""
  for (co in groupCols){
      logbooks$groupid <- paste(logbooks$groupid, logbooks[[co]], sep="/")
  }

  groupTotals <- data.table::as.data.table(stats::aggregate(list(totalGroup=logbooks[[weightCol]]), by=list(tripid=logbooks[[tripCol]], species=logbooks[[speciesCol]], groupid=logbooks$groupid), FUN=function(x){sum(x, na.rm=T)}))
  fractions <- merge(groupTotals, totals, all.x=T)

  fractions$fraction <- fractions$totalGroup / fractions$totalw
  fractions <- fractions[,c("tripid", "species", "groupid", "fraction")]

  columns <- c("groupid", groupCols)
  groupDefinition <- unique(logbooks[,.SD, .SDcols=columns])

  output <- list()
  output$fractions <- fractions
  output$groupDefinition <- groupDefinition

  return(output)
}




