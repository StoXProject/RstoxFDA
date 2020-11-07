
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
#' @param datecol character() that identifies a POSIXct column in 'logbooks' that contain the time of operation for each logbook record. Default compatible with \code{\link[RstoxData]{readErsFile}}.
#' @param vesselIdCol character() that identifies a column in 'logbooks' that contain the vessel id (e.g. radio call signal) of the reporting vessel. Default compatible with \code{\link[RstoxData]{readErsFile}}.
#' @param tripIDcol character() that identifies the column name to append to 'logbooks'
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
#'    logbooksWtripIds <- assignTripIdLogbooks(logbooks, tripIds)
#'  }
#' @export
assignTripIdLogbooks <- function(logbooks, tripIds, timeCol="STARTTIDSPUNKT", vesselIdCol="RC", tripIdCol="tripid", verbose=T){

  if (tripIdCol %in% names(logbooks)){
    stop("The column", tripIdCol,"already exist in 'logbooks'.")
  }
  if (!all(c(timeCol, vesselIdCol) %in% names(logbooks))){
    stop("The columns identified by 'datecol' or 'vesselIdCol' are not found in 'logbooks'.")
  }
  if (!all(c("vesselId", "time") %in% names(tripIds))){
    stop("The columns 'vesselId' or 'time' are not found in 'tripIds'.")
  }

  tripIds <- tripIds[tripIds$vesselId %in% logbooks[[vesselIdCol]]]

  logbooks[[tripIdCol]] <- as.character(NA)

  #put tripID on all operation before the trip end time in descending order by time
  tripIds <- tripIds[order(tripIds$vesselId, tripIds$time, decreasing = T),]
  logdate <- as.Date(logbooks[[timeCol]])
  for (i in 1:nrow(tripIds)){
    if (verbose & (i %% 1000 == 0)){
      message(paste("Prossessing trip", i, "/", nrow(tripIds), "(", tripIds$tripId[i],")"))
    }

    tripid <- tripIds$tripId[i]
    logbooks[[tripIdCol]][logbooks[[vesselIdCol]]==tripIds$vesselId[i] & logdate<=tripIds$time[i]] <- tripid
  }

  if (any(is.na(logbooks[[tripIdCol]]))){
    warning("Not all entries were assigned a trip id. Some vessels may be missing from 'tripIds' or some vessels may have logbook records after the last trip in 'tripIds'")
  }

  return(logbooks)

}
