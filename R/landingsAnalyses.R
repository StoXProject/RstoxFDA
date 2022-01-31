
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
#' @return \code{\link[data.table]{data.table}} with columns 'vesselId', 'time' (POSIXct with time zone as in 'landings') and 'tripId'
#' @examples
#'  \dontrun{
#'    #make trip ids for landings
#'    lssfile <- "" #set appropriately
#'    landings <- RstoxData::readLssFile(lssfile)
#'    tripIds <- makeTripIds(landings)
#'  }
#' @export
makeTripIds <- function(landings, vesselIdCol="Radiokallesignal (seddel)", lastCatchCol="Siste fangstdato"){

  if (!data.table::is.data.table(landings)){
    stop("Parameter 'landings' must be a data table")
  }

  if (!all(c(vesselIdCol, lastCatchCol) %in% names(landings))){
    stop("Columns identified by 'vesselIdCol' or 'lastCatchCol' not found in 'landings.")
  }
  
  if (!is.POSIXct(landings[[lastCatchCol]])){
    stop("'lastCatchCol' must be of class POSIXct.")
  }

  tripIds <- unique(landings[,.SD, .SDcols=c(vesselIdCol, lastCatchCol)])
  names(tripIds) <- c("vesselId", "time")
  tripIds$tripId <- paste(tripIds$vesselId, substr(tripIds$time, 1,12), sep="/")
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

  if (!data.table::is.data.table(landings)){
    stop("Parameter 'landings' must be a data table")
  }

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
  landings$dolcTemp <- substr(landings[[lastCatchCol]],1,12)
  tripIds$timecTemp <- substr(tripIds$time,1,12)
  tripIds$time <- NULL
  output <- merge(landings, tripIds, by.x=c(vesselIdCol, "dolcTemp"), by.y=c("vesselId", "timecTemp"), all.x=T)
  output$dolcTemp <- NULL
  return(output)

}

#' Impute catches to landings
#' @description 
#'  Imputes landings to represent individual catches of species from a trip, and redistributes selected values over the imputed landings.
#'  This mimics a situation where landings are reported for each catch rather than each trip.
#'  This is useful to obtain a higher resolution on some grouping variables and consolidate logbook records with landing records, 
#'  while preserving data for landings not reported in logbooks, and preserving the total value equal to that in landings.
#' @details 
#'  The result of imputation have added lines for each corresponding catch in logbooks, 
#'  and 'valueColumns' redistributed to reflect the fraction of 'weightCol' that each catch 
#'  contributes to the total trip catch of the species ('speciesColLog').
#'  
#'  Important: columns in landings that are not in 'valueColumns' will
#'  be duplicated exactly for the imputed catches. 
#'  That means that any weight-column not in 'valueColumns' will result in duplicated total weights for that column.
#'  That also means that any identifiers / keys may be copied and thus be rendered non-unique in the imputed landings.
#'  
#'  Landings for which no trip can be found in 'logbooks' are left untouched.
#'  logbook entries with no corresponding landings are ignored.
#'  
#'  Default values for parameters 'speciesCol' and 'valueColumns' are suitable for data read with \code{\link[RstoxData]{readLssFile}}.
#'  Default values for 'tripIdCol' corresponds to defaults for \code{\link[RstoxFDA]{makeTripIds}}, \code{\link[RstoxFDA]{appendTripIdLandings}}, and \code{\link[RstoxFDA]{appendTripIdLogbooks}}.
#' @param landings \code{\link[data.table]{data.table}} containing landings records, and a column identifying the trip that caught the landed catch ('tripIdCol').
#' @param logbooks \code{\link[data.table]{data.table}} containing logbook records, and columns identifying the trip ('tripIdCol') and catch ('catchIdCol').
#' @param tripIdCol character() that identifies the column in 'landings' and 'logbooks' that contain trip ids
#' @param catchIdCol character() that identifies the column in 'logbooks' that contain catch ids
#' @param speciesColLand character() that identifies the column in 'landings' that contain species ids
#' @param speciesColLog character() that identifies the column in 'logbooks' that contain species ids
#' @param weightCol character() that identifies the column in 'logbooks' that contain the weights to use for redistribution of value columns.
#' @param valueColumns character() vector with names of columns that should be redistributed over the imputed landings
#' @seealso 
#'  \code{\link[RstoxFDA]{makeTripIds}}, \code{\link[RstoxFDA]{appendTripIdLandings}}, and \code{\link[RstoxFDA]{appendTripIdLogbooks}} for obtaining 'landings' with trip-ids.
#' @return \code{\link[data.table]{data.table}} with 'landings' that have artificial landings imputed, each catch identified by the added column 'catchIdCol'.
#' @export
imputeCatchesLandings <- function(landings, logbooks, tripIdCol="tripid", catchIdCol, speciesColLand="Art FAO (kode)", speciesColLog="FANGSTART_FAO", weightCol="RUNDVEKT", valueColumns=c("Bruttovekt", "Produktvekt", "Rundvekt")){
  
  if (!all(valueColumns %in% names(landings))){
    stop("Not all columns in 'valueColumns' are found in 'landings'")
  }
  if (!(tripIdCol %in% names(landings))){
    stop("'tripIdCol' is not found in 'landings'")
  }
  if (!(catchIdCol %in% names(logbooks))){
    stop("'catchIdCol' is not found in 'logbooks'")
  }
  if (any(c("fraction", catchIdCol) %in% names(landings))){
    stop("'landings' may not have a column called 'fraction' or the column provided by 'catchIdCol'.")
  }
  
  
  catchPartition <- calculateLogbookPartitionByTrip(logbooks, groupCols=catchIdCol, tripCol=tripIdCol, speciesCol = speciesColLog, weightCol = weightCol)
  
  if (!all(catchPartition$fractions$tripid %in% landings[[tripIdCol]])){
    missing <- unique(catchPartition$fractions$tripid[!(catchPartition$fractions$tripid %in% landings[[tripIdCol]])])
    warning(paste("Not all tripids in 'catchPartition' are found in 'landings'", length(missing), "out of", length(unique(catchPartition$fractions$tripid)), "are missing."))
  }
  tripSpeciesRecords <- paste(catchPartition$fractions$tripid, catchPartition$fractions$species)
  tripSpeciesRecordsLanding <- paste(landings[[tripIdCol]], landings[[speciesColLand]])
  if (!all(tripSpeciesRecords %in% tripSpeciesRecordsLanding)){
    missing <- unique(tripSpeciesRecords[!(tripSpeciesRecords %in% tripSpeciesRecordsLanding)])
    warning(paste("Not all species-trips (tripid/species) in 'catchPartition' are found in 'landings'", length(missing), "out of", length(unique(tripSpeciesRecords)), "are missing."))
  }
  
  catchPartition$fractions <- catchPartition$fractions[tripSpeciesRecords %in% tripSpeciesRecordsLanding,]
  notPartitioned <- landings[!(!is.na(tripSpeciesRecordsLanding) & tripSpeciesRecordsLanding %in% tripSpeciesRecords),]
  partitioned <- landings[!is.na(tripSpeciesRecordsLanding) & tripSpeciesRecordsLanding %in% tripSpeciesRecords,]
  
  if (nrow(partitioned)==0){
    notPartitioned[[catchIdCol]] <- NA
    warning("No logbook records were imputed.")
    return(notPartitioned)
  }
  
  tab <- merge(catchPartition$fractions, catchPartition$groupDefinition, by="groupid")
  cols <- names(tab)[names(tab)!="groupid"]
  tab <- tab[, .SD, .SDcols=cols]
  
  partitioned <- merge(partitioned, tab, by.x=c(tripIdCol, speciesColLand), by.y=c("tripid", "species"), all.x=T, allow.cartesian=T)
  # distribute values
  for (v in valueColumns){
    partitioned[[v]] <- partitioned[[v]]*partitioned$fraction    
  }
  
  notPartitioned[[catchIdCol]] <- NA
  class(notPartitioned[[catchIdCol]]) <- class(partitioned[[catchIdCol]])
  partitioned$fraction <- NULL
  
  result <- rbind(partitioned, notPartitioned)

  return(result)
}

#' Fill in values from logbooks
#' @description 
#'  Fills selected columns in imputed landings from corresponding logbook records
#' @details 
#'  The following columns in 'landings' may be altered. All others are left untouched:
#'  \describe{
#'   \item{Hovedområde (kode)}{set to corresponding value in logbooks}
#'   \item{Hovedområde}{set to NA for landings with corresponding entries in logbooks}
#'   \item{Lokasjon (kode)}{set to corresponding value in logbooks}
#'   \item{Siste fangstdato}{set to corresponding value in logbooks}
#'   \item{Redskap}{set to corresponding value in logbooks}
#'  }
#' 
#'  'landings' should already imputed with information about individual catches.
#'  The columns 'tripIdCol' and 'catchIdCol' must be set on 'landings' and 'logbooks'. This may be achieved by \code{\link[RstoxFDA]{appendTripIdLogbooks}}, \code{\link[RstoxFDA]{appendTripIdLandings}} and \code{\link[RstoxFDA]{imputeCatchesLandings}}.
#'  
#'  The function sets values for gear, area codes, and date of last catch from logbooks.
#'  Landings that does not have corresponding logbook records are left untouched
#' @param landings \code{\link[data.table]{data.table}} containing landings records, and columns identifying the trip and catch ('tripIdCol', 'catchIdCol').
#' @param logbooks \code{\link[data.table]{data.table}} containing logbook records, and columns identifying the trip and catch ('tripIdCol', 'catchIdCol').
#' @param tripIdCol character() that identifies the column in 'landings' and 'logbooks' that contain trip ids
#' @param catchIdCol character() that identifies the column in 'landings' and 'logbooks' that contain catch ids
#' @param speciesColLand character() that identifies the column in 'landings' that identify species landed
#' @param speciesColLog character() that identifies the column in 'logbooks' that identify species caught.
#' @seealso 
#'  \code{\link[RstoxFDA]{makeTripIds}}, \code{\link[RstoxFDA]{appendTripIdLandings}}, \code{\link[RstoxFDA]{appendTripIdLogbooks}}, and \code{\link[RstoxFDA]{imputeCatchesLandings}} for obtaining 'landings' and 'logbooks' with trip-ids and catch-ids.
#' @export
sourceLogbookColumns <- function(landings, logbooks, tripIdCol="tripid", catchIdCol, speciesColLand="Art FAO (kode)", speciesColLog="FANGSTART_FAO"){
  
  landCols <- c(catchIdCol, tripIdCol, speciesColLand)
  if (!all(landCols %in% names(landings))){
    missing <- landCols[!(landCols %in% names(landings))]
    stop("The columns identified by 'catchIdCol', 'tripIdCol', 'speciesColLand' must exist in 'landings'. Missing:", paste(missing, sep=","))
  }
  logCols <- c(catchIdCol, tripIdCol, speciesColLog)
  if (!all(logCols %in% names(logbooks))){
    missing <- logCols[!(logCols %in% names(logbooks))]
    stop("The columns identified by 'catchIdCol', 'tripIdCol', 'speciesColLog' must exist in 'logbooks'. Missing: ", paste(missing, sep=","))
  }
  landIds <- paste(landings[[tripIdCol]], landings[[catchIdCol]], landings[[speciesColLand]])
  logIds <- paste(logbooks[[tripIdCol]], logbooks[[catchIdCol]], logbooks[[speciesColLog]])
  
  selection <- match(landIds, logIds)
  mask <- !is.na(selection)
  selection <- selection[!is.na(selection)]
  
  landings[["Hovedomr\u00E5de (kode)"]][mask] <- substr(logbooks$LOKASJON_START,1,2)[selection]
  landings[["Hovedomr\u00E5de"]][mask] <- NA
  landings$`Lokasjon (kode)`[mask] <- substr(logbooks$LOKASJON_START,3,4)[selection]
  landings$`Siste fangstdato`[mask] <- as.POSIXct(substr(logbooks$STARTTIDSPUNKT,1,10), tz="UTC")[selection]
  landings$Redskap[mask] <- logbooks$REDSKAP_NS[selection]
  
  return(landings)
   
}

#' Add columns from logbooks
#' @description
#'  Add columns to landings from corresponding logbook entries.
#' @details
#'  'landings' should already imputed with information about individual catches.
#'  The columns 'tripIdCol' and 'catchIdCol' must be set on 'landings' and 'logbooks'. This may be achieved by \code{\link[RstoxFDA]{appendTripIdLogbooks}}, \code{\link[RstoxFDA]{appendTripIdLandings}} and \code{\link[RstoxFDA]{imputeCatchesLandings}}.
#'  
#'  'landings' may contain records with no correspond logbook entires ('tripIdCol' contains NAs).
#'  The added columns will have NA for these records.
#' @param logbookColumns character() vector that identifies columns in logbooks that are to be added to 'landings'
#' @param landings \code{\link[data.table]{data.table}} containing landings records, and columns identifying the trip and catch ('tripIdCol', 'catchIdCol').
#' @param logbooks \code{\link[data.table]{data.table}} containing logbook records, and columns identifying the trip and catch ('tripIdCol', 'catchIdCol').
#' @param tripIdCol character() that identifies the column in 'landings' and 'logbooks' that contain trip ids
#' @param catchIdCol character() that identifies the column in 'landings' and 'logbooks' that contain catch ids
#' @param speciesColLand character() that identifies the column in 'landings' that identify species landed
#' @param speciesColLog character() that identifies the column in 'logbooks' that identify species caught.
#' @seealso 
#'  \code{\link[RstoxFDA]{makeTripIds}}, \code{\link[RstoxFDA]{appendTripIdLandings}}, \code{\link[RstoxFDA]{appendTripIdLogbooks}}, and \code{\link[RstoxFDA]{imputeCatchesLandings}} for obtaining 'landings' and 'logbooks' with trip-ids and catch-ids.
#' @export
addLogbookColumns <- function(landings, logbooks, logbookColumns, tripIdCol="tripid", catchIdCol, speciesColLand="Art FAO (kode)", speciesColLog="FANGSTART_FAO"){
  
  landIds <- paste(landings[[tripIdCol]], landings[[catchIdCol]], landings[[speciesColLand]])
  logIds <- paste(logbooks[[tripIdCol]], logbooks[[catchIdCol]], logbooks[[speciesColLog]])
  
  selection <- match(landIds, logIds)
  mask <- selection[!is.na(selection)]
  selection <- selection[!is.na(selection)]
  
  for (col in logbookColumns){
    if (!(col %in% names(logbooks))){
      stop(paste("Column", col, "not found in 'logbooks'"))
    }
    if (col %in% names(landings)){
      stop(paste("Column", col, "already found in 'landings'"))
    }
    landings[[col]] <- NA
    class(landings[[col]]) <- class(logbooks[[col]])
    landings[[col]][mask] <-logbooks[[col]][selection]
  }
  
  return(landings)
}

#' Adjust landings by logbooks
#' @description 
#'  imputes landings and redistributes catch within quarter and area to mimic sales notes for each individual fishing operation (logbook record).
#'  Applicable to formats parsed by \code{\link[RstoxData]{readLssFile}} and \code{\link[RstoxData]{readErsFile}}
#' @details 
#'  Imputes landings for logbook records, redistributes catches, and adds information from logbooks.
#'  See details in utilized functions: \code{\link[RstoxFDA]{imputeCatchesLandings}}, 
#'  \code{\link[RstoxFDA]{sourceLogbookColumns}}, and \code{\link[RstoxFDA]{addLogbookColumns}}.
#'  In particular consult \code{\link[RstoxFDA]{sourceLogbookColumns}} to learn
#'  which columns in the 'landings' are affected by imputation.
#'  
#'  Logbook imputations are only applied for logbook records with the gears in 'gearCodes' if given.
#'  If gearCodes are not given (the default) all available logbook records are applied.
#'  
#'  Landings for which logbook records are not available, or not applied (by 'gearCode' restriction)
#'  are retained untouched in the returned landings.
#'  
#'  Sales-note lines identifiers are made unique after imputation and redistribution ('lineIdCol').
#' @param landings landings as returned by e.g. \code{\link[RstoxData]{readLssFile}}
#' @param logbooks logbooks as returned by e.g. \code{\link[RstoxData]{readErsFile}}
#' @param gearCodes character() with NS-9400 gear-codes which logbook cleaning should be applied for. If not provided, cleaning is applied to all gear codes
#' @param speciesColLog character() that identifies the column in 'logbook' that contain information about species of catch.
#' @param speciesColLand character() that identifies the column in 'landings' that contain information about species of catch.
#' @param weightColLog character() that identifies the column in 'logbook' that contain the weights that redistribution of weights should be based on.
#' @param valueColLand character() vector that identifies the columns in 'landings' that contain values that are to be redistributed over imputed landings (weight and value columns)
#' @param addColumns character() vector that identifies columns in 'logbooks' that should be added to 'landings'
#' @param activityTypes character() vector with the activity types that should be utilized from logbook records ('AKTIVITET_KODE')
#' @param polygons \code{\link[sp]{SpatialPolygonsDataFrame}} with area names in the column 'StratumName'. If provided, area ("Hovedområde kode") will be calculated from position, rather than fetched from logbook records.
#' @param lineIdCol character() that identifies the column in 'landings' that contain the identifier for each line on a sales note.
#' @return 'landings' with catches redistributed over more sales-note lines corresponding to logbook-catch records / fishing operations.
#' @export
logbookAdjustment <- function(landings, logbooks, gearCodes=character(), speciesColLog="FANGSTART_FAO", speciesColLand="Art FAO (kode)", weightColLog="RUNDVEKT", valueColLand=c("Bruttovekt", "Produktvekt", "Rundvekt"), addColumns=character(), activityTypes=c("FIS", "REL", "SCR"), polygons=RstoxFDA::mainareaFdir2018, lineIdCol="Linjenummer"){

  #
  # Test properly against old method before exporting
  #
  
  
  if (!(lineIdCol) %in% names(landings)){
    stop("Column 'lineIdCol' not found in 'landings'")
  }
  if (any(is.na(landings[[lineIdCol]]))){
    stop("Column 'lineIdCol' has some missing values (NAs)")
  }
  
  if (!(speciesColLog) %in% names(logbooks)){
    stop("Column 'speciesColLog' not found in 'logbooks'")
  }
  if (any(is.na(logbooks[[speciesColLog]]))){
    stop("Column 'speciesColLog' has some missing values (NAs)")
  }
  if (!(speciesColLand) %in% names(landings)){
    stop("Column 'speciesColLand' not found in 'landings'")
  }
  if (!(weightColLog) %in% names(logbooks)){
    stop("Column 'weightColLog' not found in 'logbooks'")
  }
  if (any(is.na(logbooks[[weightColLog]]))){
    stop("Column 'weightColLog' has some missing values (NAs)")
  }
  if (any(is.na(landings[[speciesColLand]]))){
    stop("Column 'speciesColLand' has some missing values (NAs)")
  }
  if (!all(valueColLand %in% names(landings))){
    stop("Some columns in 'valueColLand' are not found in 'landings'")
  }
  
  if (length(activityTypes)==0){
    stop("Must provide at least one activity type ('activityTypes')")
  }
  logbooks <- logbooks[logbooks$AKTIVITET_KODE %in% activityTypes,]
  logbooks$catchId <- 1:nrow(logbooks)
  
  #check that tempcols are not used
  catchIdCol <- "catchId"
  if (any(c(catchIdCol, "tripid") %in% names(landings))){
    stop(paste("The columns '", catchIdCol, "' and 'tripid' should not exist in 'landings"))
  }
  
  tripIds <- makeTripIds(landings)
  landings <- appendTripIdLandings(landings)
  logbooks <- appendTripIdLogbooks(logbooks, tripIds)

  if (isGiven(gearCodes)){
    if (!is.character(gearCodes)){
      stop(paste("'gearCodes must be provided as character"))
    }
    tripids <- landings$tripid[!is.na(landings[["Redskap (kode)"]]) & (landings[["Redskap (kode)"]] %in% gearCodes)]
    logbooks <- logbooks[logbooks$tripid %in% tripids,]
    logbooks <- logbooks[logbooks$REDSKAP_NS %in% gearCodes,]
  }

  if (nrow(logbooks)>0){
    if (any(is.na(logbooks$tripid))){
      missing <- logbooks[is.na(logbooks$tripid),]
      catches <- paste(missing$RC, missing$STARTTIDSPUNKT, sep=":")
      all <- paste(logbooks$RC, logbooks$STARTTIDSPUNKT, sep=":")
      warning(paste("Some fishing operations could not be matched to a landing. Missing", length(unique(catches)), "out of", length(unique(all))))
      logbooks <- logbooks[!is.na(logbooks$tripid),]
    }
    
    landings <- imputeCatchesLandings(landings, logbooks, speciesColLand = speciesColLand, valueColumns = valueColLand, catchIdCol = catchIdCol)
    landings <- sourceLogbookColumns(landings, logbooks, tripIdCol = "tripid", catchIdCol = catchIdCol)
    landings <- addLogbookColumns(landings, logbooks, unique(c(addColumns, "START_LT", "START_LG")), tripIdCol = "tripid", catchIdCol = catchIdCol)    
    
    #set area from position if positions are available
    if (!is.null(landings$START_LG) & isGiven(polygons)){
      # set area from position when available
      landingsWPos <- landings[!is.na(landings$START_LT) & !is.na(landings$START_LG),]
      landingsWoPos <- landings[is.na(landings$START_LT) | is.na(landings$START_LG),]
      
      if (nrow(landingsWPos)>0){
        landingsWPos <- RstoxFDA::appendAreaCode(landingsWPos, polygons, "START_LT", "START_LG", "areaFromPos", StratumName = "StratumName")
        landingsWPos[["Hovedomr\u00E5de (kode)"]][!is.na(landingsWPos$areaFromPos)] <- landingsWPos$areaFromPos[!is.na(landingsWPos$areaFromPos)]
        landingsWPos$areaFromPos <- NULL
        landings <- rbind(landingsWPos, landingsWoPos)
      }
      else{
        landings <- landingsWoPos
      }
    }
    
    #remove position columns unless requested by user
    if (!("START_LT" %in% addColumns)){
      landings$START_LT <- NULL
    }
    if (!("START_LG" %in% addColumns)){
      landings$START_LG <- NULL
    }
    
  }

  # fix line number ids so that they are unique
  landings[[lineIdCol]] <- 1:nrow(landings)
  
  #remove temp columns
  landings$tripid <- NULL
  landings[[catchIdCol]] <- NULL
  
  return(landings)
}


#' Adjust landings with logbooks (reference implementation from stox2.7preprocessing package)
#' Kept for testing. Column names adjusted to formats read by rstoxFDA.
#' @description
#'  Adjust landings with information from logbooks for a specific species.
#' @details
#'  Logbooks contain catch reports for each fishing operation as defined in legislation for different gears.
#'  Parameters like catch date and area are defined in landings (sales notes) by a summary value for each trip.
#'  E.g. the dominant area for the trip (the area with more catch), and the date of the last catch of the trip.
#'  Applying this function alters the definition of some of these parameters to be resolved to fishing operation
#'  when information is available from logbook records.
#'  The information from logbooks are not simply copied over, but the landed weight is redistributed
#'  to reflect the partitioning of weights reported in logbooks
#'  In addition positions are added when logbook/VMS entries are available
#'  (columns FOlat and FOlon are added to landings for latitdue and longitude of fishing operation, respectively).
#'
#'  In general, the difference in definition between logbooks and landing are less likely to be relevant
#'  for shorter trips. Trip length potential is limited by vessel size.
#'  Logbook records was first introduced for trawlers, and later for all vessels larger than or equal to 15 m.
#'  Selection for certain gears or vessel sizes is supported by the parameters 'gearCodes' and 'vesselSize'
#'
#'  Logbook records may contain records of activities other than fishing. Some of them may relate to
#'  activity that does not consitute removal of fish from the sea (such as transfer of fish between vessels).
#'  Selection of activities to include is supported by the parameter 'activities'. Catch is occationally
#'  recorded for operations which do not handle fish (such as STE for steaming or SET for setting of gear), these
#'  are likely misrecorded and should possibly be included. Codes for transport and production (HAU and PRO) may
#'  indicate processing and transport of the catch weight and should probably be excluded. By default only
#'  codes for fishing (FIS), harvesting from another ships gear (REL) and research-catches (SCR) are included.
#'
#'  A trip may be reflected in several sales notes. This function distributes the landed
#'  weight for each of them on the temporal and spatial spatial variables.
#'  That is: each sales-note line with a corresponding logbook entry is replaced by
#'  a set of sale-note lines, one for each fishing operation in the corresponding trip.
#'  This is done regardless of parameters that may be specific to certain fishing operations,
#'  such as gear, and it is done without attempt to make a realistic partitioning of
#'  parameters determined at landing, such as usage.
#'
#'  trips are deduced by vessel identifiers (radio call-signal) and dates (date of last catch
#'  in landings, and date of fishing operation in logbooks). Consisteecy of gears reported
#'  between logboks and landings are not enforced.
#'
#'  Only codes are changed. Corresponding description columns are not. E.g.:
#'  Area code (Hovedområde_kode) may reflect fishing operation after logbook-adustments,
#'  while area description (Hovedområde_bokmål) will reflect dominant area for trip.
#'
#' @param landings landings as parsed by \code{\link[stox2.7preprocessing]{readLandings}}
#' @param logbooks logbooks as parsed by \code{\link[RstoxData]{readErsFile}}
#' @param speciesFAO 3-alpha FAO species code, e.g. "COD"
#' @param gearCodes Gear codes (NS9400) that should be fetched from logbooks. If null, all available gears are included.
#' @param vesselSize smallest vessel size to include from logbooks. If null, all availalbe vessels sizes are included.
#' @param activities activity-types to include from logbooks. If null, all available activites are included.
#' @return landings with columns FOlat and FOlon added, and adjusted definitions for columns:
#' \describe{
#'  \item{Hovedområde_kode}{dominant area for trip OR area of fishing operation}
#'  \item{Lokasjon_kode}{dominant location (sub area) OR location of fishing operation}
#'  \item{SisteFangstdato}{date of last catch OR date of fishing operation}
#'  \item{Rundvekt}{live weight (round weight) listed in sales-note OR weight ascribed to fishing operation}
#'  \item{Produktvekt}{product weight listed in sales-note OR weight ascribed to fishing operation}
#'  \item{Bruttovekt}{gross weight of landed fish listed in sales-note OR weight ascribed to fishing operation}
#'  Definitions depend on whether logbook information was available for the sales-note.
#' }
#' @noRd
adjustWithLogbookRefImpl <- function(landings, logbooks, speciesFAO, gearCodes=NULL, vesselSize=NULL, activities=c("FIS", "REL", "SCR")){
  logbooks <- logbooks[logbooks$FANGSTART_FAO %in% speciesFAO,]
  if (nrow(logbooks)==0){
    stop(paste("No logbook records found for species:", paste(speciesFAO, collapse=",")))
  }
  if (!is.null(gearCodes)){
    logbooks <- logbooks[logbooks$REDSKAP_NS %in% gearCodes,]
    if (nrow(logbooks)==0){
      stop(paste("No logbook records found that match criteria."))
    }
  }
  else{
    gearCodes <- unique(c(landings[["Redskap (kode)"]], logbooks$REDSKAP_NS))
  }
  if (!is.null(vesselSize)){
    logbooks <- logbooks[logbooks[["ST\u00D8RSTE_LENGDE"]] >= vesselSize,]
    if (nrow(logbooks)==0){
      stop(paste("No logbook records found that match criteria."))
    }
  }
  if (!is.null(activities)){
    logbooks <- logbooks[logbooks$AKTIVITET_KODE %in% activities,]
    if (nrow(logbooks)==0){
      stop(paste("No logbook records found that match criteria."))
    }
  }
  
  #removed because of difference in landings formats from original reference impl (stox2.7preprocessing)
  #landings$lastCatch <- as.POSIXct(landings$SisteFangstdato, format="%d.%m.%Y")
  
  tripids <- RstoxFDA::makeTripIds(landings, vesselIdCol = "Radiokallesignal (seddel)", lastCatchCol = "Siste fangstdato")
  logbooks <- suppressWarnings(RstoxFDA::appendTripIdLogbooks(logbooks, tripids, vesselIdCol = "RC", timeCol ="STARTTIDSPUNKT"))
  logbooks <- logbooks[!is.na(logbooks$tripid),]
  logbooks$FOarea <- substr(logbooks$LOKASJON_START, 1, 2)
  logbooks$FOloc <- substr(logbooks$LOKASJON_START, 3, 4)
  logbooks$FOlat <- logbooks$START_LT
  logbooks$FOlon <- logbooks$START_LG
  logbooks$FOtime <- logbooks$STARTTIDSPUNKT
  
  logbooks <- logbooks[!is.na(logbooks$FOarea) & !is.na(logbooks$FOloc) & !is.na(logbooks$FOlat) & !is.na(logbooks$FOlon) & !is.na(logbooks$FOtime) & !is.na(logbooks$FANGSTART_FAO),]
  if (nrow(logbooks)==0){
    stop(paste("No logbook records found that match criteria, and have all necessary information."))
  }
  
  # calculate fraction of trip catch of species for each haul
  # add fishin operation parameters (FO)
  tripartitions <- RstoxFDA::calculateLogbookPartitionByTrip(logbooks, groupCols = c("FOtime", "FOarea", "FOloc", "FOlat", "FOlon", "FANGSTART_FAO"))
  
  tripartitions <- merge(tripartitions$fractions, tripartitions$groupDefinition, by="groupid")
  tripartitions$FOlat <- as.numeric(tripartitions$FOlat)
  tripartitions$FOlon <- as.numeric(tripartitions$FOlon)
  
  landings <- RstoxFDA::appendTripIdLandings(landings, tripIds = tripids, vesselIdCol = "Radiokallesignal (seddel)", lastCatchCol = "Siste fangstdato")
  hasLogb <- landings[!is.na(landings$tripid) & landings$tripid %in% tripartitions$tripid &
                        !is.na(landings[["Art FAO (kode)"]]) &
                        landings[["Art FAO (kode)"]] == speciesFAO &
                        landings[["Redskap (kode)"]] %in% gearCodes,] #last clause necessary because of some inconsistency in gear coding between logbooks and landings

  rest <- landings[!(!is.na(landings$tripid) & landings$tripid %in% tripartitions$tripid &
                       !is.na(landings[["Art FAO (kode)"]]) &
                       landings[["Art FAO (kode)"]] == speciesFAO &
                       landings[["Redskap (kode)"]] %in% gearCodes),]
  rest$FOlon <- as.numeric(NA)
  rest$FOlat <- as.numeric(NA)
  
  hasLogb <- merge(hasLogb, tripartitions, by.x=c("tripid", "Art FAO (kode)"), by.y=c("tripid", "FANGSTART_FAO"), all.x=T, allow.cartesian=T)
  hasLogb$Rundvekt <- hasLogb$Rundvekt*hasLogb$fraction
  hasLogb$Bruttovekt <- hasLogb$Bruttovekt*hasLogb$fraction
  hasLogb$Produktvekt <- hasLogb$Produktvekt*hasLogb$fraction
  hasLogb[["Hovedomr\u00E5de (kode)"]] <- hasLogb$FOarea
  
  hasLogb$Lokasjon_kode <- hasLogb$FOloc
  hasLogb$SisteFangstdato <- strftime(hasLogb$FOtime, format="%d.%m.%Y")
  hasLogb <- hasLogb[,names(hasLogb) %in% names(rest), with=F]
  
  landings <- rbind(rest, hasLogb)
  
  #
  # fix keys (linjenummer)
  #
  
  landings$Linjenummer <- 1:nrow(landings)
  return(landings)
}
