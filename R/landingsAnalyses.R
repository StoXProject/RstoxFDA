
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

  if (!data.table::is.data.table(landings)){
    stop("Parameter 'landings' must be a data table")
  }

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
  landings$dolcTemp <- as.Date(landings[[lastCatchCol]])
  output <- merge(landings, tripIds, by.x=c(vesselIdCol, "dolcTemp"), by.y=c("vesselId", "time"), all.x=T)
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
imputeCatchesLandings <- function(landings, logbooks, tripIdCol="tripid", catchIdCol="STARTTIDSPUNKT", speciesColLand="Art FAO (kode)", speciesColLog="FANGSTART_FAO", weightCol="RUNDVEKT", valueColumns=c("Bruttovekt", "Produktvekt", "Rundvekt")){
  
  catchkey <- paste(logbooks[[catchIdCol]], logbooks[[speciesColLog]], logbooks[[tripIdCol]])
  if (length(unique(catchkey)) != nrow(logbooks)){
    stop("The columns 'tripIdCol', 'catchIdCol', and 'speciesColLog' must uniquely identify a logbook record.")
  }
  if (!all(valueColumns %in% names(landings))){
    stop("Not all columns in 'valueColumns' are found in 'landings'")
  }
  if (!(tripIdCol %in% names(landings))){
    stop("'tripIdCol' is not found in 'landings'")
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
  notPartitioned <- landings[!(tripSpeciesRecordsLanding %in% tripSpeciesRecords),]
  partitioned <- landings[tripSpeciesRecordsLanding %in% tripSpeciesRecords,]
  
  if (nrow(partitioned)==0){
    notPartitioned[[catchIdCol]] <- NA
    warning("No logbook records were imputed.")
    return(notPartitioned)
  }
  
  tab <- merge(catchPartition$fractions, catchPartition$groupDefinition, by="groupid")
  cols <- names(tab)[names(tab)!=c("groupid")]
  tab <- tab[, .SD, .SDcols=cols]
  data.table::setkeyv(partitioned, cols=c(tripIdCol, speciesColLand))
  data.table::setkeyv(tab, cols= c("tripid", "species"))
  partitioned <- partitioned[tab, allow.cartesian=T]
  
  # distribute values
  for (v in valueColumns){
    partitioned[[v]] <- partitioned[[v]]*partitioned$fraction    
  }
  
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
sourceLogbookColumns <- function(landings, logbooks, tripIdCol="tripid", catchIdCol="STARTTIDSPUNKT", speciesColLand="Art FAO (kode)", speciesColLog="FANGSTART_FAO"){
  
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
  mask <- selection[!is.na(selection)]
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
addLogbookColumns <- function(landings, logbooks, logbookColumns, tripIdCol="tripid", catchIdCol="STARTTIDSPUNKT", speciesColLand="Art FAO (kode)", speciesColLog="FANGSTART_FAO"){
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
#' @param lineIdCol character() that identifies the column in 'landings' that contain the identifier for each line on a sales note.
#' @return 'landings' with catches redistributed over more sales-note lines corresponding to logbook-catch records / fishing operations.
#' @export
logbookAdjustment <- function(landings, logbooks, gearCodes=character(), speciesColLog="FANGSTART_FAO", speciesColLand="Art FAO (kode)", weightColLog="RUNDVEKT", valueColLand=c("Bruttovekt", "Produktvekt", "Rundvekt"), addColumns=character(), activityTypes=c("FIS", "REL", "SCR"), lineIdCol="Linjenummer"){
  
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
  
  #check that tempcols are not used
  catchIdCol <- "STARTTIDSPUNKT"
  if (any(c(catchIdCol, "tripid") %in% names(landings))){
    stop(paste("The columns '", catchIdCol, "' and 'tripid' should not exist in 'landings"))
  }

  tripIds <- makeTripIds(landings)
  landings <- appendTripIdLandings(landings)
  logbooks <- appendTripIdLogbooks(logbooks, tripIds)
  
  if (isGiven(gearCodes)){
    tripids <- landings$tripid[(is.na(landings[["Redskap (kode)"]]) | (landings[["Redskap (kode)"]] %in% gearCodes))]
    logbooks <- logbooks[!(logbooks$tripid %in% tripids),]
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
    landings <- sourceLogbookColumns(landings, logbooks, tripIdCol = "tripid")
    landings <- addLogbookColumns(landings, logbooks, addColumns, tripIdCol = "tripid", catchIdCol = catchIdCol)    
  }

  # fix line number ids so that they are unique
  landings[[lineIdCol]] <- 1:nrow(landings)
  
  #remove temp columns
  landings$tripid <- NULL
  landings[[catchIdCol]] <- NULL
  
  return(landings)
}
