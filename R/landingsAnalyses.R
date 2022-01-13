
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
#'  while preserving data for landings not reported in logbooks, and preseriving the total value equal to that in landings.
#' @details 
#'  Important: columns in landings that are not in 'valueColumns' and are not in the group definition ('catchPartition') will
#'  be duplicated exactly for the imputed catches. 
#'  That means that any weight-column not in 'valueColumns' will result in duplicated total weights for that column.
#'  That also means that any identifiers / keys may be copied and thus be rendered non-unique in the imputed landings.
#'  Landings are imputed and redistributed line-by-line, so that the total proportions of categorical variables not in the group definition ('catchPartition') are preserved for the columns in 'valueColumns'.
#'  
#'  Landings for which no partition can be found in 'catchPartition' are left untouched.
#'  Catchpartitions with no corresponding landings are ignored.
#'  
#'  Default values for parameters 'speciesCol' and 'valueColumns' are suitable for data read with \code{\link[RstoxData]{readLssFile}}.
#'  Default values for 'tripIdCol' corresponds to defaults for \code{\link[RstoxFDA]{makeTripIds}} and \code{\link[RstoxFDA]{appendTripIdLandings}}.
#' @param landings \code{\link[data.table]{data.table}} containing landings records, and a column identifying the trip that caught the landed catch ('tripIdCol').
#' @param catchPartition \code{\link[RstoxFDA]{TripPartition}} partitioning the catch of each trip on grouping variables.
#' @param tripIdCol character() that identifies the column in 'landings' that contain trip ids corresponding to those in 'catchPartition'
#' @param speciesCol character() that identifies the column in 'landings' that contain species ids corresponding to those in 'catchPartition'
#' @param valueColumns character() vector with names of columns that should be redistributed over the imputed landings
#' @seealso 
#'  \code{\link[RstoxFDA]{calculateLogbookPartitionByTrip}} for calculation of 'catchPartition'
#'  \code{\link[RstoxFDA]{makeTripIds}} and \code{\link[RstoxFDA]{appendTripIdLandings}} for obtaining 'landings' with trip-ids.
#' @return \code{\link[data.table]{data.table}} with 'landings' that have artificial landings imputed and 'valueColumns' redistributed to reflect the 'logbookPartition'.
#' @export
imputeCatchesLandings <- function(landings, catchPartition, tripIdCol="tripid", speciesCol="Art FAO (kode)", valueColumns=c("Bruttovekt", "Produktvekt", "Rundvekt")){
  
  groupCols <- names(catchPartition$groupDefinition)
  groupCols <- groupCols[!(groupCols=="groupid")]
  
  if (!all(groupCols %in% names(landings))){
    stop("Not all columns in group definition of 'catchPartition' are found in 'landings'")
  }
  if (!all(valueColumns %in% names(landings))){
    stop("Not all columns in 'valueColumns' are found in 'landings'")
  }
  if (!(tripIdCol %in% names(landings))){
    stop("'tripIdCol' is not found in 'landings'")
  }
  if ("fraction" %in% names(landings)){
    stop("'landings' may not have a column called 'fraction'.")
  }
  
  if (!all(catchPartition$fractions$tripid %in% landings[[tripIdCol]])){
    missing <- unique(catchPartition$fractions$tripid[!(catchPartition$fractions$tripid %in% landings[[tripIdCol]])])
    warning(paste("Not all tripids in 'catchPartition' are found in 'landings'", length(missing), "out of", length(unique(catchPartition$fractions$tripid)), "are missing."))
  }
  tripSpeciesRecords <- paste(catchPartition$fractions$tripid, catchPartition$fractions$species)
  tripSpeciesRecordsLanding <- paste(landings[[tripIdCol]], landings[[speciesCol]])
  if (!all(tripSpeciesRecords %in% tripSpeciesRecordsLanding)){
    missing <- unique(tripSpeciesRecords[!(tripSpeciesRecords %in% tripSpeciesRecordsLanding)])
    warning(paste("Not all species-trips (tripid/species) in 'catchPartition' are found in 'landings'", length(missing), "out of", length(unique(tripSpeciesRecords)), "are missing."))
  }
  catchPartition$fractions <- catchPartition$fractions[tripSpeciesRecords %in% tripSpeciesRecordsLanding,]
  notPartitioned <- landings[!(tripSpeciesRecordsLanding %in% tripSpeciesRecords),]
  partitioned <- landings[tripSpeciesRecordsLanding %in% tripSpeciesRecords,]
  
  if (nrow(partitioned)==0){
    return(notPartitioned)
  }
    
  tab <- merge(catchPartition$fractions, catchPartition$groupDefinition, by="groupid")
  cols <- names(tab)[names(tab)!=c("groupid")]
  tab <- tab[, .SD, .SDcols=cols]
  part <- partitioned[,.SD, .SDcols=!groupCols]
  data.table::setkeyv(part, cols=c(tripIdCol, speciesCol))
  data.table::setkeyv(tab, cols= c("tripid", "species"))
  partitioned <- part[tab, by=.EACHI, allow.cartesian=T]
  
  # distribute values
  for (v in valueColumns){
    partitioned[[v]] <- partitioned[[v]]*partitioned$fraction    
  }
  
  partitioned$fraction <- NULL
  result <- rbind(partitioned, notPartitioned)
  return(result)
}

#' Adjust landings by logbooks
#' @description 
#'  imputes landings and redistributes catch within quarter and area to mimic sales notes for each individual fishing operation (logbook record).
#'  Applicabe to formats parsed by \code{\link[RstoxData]{readLssFile}} and \code{\link[RstoxData]{logbookfile}}
#' @details 
#'  As there is some variation in naming conventions used in the supported formats, 
#'  relevant column names are paramaterized so that they can be changed to the appropriate format dialect.
#'  This may also be utilized to choose between coding systems. For instance 'gearCol' can be specified as 
#'  "REDSKAP_FAO" or "REDSKAP_NS" to use FAO-gear codes or NS-9400 gear codes, respectively.
#'  
#'  Sales-note lines identifiers are made unique after imputation and redistibution ('lineIdCol').
#' @param landings landings as returned by e.g. \code{\link[RstoxData]{readLssFile}}
#' @param logbook logbooks as returned by e.g. \code{\link[RstoxData]{logbookfile}}
#' @param gearCodes character() with gear-codes which logbook cleaning should be applied for (as they appear in 'gearCol'). If not provided, cleaning is applied to all gear codes
#' @param gearCol character() that identifies the column in 'logbook' that contain gear codes.
#' @param timeCol character() that identifies the column in 'logbook' that contain the time columns to use for inferring quarter.
#' @param dateLastCatchCol character() that identifies the column in 'landings' that contain the date of last catch.
#' @param locCol character() that identifies the column in 'logbook' that contain area and location info.
#' @param areaCol character() that identifies the column in 'landings' that contain the area code.
#' @param speciesColLog character() that identifies the column in 'logbook' that contain information about species of catch.
#' @param speciesColLand character() that identifies the column in 'landings' that contain information about species of catch.
#' @param weightColLog character() that identifies the column in 'logbook' that contain the weights that redistribution of weights should be based on.
#' @param valueColLand character() vector that identifies the columns in 'landings' that contain values that are to be redistributed over imputed landings (weight and value columns)
#' @param lineIdCol character() that identifies the column in 'landings' that contain the identifier for each line on a sales note.
#' @return 'landings' with catches redistributed over more sales-note lines corresponding to logbook-catch records / fishing operations.
#' @export
logbookAdjustment <- function(landings, logbooks, gearCodes=character(), gearCol="REDSKAP_FAO", timeCol="STARTTIDSPUNKT", dateLastCatchCol="Siste fangstdato", locCol="LOKASJON_START", areaCol="Hovedomr\u00E5de (kode)", speciesColLog="FANGSTART_FAO", speciesColLand="Art FAO (kode)", weightColLog="RUNDVEKT", valueColLand=c("Bruttovekt", "Produktvekt", "Rundvekt"), lineIdCol="Linjenummer"){
  
  if (!(gearCol) %in% names(logbooks)){
    stop("Column 'gearCol' not found in 'logbooks'")
  }
  if (any(is.na(logbooks[[gearCol]]))){
    stop("Column 'gearCol' has some missing values (NAs)")
  }
  if (!(lineIdCol) %in% names(landings)){
    stop("Column 'lineIdCol' not found in 'landings'")
  }
  if (any(is.na(landings[[lineIdCol]]))){
    stop("Column 'lineIdCol' has some missing values (NAs)")
  }
  if (!(timeCol) %in% names(logbooks)){
    stop("Column 'timeCol' not found in 'logbooks'")
  }
  if (any(is.na(logbooks[[timeCol]]))){
    stop("Column 'timeCol' has some missing values (NAs)")
  }
  if (!(dateLastCatchCol) %in% names(landings)){
    stop("Column 'dateLastCatch' not found in 'landings'")
  }
  if (any(is.na(landings[[dateLastCatchCol]]))){
    stop("Column 'dateLastCatchCol' has some missing values (NAs)")
  }
  if (!(locCol) %in% names(logbooks)){
    stop("Column 'locCol' not found in 'logbooks'")
  }
  if (any(is.na(logbooks[[locCol]]))){
    stop("Column 'locCol' has some missing values (NAs)")
  }
  if (!(areaCol) %in% names(landings)){
    stop("Column 'areaCol' not found in 'landings'")
  }
  if (any(is.na(landings[[areaCol]]))){
    stop("Column 'areaCol' has some missing values (NAs)")
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
  
  #make sure temp columns are not used
  if (any(c("mainarea", "quarter", "tripid") %in% names(landings))){
    stop("Columns 'mainarea' and 'quarter' are not allowed in 'landingsfile")
  }
  if (any(c("mainarea", "quarter", "tripid") %in% names(logbooks))){
    stop("Columns 'mainarea' and 'quarter' are not allowed in 'logbookfile")
  }
  
  tripIds <- makeTripIds(landings)
  landings <- appendTripIdLandings(landings)
  logbooks <- appendTripIdLogbooks(logbooks, tripIds)
  
  if (isGiven(gearCodes)){
    logbooks <- logbooks[logbooks[[gearCol]] %in% gearCodes]
  }
  
  #
  # annotate quarter and area
  #
  logbooks$quarter <- quarters(as.POSIXct(logbooks[[timeCol]]), abbreviate = T)
  landings$quarter <- quarters(landings[[dateLastCatchCol]], abbreviate = T)
  logbooks$mainarea <- substr(logbooks[[locCol]],1,2)
  landings$mainarea <- landings[[areaCol]]
  
  if (any(is.na(logbooks$tripid))){
    missing <- logbooks[is.na(logbooks$tripid),]
    catches <- paste(missing$RC, missing$STARTTIDSPUNKT, sep=":")
    all <- paste(logbooks$RC, logbooks$STARTTIDSPUNKT, sep=":")
    warning(paste("Some fishing operations could not be matched to a landing. Missing", length(unique(catches)), "out of", length(unique(all))))
    logbooks <- logbooks[!is.na(logbooks$tripid),]
  }
  
  catchPartition <- calculateLogbookPartitionByTrip(logbooks, groupCols = c("mainarea", "quarter"), speciesCol = speciesColLog, weightCol = weightColLog)
  
  landings <- imputeCatchesLandings(landings, catchPartition, speciesCol = speciesColLand, valueColumns = valueColLand)
  
  # fix line number ids so that they are unique
  landings[[lineIdCol]] <- 1:nrow(landings)
  
  #remove temp columns
  landings$mainarea <- NULL
  landings$quarter <- NULL
  landings$tripid <- NULL
  
  return(landings)
}
