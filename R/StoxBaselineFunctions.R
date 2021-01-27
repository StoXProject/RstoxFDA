
#' Check if parameter is given
#' @noRd
isGiven <- function(value){
  if (is.null(value)){
    return(FALSE)
  }
  if (length(value) == 0){
    return(FALSE)
  }
  if (length(value) == 1){
    
    if (value == ""){
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Checks symmetry of Car table
#' @noRd
checkSymmetry <- function(tab){

  #to get data.table notation throguh check
  CarValue <- Neighbours <- NULL

  getn <- function(value){
    neighbours <- trimws(unlist(strsplit(tab[CarValue==value,Neighbours], split = ",")))
    return(neighbours)
  }

  for (i in 1:nrow(tab)){
    carvalue <- tab[i,1]
    neighbours <- getn(carvalue)
    for (n in neighbours){
      if (!(n %in% tab[["CarValue"]]) | !(carvalue %in% getn(n))){
        stop(paste("Neighbour definition not symmetric.", n, "is neighbour of", carvalue, "but not vice versa."))
      }
    }
  }
}


#' Append position to landings data
#' @description
#'  StoX function
#'  Appends a position to landings data, based on Area and Location codes.
#' @details
#'  Positions are appended in the new columns 'Latitude' and 'Longitude'
#'  When 'LocationVariable' is specified as 'None' Area is looked up from 'AreaPosition', using the row where 'Location' is missing.
#'  When 'LocationVariable' is specified as 'Location', 'Area' and 'Location' in 'StoxLandingData' is looked up against 'Area' and 'Location' in 'AreaPosition'.
#'  When 'LocationVariable' is specified as 'Coastal', 'Area' and 'Costal' in 'StoxLandingData' is looked up against 'Area' and 'Location' in 'AreaPosition'.
#' @param StoxLandingData landing data, see \code{\link[RstoxData]{StoxLandingData}}
#' @param AreaPosition coordinates for Area and Location codes, see \code{\link[RstoxFDA]{AreaPosition}}
#' @param LocationVariable Specify which column in 'StoxLandingsData' should are represented by 'Location' in 'AreaPosition'. See details.
#' @return \code{\link[RstoxData]{StoxLandingData}} with columns for latitude and longitude appended.
#' @export
AddAreaPositionStoxLanding <- function(StoxLandingData, AreaPosition, LocationVariable = c("None", "Location", "Coastal")){
  
  LocationVariable <- match.arg(LocationVariable)
  
  latColName="Latitude"    
  lonColName="Longitude"
  

  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.AreaPosition(AreaPosition))

  if (latColName %in% names(StoxLandingData$landings)){
    stop(paste("Column", latColName, "already exists."))
  }
  if (lonColName %in% names(StoxLandingData$landings)){
    stop(paste("Column", lonColName, "already exists."))
  }

  AreaPosition[[latColName]] <- AreaPosition$Latitude
  AreaPosition[[lonColName]] <- AreaPosition$Longitude

  if (LocationVariable == "None"){
    
    if (!all(StoxLandingData$landings$Area %in% AreaPosition$Area)){
      missing <- StoxLandingData$landings$Area[!(StoxLandingData$landings$Area %in% AreaPosition$Area)]
      stop(paste("Positions not provided for all Areas in StoxLandingData. Missing: ", paste(missing, collapse=",")))
    }
    if (!all(StoxLandingData$landings$Area %in% AreaPosition$Area[is.na(AreaPosition$Location)])){
      missing <- StoxLandingData$landings$Area[!(StoxLandingData$landings$Area %in% AreaPosition$Area[is.na(AreaPosition$Location)])]
      stop(paste("Positions is not provided for the case of missing Location for some Areas in StoxLandingData: ", paste(missing, collapse=",")))
    }
    AreaPosition <- AreaPosition[is.na(AreaPosition$Location),c("Area", latColName, lonColName), with=F]
    stopifnot(!any(duplicated(AreaPosition$Area)))
    
    StoxLandingData$landings <- data.table::as.data.table(merge(StoxLandingData$landings, AreaPosition, by.x="Area", by.y="Area", all.x=T))
    return(StoxLandingData)
  }
  else if (LocationVariable %in% c("Location", "Coastal")){
    
    arealocdata <- paste(StoxLandingData$landings$Area, StoxLandingData$landings[[LocationVariable]], sep="-")
    arealocresource <- paste(AreaPosition$Area, AreaPosition$Location, sep="-")
    if (!all(arealocdata %in% arealocresource)){
      missing <- arealocdata[!(arealocdata %in% arealocresource)]
      stop(paste("Positions not provided for all Areas and Locations in StoxLandingData Missing: ", paste(missing, collapse=",")))
    }
    AreaPosition <- AreaPosition[,c("Area", "Location", latColName, lonColName), with=F]
    stopifnot(!any(duplicated(paste(AreaPosition$Area, AreaPosition$Location))))
    StoxLandingData$landings <- data.table::as.data.table(merge(StoxLandingData$landings, AreaPosition, by.x=c("Area", LocationVariable), by.y=c("Area", "Location"), all.x=T))
    return(StoxLandingData)
  }
  else{
    stop(paste("Parameter", LocationVariable, "is not supported for 'LocationVariable'."))
  }

}

#' Set position Biotic
#' @description 
#'  Sets start position based on area codes.
#' @details 
#'  Positions are appended in the columns 'latitudestart' and 'longitudestart' on 'fishstation' 
#'  whenever the value in the column 'system' on 'fishstation' is equal to the parameter 'System'.
#'  These columns are on the table 'fishstation' in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#'  
#'  No modifications are done to rows that have missing values for 'system' or 'area' (or 'location', depending on the parameter 'LocationVariable').
#'  Any rows with areas coded in another system than specified by the parameter 'System' will not be changed.
#'  
#'  When 'LocationVariable' is specified as 'None' area is looked up from 'AreaPosition', using the row where 'Location' is missing.
#'  When 'LocationVariable' is specified as 'location', 'area' and 'location' in 'BioticData' is looked up against 'Area' and 'Location' in 'AreaPosition'.
#'  
#' @param BioticData \code{\link[RstoxData]{BioticData}} data for which positions should be set
#' @param AreaPosition coordinates for Area and Location codes, see \code{\link[RstoxFDA]{AreaPosition}}
#' @param LocationVariable Specify which column in 'BioticData' should are represented by 'Location' in 'AreaPosition'. See details.
#' @param System identifies the area coding system used. Corresonds to the column 'system' on 'fishstation' in 'BioticData'.
#' @param Overwrite if True any existing values in 'latitudestart' and 'longitudestart' will be overwritten. If False postions with both latitude and longitude will be kept as they were.
#' @return \code{\link[RstoxData]{BioticData}}
#' @export
SetAreaPositionsBiotic <- function(BioticData, AreaPosition, LocationVariable = c("None", "location"), System=character(), Overwrite=F){
  
  if (!isGiven(System)){
    stop("Parameter 'System' must be provided.")
  }
  if (!isGiven(LocationVariable)){
    stop("Parameter 'LocationVariable' must be provided.")
  }
  
  LocationVariable <- match.arg(LocationVariable, LocationVariable)
  if (LocationVariable == "None"){
    AreaPosition <- AreaPosition[is.na(AreaPosition$Location)]
  }
  else if (LocationVariable == "location"){
    AreaPosition <- AreaPosition[!is.na(AreaPosition$Location)]
  }
  else{
    stop(paste("'LocationVariable", LocationVariable, "is not supported."))
  }
  
  for (file in names(BioticData)){
    #
    # should extract this to separate function
    #
    if ("fishstation" %in% names(BioticData[[file]]) &
        "system" %in% names(BioticData[[file]]$fishstation) &
        "area" %in% names(BioticData[[file]]$fishstation) &
        "location" %in% names(BioticData[[file]]$fishstation) &
        "latitudestart" %in% names(BioticData[[file]]$fishstation) &
        "longitudestart" %in% names(BioticData[[file]]$fishstation)){
      
      selection <- !is.na(BioticData[[file]]$fishstation$area) &
        !is.na(BioticData[[file]]$fishstation$system) &
        BioticData[[file]]$fishstation$system == System
      
      if (!Overwrite){
        selection <- selection &  
          (is.na(BioticData[[file]]$fishstation$latitudestart) | 
          is.na(BioticData[[file]]$fishstation$longitudestart))
      }
      if (LocationVariable == "location"){
        selection <- selection & 
          !is.na(BioticData[[file]]$fishstation$location)
      }
      
      #avoid checking if nothing is selected
      if (any(selection)){
        latitudes <- BioticData[[file]]$fishstation$latitudestart
        longitudes <- BioticData[[file]]$fishstation$longitudestart
        
        selectedAreas <- BioticData[[file]]$fishstation$area[selection]
        selectedLocations <- BioticData[[file]]$fishstation$location[selection]
        
        if (LocationVariable == "location"){
          combocodeData <- paste(selectedAreas, selectedLocations, sep="-")
          combocodeReference <- paste(AreaPosition$Area, AreaPosition$Location, sep="-")
          missing <- unique(combocodeData[!(combocodeData %in% combocodeReference)])
          if (length(missing)>0){
            stop(paste("Not all areas and locations in 'BioticData' are defined in 'AreaPosition. Missing:", paste(missing, collapse=",")))
          }
        }
        else if (!all(selectedAreas %in% AreaPosition$Area)){
          missing <- unique(selectedAreas[!(selectedAreas %in% AreaPosition$Area)])
          stop(paste("Not all areas in 'BioticData' are defined without Location in 'AreaPosition'. Missing:", paste(missing, collapse=",")))
        }
        
        if (LocationVariable == "None"){
          latitudes[selection] <- AreaPosition$Latitude[match(selectedAreas, AreaPosition$Area)]
          longitudes[selection] <- AreaPosition$Longitude[match(selectedAreas, AreaPosition$Area)]
        }
        else if (LocationVariable == "location"){
          latitudes[selection] <- AreaPosition$Latitude[match(paste(selectedAreas, selectedLocations), paste(AreaPosition$Area, AreaPosition$Location))]
          longitudes[selection] <- AreaPosition$Longitude[match(paste(selectedAreas, selectedLocations), paste(AreaPosition$Area, AreaPosition$Location))]
        }
        
        BioticData[[file]]$fishstation$latitudestart <- latitudes
        BioticData[[file]]$fishstation$longitudestart <- longitudes 
      }
    }
  }
  
  return(BioticData)
}

#'
#' @param dateColumns vector of date-columns to try in order.
#' @noRd
appendTemporal <- function(table, temporalColumn, temporalDefinition, datecolumns){
  stopifnot(is.TemporalDefinition(temporalDefinition))
  if (temporalColumn %in% names(table)){
    stop(paste("Temporal column", temporalColumn, "exists already."))
  }

  if (!(all(is.na(temporalDefinition$StartYear))) & any(is.na(temporalDefinition$StartYear))){
    stop("Year is provided for some, but not all temporal definitions.")
  }
  dateCol <- as.POSIXct(rep(NA, nrow(table)))

  for (d in datecolumns){
    if (!is.POSIXct(table[[d]])){
      stop("Error. Invalid date format. Use POSIXct.")
    }
    filter <- is.na(dateCol) & !is.na(table[[d]])
    dateCol[filter] <- table[[d]][filter]
  }

  if (any(is.na(dateCol))){
    stop("NA for some dates")
  }

  month <- as.integer(strftime(dateCol, format="%m"))
  day <- as.integer(strftime(dateCol, format="%d"))
  year <- as.integer(strftime(dateCol, format="%Y"))

  if (!(all(is.na(temporalDefinition$StartYear))) & !(all(year %in% temporalDefinition$StartYear))){
    stop("Year is provided in temporal definitions, but does not contain definitions for all years in data.")
  }

  temporalDefinition <- temporalDefinition[order(temporalDefinition$StartYear, temporalDefinition$StartMonth, temporalDefinition$StartDay, decreasing = F),]

  temporalCategory <- rep(NA, nrow(table))

  if (all(is.na(temporalDefinition$StartYear))){
    filt <- (month < temporalDefinition$StartMonth[1] | (month == temporalDefinition$StartMonth[1] & day < temporalDefinition$StartDay[1]))
    temporalCategory[filt] <- temporalDefinition$Period[nrow(temporalDefinition)]

    for (i in 1:nrow(temporalDefinition)){
      filt <- (month > temporalDefinition$StartMonth[i] | (month == temporalDefinition$StartMonth[i] & day >= temporalDefinition$StartDay[i]))
      temporalCategory[filt] <- temporalDefinition$Period[i]
    }

  }
  else if (all(!is.na(temporalDefinition$StartYear))){

    if (any(year < temporalDefinition$StartYear[1] |
            year == temporalDefinition$StartYear[1] & month < temporalDefinition$StartMonth[1] |
            (year == temporalDefinition$StartYear[1] & month == temporalDefinition$StartMonth[1] & day < temporalDefinition$StartDay[1]))){
      stop("Some dates preced the first temporal category.")
    }

    for (i in 1:nrow(temporalDefinition)){
      filt <- (year > temporalDefinition$StartYear[i] |
                 (year == temporalDefinition$StartYear[i] & month > temporalDefinition$StartMonth[i]) |
                 (year == temporalDefinition$StartYear[i] & month == temporalDefinition$StartMonth[i] & day >= temporalDefinition$StartDay[i]))
      temporalCategory[filt] <- temporalDefinition$Period[i]
    }
  }
  else{
    stop()
  }


  table[,temporalColumn] <- temporalCategory

  return(table)
}

#' Add Period to StoxLandingData
#' @description
#'  Add a column 'Period' to StoxLandingData with a temporal category, such as 'quarter'.
#' @details 
#'  Temporal definitions (\code{\link[RstoxFDA]{TemporalDefinition}}) may be produced by
#'  \code{\link[RstoxFDA]{DefinePeriod}}
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param TemporalDefinition \code{\link[RstoxFDA]{TemporalDefinition}} definiton of temporal category.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AddPeriodStoxLanding <- function(StoxLandingData, TemporalDefinition){
  
  columnName="Period"
  
  if (columnName %in% names(StoxLandingData)){
    stop(paste("The column", columnName, "already exists in StoxLandingData."))
  }
  if (any(is.na(StoxLandingData$landings$CatchDate))){
    stop("Cannot add period when 'CatchDate' is missing for some rows.")
  }
  
  
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.TemporalDefinition(TemporalDefinition))
  StoxLandingData$landings <- appendTemporal(StoxLandingData$landings, columnName, TemporalDefinition, "CatchDate")
  return(StoxLandingData)
}

#' Add Period to StoxBioticData
#' @description
#'  Add a column 'Period' to 'Station' on StoxBioticData with a temporal category, such as 'quarter'.
#' @details 
#'  Temporal definitions (\code{\link[RstoxFDA]{TemporalDefinition}}) may be produced by
#'  \code{\link[RstoxFDA]{DefinePeriod}}
#' @param StoxBioticData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param TemporalDefinition \code{\link[RstoxFDA]{TemporalDefinition}} definiton of temporal category.
#' @return StoxBioticData with column appended. See \code{\link[RstoxData]{StoxBioticData}}.
#' @export
AddPeriodStoxBiotic <- function(StoxBioticData, TemporalDefinition){
  columnName="Period"
  
  if (columnName %in% names(StoxBioticData$Station)){
    stop(paste("The column", columnName, "already exists in StoxLandingData."))
  }
  if (any(is.na(StoxBioticData$Station$DateTime))){
    stop("Cannot add period when 'DateTime' is missing for some rows.")
  }
  
  stopifnot(is.TemporalDefinition(TemporalDefinition))
  StoxBioticData$Station <- appendTemporal(StoxBioticData$Station, columnName, TemporalDefinition, "DateTime")
  return(StoxBioticData)
}


#' Adds Stratum to StoxLandingData
#' @description
#'  StoX function
#'  Adds a column to StoxLandingData with the spatial strata each row belongs to.
#' @details
#'  The strata are added to the new column 'Stratum'.
#'  \code{\link[RstoxData]{StoxLandingData}} does not contain columns for positions,
#'  these need to be added as columns 'Latitude' and 'Longitude' before calling this function.
#'  \code{\link[RstoxFDA]{AddAreaPositionStoxLanding}} may be used to append positions, based on area codes.
#' @seealso \code{\link[RstoxFDA]{AddAreaPositionStoxLanding}} for appending positions to \code{\link[RstoxData]{StoxLandingData}}.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated. Needs postions appended. See details.
#' @param StratumPolygon Definition of spatial strata. See \code{\link[RstoxBase]{StratumPolygon}}
#' @param columnName Name of the column that will be appended. Defaults to 'Stratum'.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AddStratumStoxLanding <- function(StoxLandingData, StratumPolygon){
  
  columnName <- "Stratum"    
  latColumn <- "Latitude"    
  lonColumn <- "Longitude"  
  
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  if (!(all(c(latColumn, lonColumn) %in% names(StoxLandingData$landings)))){
    stop(paste("Could not find columns:", latColumn, "and", lonColumn, "that should be added to StoxLandingData"))
  }
  if (columnName %in% names(StoxLandingData$landings)){
    stop(paste("Column name", columnName, "already exists."))
  }
  StoxLandingData$landings <- appendAreaCode(StoxLandingData$landings, StratumPolygon, latColumn, lonColumn, columnName)
  return(StoxLandingData)
}

#' Adds Stratum to StoxBioticData
#' @description
#'  Adds a column to StoxBioticData with the spatial strata each row belongs to.
#' @details 
#'  The strata are added to the new column 'Stratum'.
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated. Needs postions appended. See details.
#' @param StratumPolygon Definition of spatial strata. See \code{\link[RstoxBase]{StratumPolygon}}
#' @param columnName Name of the column that will be appended. Defaults to 'Stratum'.
#' @return StoxBioticData with column appended to data.table 'Station'. See \code{\link[RstoxData]{StoxBioticData}}.
#' @export
AddStratumStoxBiotic <- function(StoxBioticData, StratumPolygon){
  
  columnName <- "Stratum"    

  stopifnot("Station" %in% names(StoxBioticData))
  
  if (columnName %in% names(StoxBioticData$Station)){
    stop(paste("Column name", columnName, "already exists."))
  }
  StoxBioticData$Station <- appendAreaCode(StoxBioticData$Station, StratumPolygon, "Latitude", "Longitude", columnName)
  return(StoxBioticData)
}


#' append gear
#' @noRd
appendGear <- function(table, gearcolumn, gearDefinition, colName){

  if (length(unique(gearDefinition$Source)) > 1){
    stop("Error: filter gearDefinition before call")
  }

  if (colName %in% names(table)){
    stop(paste("Column with name '", colName, "' already exists.", sep=""))
  }
  conversionTable <- as.list(gearDefinition[[2]])
  names(conversionTable) <- gearDefinition[[1]]
  
  table[[colName]] <- convertCodes(unlist(table[,gearcolumn,with=F]), conversionTable)
  
  return(table)
}

###
# Functions for appending columns to data
#

#' Add Gear group to StoxLandingData
#' @description
#'  Adds a column to StoxLandingData with gear groups
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param Translation Translation table (\code{\link[RstoxData]{Translation}}) that maps the column 'Gear' in 'StoxLandingData' to a gear group.
#' @return StoxLandingData with column 'GearGroup' appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AddGearGroupStoxLanding <- function(StoxLandingData, Translation){
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  geardef <- Translation
  StoxLandingData$landings<-appendGear(StoxLandingData$landings, "Gear", geardef, "GearGroup")
  return(StoxLandingData)
}

#' Add Gear group to StoxBioticData
#' @description
#'  Adds a column to StoxBioticData with gear groups
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated.
#' @param Translation Translation table (\code{\link[RstoxData]{Translation}}) that maps the column 'Gear' in 'StoxLandingData' to a gear group.
#' @return StoxBioticData with column 'GearGroup' appended. See \code{\link[RstoxData]{StoxBioticData}}.
#' @export
AddGearGroupStoxBiotic <- function(StoxBioticData, Translation){
  geardef <- Translation
  StoxBioticData$Haul<-appendGear(StoxBioticData$Haul, "Gear", geardef, "GearGroup")
  return(StoxBioticData)
}

#' Set time Biotic
#' @description 
#'  Set start time to a fixed time for all stations.
#' @details 
#'  Set the column 'stationstarttime' on the table 'fishstation' in \code{\link[RstoxData]{BioticData}} to a fixed time
#'  'stationstarttime' is on the table 'fishstation' in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#'  Setting a fixed time to stationstarttime facilitates conversion to \code{\link[RstoxData]{StoxBioticData}} 
#'  using \code{\link[RstoxData]{StoxBiotic}}, and is applicable if hourly resolution of date and time is not necessary
#'  for subsequent analysis.
#' @param BioticData \code{\link[RstoxData]{BioticData}} data for which time should be set
#' @param Time character encoding time. Defaults to 12:00:00Z if not given, otherwise provide 
#'   UTC-time formatted as \code{\link[base]{strptime}}-string: \%H:\%M:\%SZ, e.g. 12:00:00Z
#' @param Overwrite if True any existing values in stationstarttime will be overwritten.
#' @return \code{\link[RstoxData]{BioticData}}
#' @export
SetTimeBiotic <- function(BioticData, Time=character(), OverWrite=F){
  if (!isGiven(Time)){
    Time="12:00:00Z"
  }
  
  timeobj <- as.POSIXlt(Time, format="%H:%M:%SZ")
  if (is.na(timeobj)){
    stop(paste("Invalid time specification: ", Time, ". Provide as %H:%M:%SZ, e.g: 12:00:00Z", sep=""))
  }
  
  for (file in names(BioticData)){
    if ("fishstation" %in% names(BioticData[[file]]) & "stationstarttime" %in% names(BioticData[[file]]$fishstation)){
      times <- BioticData[[file]]$fishstation$stationstarttime
      if (!OverWrite){
        times[is.na(times)] <- Time  
        BioticData[[file]]$fishstation$stationstarttime <- times
      }
      else{
        BioticData[[file]]$fishstation$stationstarttime <- Time
      }
    }
  }
  return(BioticData)
}

#' Set startdate Biotic
#' @description 
#'  Set start date from stop date. 
#' @details
#'  Set the column 'stationstartdate' on the table 'fishstation' in \code{\link[RstoxData]{BioticData}}
#'  to the value of 'stationstopdate'.
#'  These columns are on the table 'fishstation' in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#'  Setting stationstartdate facilitates conversion to \code{\link[RstoxData]{StoxBioticData}} 
#'  using \code{\link[RstoxData]{StoxBiotic}}, and is applicable if daily resolution of date and time is not critical
#'  for subsequent analysis.
#' @param BioticData \code{\link[RstoxData]{BioticData}} data for which time should be set
#' @param Overwrite if True any existing values in stationstartdate will be overwritten.
#' @return \code{\link[RstoxData]{BioticData}}
#' @export
SetStartDateBiotic <- function(BioticData, OverWrite=F){
 
  for (file in names(BioticData)){
    if ("fishstation" %in% names(BioticData[[file]]) & 
        "stationstartdate" %in% names(BioticData[[file]]$fishstation) &
        "stationstopdate" %in% names(BioticData[[file]]$fishstation)){
      dates <- BioticData[[file]]$fishstation$stationstartdate
      if (!OverWrite){
        dates[is.na(dates)] <- BioticData[[file]]$fishstation$stationstopdate[is.na(dates)]
        BioticData[[file]]$fishstation$stationstartdate <- dates
      }
      else{
        BioticData[[file]]$fishstation$stationstartdate <- BioticData[[file]]$fishstation$stationstopdate
      }
    }
  }
  return(BioticData) 
}

#' Define Periods
#' @description
#'  Define periods for grouping data based on date.
#' @details
#'  The 'TemporalCategory'-options 'Quarter' and 'Month' produce seasonal definitions.
#'  Seasonal definitions include dates
#'  based on day and month, irrespective of year. Seasonal definitions also wrap around so that 
#'  January is considered to follow December. 
#'  
#'  In order to make custom seasonal definitions,
#'  use the TemporalCategory'-option 'Custom' without providing years in 'CustomPeriods': "DD-MM".
#'  In order to make non-seasonal definitions for Quarter or Month, provide the as 'CustomPeriods'
#'  for all years of interest. If years are provided categories are automatically extended to the entire year, if necesesarry.
#'  That is a category starting with 1. Jan is added if not present, and likewise a category ending with 31. Dec.
#' @param ProcessData \code{\link[RstoxFDA]{TemporalDefinition}} as returned from this function
#' @param TemporalCategory type of temporal category: 'Quarter', 'Month' or 'Custom', defaults to 'Quarter'
#' @param CustomPeriods provided if temporalCategory is 'Custom', vector of strings formatted as DD-MM or DD-MM-YYYY, giving the start date of each temporal category.
#' @param UseProcessData Bypasses execution of function, if TRUE, and simply returns argument 'ProcessData'
#' @return Temporal Categories, see: \code{\link[RstoxFDA]{TemporalDefinition}}.
#' @export
DefinePeriod <- function(processData, TemporalCategory=c("Quarter", "Month", "Custom"), CustomPeriods = character(), UseProcessData=F){
  
  if (UseProcessData){
    return(processData)
  }
  
  TemporalCategory <- match.arg(TemporalCategory, TemporalCategory)

  if (length(CustomPeriods)>0 & TemporalCategory != "Custom"){
    stop(paste("Custom period provided, but 'TemporalCategory' is", TemporalCategory))
  }

  if (length(CustomPeriods)>0){
    if (length(CustomPeriods) == 1){
      stop("Need to provide at least two periods")
    }
    if (any(duplicated(CustomPeriods))){
      stop("Need to provide unique periods.")
    }
    CustomPeriods <- trimws(CustomPeriods)
    form <- function(x){
      if (nchar(x) != 5 & nchar(x) != 10){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' or 'DD-MM-YYYY' (startday)")
      }
      if (substr(x,3,3) != "-"){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' or 'DD-MM-YYYY' (startday)")
      }
      if (is.na(as.integer(substr(x,1,2))) | is.na(as.integer(substr(x,4,5)))){
        stop("Malformed custom period. All periods must be on the form 'DD-MM'or 'DD-MM-YYYY' (startday)")
      }
      if (is.na(as.integer(substr(x,1,2))) | as.integer(substr(x,1,2)) > 31 |
          is.na(as.integer(substr(x,1,2))) | as.integer(substr(x,1,2)) < 1 |
          is.na(as.integer(substr(x,4,5))) | as.integer(substr(x,4,5)) < 1 |
          is.na(as.integer(substr(x,4,5))) | as.integer(substr(x,4,5)) > 12){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' or 'DD-MM-YYYY' (startday)")
      }
      
      if (nchar(x) == 10 & is.na(as.integer(substr(x,7,10)))){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' or 'DD-MM-YYYY' (startday)")
      }
    }

    sapply(CustomPeriods, form)
  }
  
  if (TemporalCategory == "Month"){
    output <- data.table::data.table(Period=as.character(
        c("January", "Februrary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
        StartDay=as.integer(rep(1,12)),
        StartMonth=as.integer(seq(1,12)),
        StartYear=as.integer(rep(NA, 12))
    )
  }
  else if (TemporalCategory == "Quarter"){
    output <- data.table::data.table(Period=as.character(
      c("Q1", "Q2", "Q3", "Q4")),
      StartDay=as.integer(rep(1,4)),
      StartMonth=as.integer(c(1,4,7,10)),
      StartYear=as.integer(rep(NA, 4))
    )
  }
  else if (TemporalCategory == "Custom"){
    days <- as.integer(substr(CustomPeriods, 1,2))
    months <- as.integer(substr(CustomPeriods, 4,5))
    years <- as.integer(substr(CustomPeriods, 7,10))
    
    if (any(is.na(years)) & !all(is.na(years))){
      stop("Provide year (DD-MM-YYYY) for either all or none of the temporal categories in paramaeter 'CustomPeriods'.")
    }
    
    ord <- order(months, days, decreasing = F)
    if (all(!is.na(years))){
      ord <- order(years, months, days, decreasing = F)
    }
    months <- months[ord]
    days <- days[ord]
    years <- years[ord]
    CustomPeriods <- CustomPeriods[ord]
    
    #make span entire year, if years are provied
    if (all(!is.na(years))){
      if (days[1]!=1 | months[1]!=1){
        days <- c(1, days)
        months <- c(1, months)
        years <- c(min(years), years)
        CustomPeriods <- c(paste("01-01", min(years), sep="-"), CustomPeriods)
      }
      if (days[length(days)]!=31 | months[length(months)]!=12){
        days <- c(days, 31)
        months <- c(months, 12)
        years <- c(years, max(years))
        CustomPeriods <- c(CustomPeriods, paste("31-12", max(years), sep="-"))
      }
      
    }

    startstr <- CustomPeriods

    if (months[1]==1 & days[1] == 1 & is.na(years[1])){
        endstr <- c(CustomPeriods[2:length(CustomPeriods)], "-----")
    }
    else {
      endstr <- c(CustomPeriods[2:length(CustomPeriods)], CustomPeriods[1])
    }
    output <- data.table::data.table(Period=as.character(
      paste("[", startstr, ", ", endstr, ">", sep="")),
      StartDay=days,
      StartMonth=months,
      StartYear=years
    )
    
  }
  else{
    stop(paste("Temporal category", TemporalCategory, "not recognized."))
  }

  return(output)
}

#' Define Area Code Positions
#' @description
#'  StoX function
#'  Define positions for areas of a spatial coding system.
#' @details
#'  For DefinitionMethod ResourceFile:
#'  Definitions are read from a tab separated, UTF-8 encoded file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'Area'}{Area code (key)}
#'  \item{Column 2: 'Location'}{optional subdivision of area. If provided, positions for missing locations should be encoded as well.}
#'  \item{Column 3: 'Latitude'}{WGS84 Latitude, decimal degrees}
#'  \item{Column 4: 'Longitude'}{WGS84 Longitude, decimal degrees}
#'  }
#'  
#'  For DefinitionMethod StratumPolygon:
#'  Definitions are extracted from a \code{\link[RstoxBase]{StratumPolygon}}:
#'  Area is derived from the column 'polygonName' in 'StratumPolygon'.
#'  Location is encoded as missing.
#'  Latitude and Longitude are the coordinates set for each polygon in 'StratumPolygon'.
#'  
#' @param processData data.table() as returned from this function
#' @param DefinitionMethod 'ResourceFile' or 'StratumPolygon', see details.
#' @param FileName path to resource file
#' @param StratumPolygon \code{\link[RstoxBase]{StratumPolygon}} to extract area positions from
#' @param UseProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return \code{\link[RstoxFDA]{AreaPosition}}.
#' @export
DefineAreaPosition <- function(processData, DefinitionMethod=c("ResourceFile", "StratumPolygon"), FileName=character(), StratumPolygon, UseProcessData=F){

  DefinitionMethod <- match.arg(DefinitionMethod, DefinitionMethod)
  encoding="UTF-8"
  
  if (UseProcessData){
    return(processData)
  }

  if (DefinitionMethod == "ResourceFile"){
    tab <- readTabSepFile(FileName, col_types = "ccdd", col_names = c("Area", "Location",	"Latitude",	"Longitude"), encoding = encoding)
    
    missingLoc <- tab[is.na(tab[["Location"]]),]
    
    if (length(unique(missingLoc[["Area"]])) != length(unique(tab[["Area"]]))){
      stop("Malformed resource file. Some Area does not have coordinates defined for the case when location is missing.")
    }
    
    return(tab)
  }
  
  if (DefinitionMethod == "StratumPolygon"){
    if (!("polygonName" %in% names(StratumPolygon))){
      stop("'StratumPolygon' must be an RstoxBase::StratumPolygon object.")
    }
    pos <- data.table::data.table(sp::coordinates(StratumPolygon))
    names(pos) <- c("Longitude", "Latitude")
    
    stopifnot(nrow(pos)==nrow(StratumPolygon))
    
    pos$Area <- StratumPolygon$polygonName
    pos$Location <- as.character(NA)
    return(pos)
    
  }
  
  stop(paste("DefinitionMethod", DefinitionMethod, "not supported."))

}

#' Read CAR neighbours from file
#' @noRd
loadCarNeighboursFile <- function(FileName, encoding){
  tab <- readTabSepFile(FileName, col_types = "cc", col_names = c("CarValue", "Neighbours"), encoding = encoding)
  
  checkSymmetry(tab)
  
  if (length(unique(tab[["CarValue"]])) != nrow(tab)){
    d <- tab[["CarValue"]][duplicated(tab[["CarValue"]])]
    stop(paste("Malformed resource file, Non-unique keys: repition in first column:", paste(d, collapse = ",")))
  }
  
  return(tab)
}

#' Calculate CAR neighbours from stratum polygon
#' @noRd
calculateCarNeighbours <- function(StratumPolygon){
  sfpoly <- sf::st_as_sf(StratumPolygon)
  neighbourIndecies <- sf::st_touches(sfpoly, sfpoly)
  
  carValues <- sfpoly$polygonName
  neighbours <- unlist(lapply(neighbourIndecies, function(x){paste(sfpoly$polygonName[x],collapse=",")}))
  
  carTable <- data.table::data.table(CarValues=carValues, Neighbours=neighbours)
  
  return(carTable)
}

#' Define CAR neighbours
#' @description
#'  Define which spatial strata are to be considered neighbours,
#'  when used as a CAR-variable (Conditional AutoRegressive variable).
#' @details
#'  For DefinitionMethod 'ResourceFile':
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'CarValue'}{Value for the CAR-variable (key)}
#'  \item{Column 2: 'Neigbhours'}{Comma-separated list of neighbours (each should occur in Column 1)}
#'  }
#'  The neighbour definition must be symmetric.
#'  If a is among the neighbours of b, b must also be among the neighbours of a.
#'  For DefinitionMethod 'StratumPolygon':
#'  A \code{\link[RstoxFDA]{CarNeighbours}} table will be calculated from the provided 'StratumPolygon'
#'  runing time and correctness of calcuation may depend on the quality and complexity of the 'StratumPolygon'.
#' @param processData data.table() as returned from this function
#' @param DefinitionMethod 'ResourceFile' or 'StratumPolygon'
#' @param FileName path to file for resource 
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return Area Neighbour Definition, see: \code{\link[RstoxFDA]{CarNeighbours}}.
#' @export
DefineCarNeighbours <- function(processData,
                                DefinitionMethod = c("ResourceFile", "StratumPolygon"), 
                                FileName, StratumPolygon, UseProcessData = FALSE){
  if (UseProcessData){
    return(processData)
  }

  DefinitionMethod <- match.arg(DefinitionMethod, DefinitionMethod)
  
  if (DefinitionMethod == "ResourceFile"){
    encoding = "UTF-8"
    return(loadCarNeighboursFile(FileName, encoding))
  }
  
  else if (DefinitionMethod == "StratumPolygon"){
    return(calculateCarNeighbours(StratumPolygon))
  }
  
  else{
    stop(paste("DefinitionMethod", DefinitionMethod, "is not supported."))
  }
  
  
}

#' Define Age Error Matrix
#' @description
#'  StoX function.
#'  Defines probabilities for misreading ages.
#' @details
#'  Definitions are read from a tab separated file with headers and row names in first column.
#'  All row and column names should be integers.
#'  The matrix encodes the probability of observing an age (rows), given true age (columns).
#'  Columns must sum to 1.
#' @param processData data.table() as returned from this function
#' @param FileName path to resource file
#' @param encoding encoding of resource file
#' @param useProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return Age Error Matrix, see: \code{\link[RstoxFDA]{AgeErrorMatrix}}.
#' @export
DefineAgeErrorMatrix <- function(processData, DefinitionMethod=c("ResourceFile"), FileName, UseProcessData=F){
  
  encoding="UTF-8"

  if (UseProcessData){
    return(processData)
  }

  stream <- file(FileName, open="r")
  matrixNoHeader <- utils::read.delim(stream, sep="\t", header=F, encoding = encoding)
  close(stream)

  stream <- file(FileName, open="r")
  matrix <- utils::read.delim(stream, sep="\t", header=T, row.names = 1, encoding = encoding)
  close(stream)

  coln <- as.character(matrixNoHeader[1,2:ncol(matrixNoHeader)])
  dt <- data.table::data.table(matrix)

  colnames(dt) <- coln
  dt$ReadAge <- rownames(matrix)

  if (!all(colSums(matrix) == 1)){
    stop("Malformed resource file. Columns must sum to 1.")
  }

  if (any(matrix < 0) | any(matrix > 1)){
    stop("Malformed resource file. All probabilities must be in >=0 and <=1.")
  }

  return(dt)
}

#' Define Classification Error (for stock splitting)
#' @description
#'  StoX function.
#'  Defines probabilities for misclassifying when determining stock membership of a specimen.
#' @details
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'   \item{Column 1 : ptype1.CC}{Probability of classifying a type 1 specimen as type 1.}
#'   \item{Column 2: ptype1.S}{Probability of classifying a type 5 specimen as type 1.}
#'   \item{Column 3: ptype2.CC}{Probability of classifying a type 2 specimen as type 2.}
#'   \item{Column 4: ptype2.S}{Probability of classifying a type 4 specimen as type 2.}
#'   \item{Column 5: ptype4.CC}{Probability of classifying a type 2 specimen as type 4.}
#'   \item{Column 6: ptype4.S}{Probability of classifying a type 4 specimen as type 4.}
#'   \item{Column 7: ptype5.CC}{Probability of classifying a type 1 specimen as type 5.}
#'   \item{Column 8: ptype5.S}{Probability of classifying a type 5 specimen as type 5.}
#'  }
#'  see \code{\link[RstoxFDA]{ClassificationError}} for further explanation on the coding system.
#' @param processData data.table() as returned from this function
#' @param resourceFilePath path to resource file
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Classification Error Matrix, see: \code{\link[RstoxFDA]{ClassificationError}}.
#' @export
DefineClassificationError<- function(processData, resourceFilePath, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  tab <- readTabSepFile(resourceFilePath,
                        col_types = "dddddddd",
                        col_names = c("ptype1.CC", "ptype1.S", "ptype2.CC", "ptype2.S", "ptype4.CC", "ptype4.S", "ptype5.CC", "ptype5.S"),
                        encoding = encoding)

  if (nrow((tab)) != 1){
    stop("Malformed resource file: contains more than one row.")
  }

  return(tab)
}

