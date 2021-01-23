
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
#'  When 'LocationCol' is specified as 'None' Area is looked up from 'AreaPosition', using the row where 'Location' is missing.
#'  When 'LocationCol' is specified as 'Location', 'Area' and 'Location' in 'StoxLandingData' is looked up against 'Area' and 'Location' in 'AreaPosition'.
#'  When 'LocationCol' is specified as 'Coastal', 'Area' and 'Costal' in 'StoxLandingData' is looked up against 'Area' and 'Location' in 'AreaPosition'.
#' @param StoxLandingData landing data, see \code{\link[RstoxData]{StoxLandingData}}
#' @param AreaPosition coordinates for Area and SubArea codes, see \code{\link[RstoxFDA]{AreaPosition}}
#' @param LocationCol Specify which column in 'StoxLandingsData' should are represented by 'Location' in 'AreaPosition'. See details.
#' @return \code{\link[RstoxData]{StoxLandingData}} with columns for latitude and longitude appended.
#' @export
AddAreaPositionStoxLanding <- function(StoxLandingData, AreaPosition, LocationCol = c("None", "Location", "Coastal")){
  
  LocationCol <- match.arg(LocationCol)
  
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

  if (LocationCol == "None"){
    
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
  else if (LocationCol %in% c("Location", "Coastal")){
    
    arealocdata <- paste(StoxLandingData$landings$Area, StoxLandingData$landings[[LocationCol]], sep="-")
    arealocresource <- paste(AreaPosition$Area, AreaPosition$Location, sep="-")
    if (!all(arealocdata %in% arealocresource)){
      missing <- arealocdata[!(arealocdata %in% arealocresource)]
      stop(paste("Positions not provided for all Areas and Locations in StoxLandingData Missing: ", paste(missing, collapse=",")))
    }
    AreaPosition <- AreaPosition[,c("Area", "Location", latColName, lonColName), with=F]
    stopifnot(!any(duplicated(paste(AreaPosition$Area, AreaPosition$Location))))
    StoxLandingData$landings <- data.table::as.data.table(merge(StoxLandingData$landings, AreaPosition, by.x=c("Area", LocationCol), by.y=c("Area", "Location"), all.x=T))
    return(StoxLandingData)
  }
  else{
    stop(paste("Parameter", LocationCol, "is not supported for 'LocationCol'."))
  }

}

#'
#' @param dateColumns vector of date-columns to try in order.
#' @noRd
appendTemporal <- function(table, temporalColumn, temporalDefinition, datecolumns){
  stopifnot(is.TemporalDefinition(temporalDefinition))

  if (temporalColumn %in% names(table)){
    stop(paste("Temporal column", temporalColumn, "exists already."))
  }

  if (!(all(is.na(temporalDefinition$year))) & any(is.na(temporalDefinition$year))){
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

  if (!(all(is.na(temporalDefinition$year))) & !(all(year %in% temporalDefinition$year))){
    stop("Year is provided in temporal definitions, but does not contain definitions for all years in data.")
  }

  temporalDefinition <- temporalDefinition[order(temporalDefinition$year, temporalDefinition$startMonth, temporalDefinition$startDay, decreasing = F),]

  temporalCategory <- rep(NA, nrow(table))

  if (all(is.na(temporalDefinition$year))){
    filt <- (month < temporalDefinition$startMonth[1] | (month == temporalDefinition$startMonth[1] & day < temporalDefinition$startDay[1]))
    temporalCategory[filt] <- temporalDefinition$temporalCategory[nrow(temporalDefinition)]

    for (i in 1:nrow(temporalDefinition)){
      filt <- (month > temporalDefinition$startMonth[i] | (month == temporalDefinition$startMonth[i] & day >= temporalDefinition$startDay[i]))
      temporalCategory[filt] <- temporalDefinition$temporalCategory[i]
    }

  }
  else if (all(!is.na(temporalDefinition$year))){

    if (any(year < temporalDefinition$year[1] |
            year == temporalDefinition$year[1] & month < temporalDefinition$startMonth[1] |
            (year == temporalDefinition$year[1] & month == temporalDefinition$startMonth[1] & day < temporalDefinition$startDay[1]))){
      stop("Some dates preced the first temporal category.")
    }

    for (i in 1:nrow(temporalDefinition)){
      filt <- (year > temporalDefinition$year[i] |
                 (year == temporalDefinition$year[i] & month > temporalDefinition$startMonth[i]) |
                 (year == temporalDefinition$year[i] & month == temporalDefinition$startMonth[i] & day >= temporalDefinition$startDay[i]))
      temporalCategory[filt] <- temporalDefinition$temporalCategory[i]
    }
  }
  else{
    stop()
  }


  table[,temporalColumn] <- temporalCategory

  return(table)
}

#' Append Temporal Categories to StoxLandingData
#' @description
#'  StoX function
#'  Appends a column to StoxLandingData with a temporal category, such as 'quarter',
#'  that are also defined for for other formats, such as StoxBioticData.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param TemporalDefinition \code{\link[RstoxFDA]{TemporalDefinition}} definiton of temporal category.
#' @param columnName character(), defaults to 'TemporalCategory', name of the appended column.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AppendTemporalStoxLanding <- function(StoxLandingData, TemporalDefinition, columnName="TemporalCategory"){
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.TemporalDefinition(TemporalDefinition))
  return(appendTemporal(StoxLandingData$landings, columnName, TemporalDefinition, "CatchDate"))
}


AppendTemporalStoxBiotic <- function(StoxBioticData, TemporalDefinition, columnName="TemporalCategory"){
  stop("Not implemented")
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

  conversionTable <- makeUnifiedDefinitionLookupList(gearDefinition)[[1]]
  table[,colName] <- convertCodes(unlist(table[,gearcolumn,with=F]), conversionTable)

  return(table)
}

###
# Functions for appending columns to data
#

#' Append Gear to StoxBioticData
#' @description
#'  StoX function
#'  Appends a column to StoxBioticData with a unified gear definition
#'  that are also defined for for other formats, such as StoxLandingData.
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated.
#' @param UnifiedVariableDefinition \code{\link[RstoxFDA]{UnifiedVariableDefinition}} unified gear definition.
#' @param columnName character(), defaults to 'UnifiedGear', name of the appended column.
#' @return StoxBioticData with column appended. See \code{\link[RstoxData]{StoxBioticData}}.
#' @export
AppendGearStoxBiotic <- function(StoxBioticData, UnifiedVariableDefinition, columnName="UnifiedGear"){
  stopifnot(is.UnifiedVariableDefinition(UnifiedVariableDefinition))
  geardef <- UnifiedVariableDefinition[UnifiedVariableDefinition$Source == "StoxBioticData",]
  return(appendGear(StoxBioticData, "gear", geardef, columnName))
}

#' Append Gear to StoxLandingData
#' @description
#'  StoX function
#'  Appends a column to StoxLandingData with a unified gear definition
#'  that are also defined for for other formats, such as StoxBioticData.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param UnifiedVariableDefinition \code{\link[RstoxFDA]{UnifiedVariableDefinition}} unified gear definition.
#' @param columnName character(), defaults to 'UnifiedGear', name of the appended column.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AppendGearStoxLanding <- function(StoxLandingData, UnifiedVariableDefinition, columnName="UnifiedGear"){
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.UnifiedVariableDefinition(UnifiedVariableDefinition))
  geardef <- UnifiedVariableDefinition[UnifiedVariableDefinition$Source == "StoxLandingData",]
  StoxLandingData$landings<-appendGear(StoxLandingData$landings, "Gear", geardef, columnName)
  return(StoxLandingData)
}

###
# Functions for defining resources, typically processData
#



#' Define Temporal Categories
#' @description
#'  StoX function
#'  Define temporal categories for grouping data based on date.
#' @details
#'  Not providing years, has the effect of making the defintion seasonal, independent of year,
#'  so that e.g. Q1 in 2015 is considered the same category as Q1 in 2016
#' @param processData data.table() as returned from this function
#' @param temporalCategory character(), defaults to 'Quarter', type of temporal category: 'Quarter', 'Month' or 'Custom'
#' @param customPeriods character(), provided if temporalCategory is 'Custom', vector of strings formatted as DD-MM, giving the start date of each temporal category.
#' @param years integer() vector, optional, provide if defintion should be non-seasonal.
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Temporal Categories, see: \code{\link[RstoxFDA]{TemporalDefinition}}.
#' @export
DefineTemporalCategories <- function(processData, temporalCategory=c("Quarter", "Month", "Custom"), customPeriods = NULL,  years = NULL, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  temporalCategory <- match.arg(temporalCategory, temporalCategory)

  if (length(customPeriods)>0 & temporalCategory != "Custom"){
    stop(paste("Custom period provided, but temporalCategory is", temporalCategory))
  }

  if (length(customPeriods)>0){
    if (length(customPeriods) == 1){
      stop("Need to provide at least two periods")
    }
    if (any(duplicated(customPeriods))){
      stop("Need to provide unique periods.")
    }
    customPeriods <- trimws(customPeriods)
    form <- function(x){
      if (nchar(x) != 5){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
      if (substr(x,3,3) != "-"){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
      if (is.na(as.integer(substr(x,1,2))) | is.na(as.integer(substr(x,4,5)))){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
      if (as.integer(substr(x,1,2)) > 31 |
          as.integer(substr(x,1,2)) < 1 |
          as.integer(substr(x,4,5)) < 1 |
          as.integer(substr(x,4,5)) > 12){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
    }

    sapply(customPeriods, form)
  }

  if (temporalCategory == "Month"){
    output <- data.table::data.table(temporalCategory=as.character(
        c("January", "Februrary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
        startDay=as.integer(rep(1,12)),
        startMonth=as.integer(seq(1,12)),
        year=as.integer(rep(NA, 12))
    )
  }
  else if (temporalCategory == "Quarter"){
    output <- data.table::data.table(temporalCategory=as.character(
      c("Q1", "Q2", "Q3", "Q4")),
      startDay=as.integer(rep(1,4)),
      startMonth=as.integer(c(1,4,7,10)),
      year=as.integer(rep(NA, 4))
    )
  }
  else if (temporalCategory == "Custom"){
    days <- as.integer(substr(customPeriods, 1,2))
    months <- as.integer(substr(customPeriods, 4,5))

    ord <- order(months, days, decreasing = F)
    months <- months[ord]
    days <- days[ord]

    startstr <- customPeriods


    if (months[1]==1 & days[1] == 1){
        endstr <- c(customPeriods[2:length(customPeriods)], "-----")
    }
    else {
      endstr <- c(customPeriods[2:length(customPeriods)], customPeriods[1])
    }

    output <- data.table::data.table(temporalCategory=as.character(
      paste("[", startstr, ", ", endstr, ">", sep="")),
      startDay=days,
      startMonth=months,
      year=as.integer(rep(NA, length(days)))
    )
  }
  else{
    stop(paste("Temporal category", temporalCategory, "not recognized."))
  }

  if (length(years)>0){
    ncat <- nrow(output)
    out <- output
    out$year <- rep(years[1], ncat)
    out$temporalCategory <- paste(years[1], " ", output$temporalCategory, sep="")
    yearoutput <- out
    output <- yearoutput
  }
  if (length(years)>1){
    for (i in 1:(length(years)-1)){
      y<-i+1
      out <- output
      out$year <- rep(years[y], ncat)
      out$temporalCategory <- paste(years[y], " ", output$temporalCategory, sep="")
      yearoutput <- rbind(yearoutput, out)
    }
    output <- yearoutput
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
DefineAreaPosition <- function(processData, DefinitionMethod=c("ResourceFile", "StratumPolygon"), FileName=NULL, StratumPolygon=NULL, UseProcessData=F){

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

#' Define CAR neighbours
#' @description
#'  StoX function.
#'  Define which spatial strata are to be considered neighbours,
#'  when used as a CAR-variable (Conditional AutoRegressive variable).
#' @details
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'CarValue'}{Value for the CAR-variable (key)}
#'  \item{Column 2: 'Neigbhours'}{Comma-separated list of neighbours (each should occur in Column 1)}
#'  }
#'  The neighbour definition must be symmetric.
#'  If a is among the neighbours of b, b must also be among the neighbours of a.
#' @param processData data.table() as returned from this function
#' @param DefinitionMethod ResourceFile
#' @param FileName path to file for resource 
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return Area Neighbour Definition, see: \code{\link[RstoxFDA]{CarNeighbours}}.
#' @export
DefineCarNeighbours <- function(processData,
                                DefinitionMethod = c("ResourceFile"), 
                                FileName, UseProcessData = FALSE){
  encoding = "UTF-8"
  if (UseProcessData){
    return(processData)
  }

  tab <- readTabSepFile(FileName, col_types = "cc", col_names = c("CarValue", "Neighbours"), encoding = encoding)

  checkSymmetry(tab)

  if (length(unique(tab[["CarValue"]])) != nrow(tab)){
    d <- tab[["CarValue"]][duplicated(tab[["CarValue"]])]
    stop(paste("Malformed resource file, Non-unique keys: repition in first column:", paste(d, collapse = ",")))
  }

  return(tab)
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

