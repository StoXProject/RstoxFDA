
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

  if (latColName %in% names(StoxLandingData$Landing)){
    stop(paste("Column", latColName, "already exists."))
  }
  if (lonColName %in% names(StoxLandingData$Landing)){
    stop(paste("Column", lonColName, "already exists."))
  }

  AreaPosition[[latColName]] <- AreaPosition$Latitude
  AreaPosition[[lonColName]] <- AreaPosition$Longitude

  if (LocationVariable == "None"){
    
    if (!all(StoxLandingData$Landing$Area %in% AreaPosition$Area)){
      missing <- StoxLandingData$Landing$Area[!(StoxLandingData$Landing$Area %in% AreaPosition$Area)]
      stop(paste("Positions not provided for all Areas in StoxLandingData. Missing: ", paste(missing, collapse=",")))
    }
    if (!all(StoxLandingData$Landing$Area %in% AreaPosition$Area[is.na(AreaPosition$Location)])){
      missing <- StoxLandingData$Landing$Area[!(StoxLandingData$Landing$Area %in% AreaPosition$Area[is.na(AreaPosition$Location)])]
      stop(paste("Positions is not provided for the case of missing Location for some Areas in StoxLandingData: ", paste(missing, collapse=",")))
    }
    AreaPosition <- AreaPosition[is.na(AreaPosition$Location),c("Area", latColName, lonColName), with=F]
    stopifnot(!any(duplicated(AreaPosition$Area)))
    
    StoxLandingData$Landing <- data.table::as.data.table(merge(StoxLandingData$Landing, AreaPosition, by.x="Area", by.y="Area", all.x=T))
    return(StoxLandingData)
  }
  else if (LocationVariable %in% c("Location", "Coastal")){
    
    arealocdata <- paste(StoxLandingData$Landing$Area, StoxLandingData$Landing[[LocationVariable]], sep="-")
    arealocresource <- paste(AreaPosition$Area, AreaPosition$Location, sep="-")
    if (!all(arealocdata %in% arealocresource)){
      missing <- arealocdata[!(arealocdata %in% arealocresource)]
      stop(paste("Positions not provided for all Areas and Locations in StoxLandingData Missing: ", paste(missing, collapse=",")))
    }
    AreaPosition <- AreaPosition[,c("Area", "Location", latColName, lonColName), with=F]
    stopifnot(!any(duplicated(paste(AreaPosition$Area, AreaPosition$Location))))
    StoxLandingData$Landing <- data.table::as.data.table(merge(StoxLandingData$Landing, AreaPosition, by.x=c("Area", LocationVariable), by.y=c("Area", "Location"), all.x=T))
    return(StoxLandingData)
  }
  else{
    stop(paste("Parameter", LocationVariable, "is not supported for 'LocationVariable'."))
  }

}

#' Convert weights Biotic
#' @description 
#'  Convert weights to round weights.
#' @details 
#'  Different products of fish may be measured and round weight (live weight)
#'  or some other product type may be approximated by multiplying with a scalar conversion factor.
#'  
#'  For records where the variable 'catchcategory' on the table 'catchsample' matches the 'Species' variable in 'Translation'
#'  this function converts weights to desired product type ('TargetProductType') by matching 'producttype'-variables to 'ProductType' in 'Translation'.
#'  The following weights are converted:
#'  \itemize{
#'   \item{'catchweight' based on 'catchproducttype' both on the table 'catchsample'}
#'   \item{'lengthsampleweight' based on 'sampleproducttype' both on the table 'catchsample'}
#'   \item{'individualweight' based on 'individualproducttype' both on the table 'individual'} 
#'  }
#'  
#'  After conversion the 'producttype'-variables will be changed to reflect the approximate weight.
#'  That is they will be set to 'TargetProductType'.
#'  
#'  If 'WeightConversionTable' contains NAs for 'WeightFactor', converted weights will be NA.
#'  This is useful for dealing with species whoose weights are not needed in subsequent analysis. 
#'  
#'  The variables 'catchvolume', 'lengthsamplevolume', and 'individualvolume'
#'  are also dependent on product type. These are set to NA as weight is converted.
#'  
#'  These variables are on these tables in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#' @param BioticData \code{\link[RstoxData]{BioticData}} for which weights should be converted
#' @param WeightConversionTable \code{\link[RstoxFDA]{WeightConversionTable}} with parameters for converting weights to 'TargetProductType'.
#' @param TargetProductType The desired producttype. Typically the code for Round fish.
#' @return \code{\link[RstoxData]{BioticData}} with converted weights.
#' @export
ConvertWeightsBiotic <- function(BioticData, WeightConversionTable, TargetProductType=character()){
  
  if (!isGiven(TargetProductType)){
    stop("'TargetProductType' must be provided.")
  }
  if (!is.WeightConversionTable(WeightConversionTable)){
    stop("invalid WeightConversionTable")
  }
  
  for (file in names(BioticData)){
    if ("catchsample" %in% names(BioticData[[file]]) &
        "individual" %in% names(BioticData[[file]]) &
        "catchcategory" %in% names(BioticData[[file]]$catchsample) &
        "catchweight" %in% names(BioticData[[file]]$catchsample) &
        "catchproducttype" %in% names(BioticData[[file]]$catchsample) &
        "lengthsampleweight" %in% names(BioticData[[file]]$catchsample) &
        "sampleproducttype" %in% names(BioticData[[file]]$catchsample) &
        "individualweight" %in% names(BioticData[[file]]$individual) &
        "individualproducttype" %in% names(BioticData[[file]]$individual)){
      
      
      speciesInData <- unique(BioticData[[file]]$catchsample$catchcategory)
      missing <- speciesInData[!(speciesInData %in% WeightConversionTable$Species)]
      if (length(missing)>0){
        stop(paste("No conversion factors found for species:", paste(missing, collapse=",")))  
      }
      
      #set producttype and volume for any missing weights to NA
      BioticData[[file]]$catchsample$catchproducttype[is.na(BioticData[[file]]$catchsample$catchweight) & !is.na(BioticData[[file]]$catchsample$catchproducttype)] <- TargetProductType
      BioticData[[file]]$catchsample$sampleproducttype[is.na(BioticData[[file]]$catchsample$lengthsampleweight) & !is.na(BioticData[[file]]$catchsample$sampleproducttype)] <- TargetProductType
      BioticData[[file]]$individual$individualproducttype[is.na(BioticData[[file]]$individual$individualweight) & !is.na(BioticData[[file]]$individual$individualproducttype)] <- TargetProductType
      
      for (species in speciesInData){
        spectab <- WeightConversionTable[WeightConversionTable$Species == species,]
        
        #convert catchweight
        conversionCatch <- spectab$WeightFactor[match(BioticData[[file]]$catchsample$catchproducttype, spectab$ProductType)]
        filterCatchSample <- BioticData[[file]]$catchsample$catchcategory == species & 
          BioticData[[file]]$catchsample$catchproducttype != TargetProductType &
          !is.na(BioticData[[file]]$catchsample$catchweight)
        
        if (!all(BioticData[[file]]$catchsample$catchproducttype[filterCatchSample] %in% spectab$ProductType)){
          missing <- unique(BioticData[[file]]$catchsample$catchproducttype[filterCatchSample][!(BioticData[[file]]$catchsample$catchproducttype[filterCatchSample] %in% spectab$ProductType)])
          stop(paste("Not all necessary conversion factors found for species ", species, ". Missing: ", paste(missing, collapse=","), sep=""))
        }
        
        BioticData[[file]]$catchsample$catchweight[filterCatchSample] <- BioticData[[file]]$catchsample$catchweight[filterCatchSample] * conversionCatch[filterCatchSample]
        BioticData[[file]]$catchsample$catchproducttype[filterCatchSample] <- TargetProductType
        BioticData[[file]]$catchsample$catchvolume[filterCatchSample] <- NA
          
        #convert sampleweight
        conversionSample <- spectab$WeightFactor[match(BioticData[[file]]$catchsample$sampleproducttype, spectab$ProductType)]
        filterCatchSample <- BioticData[[file]]$catchsample$catchcategory == species & 
          BioticData[[file]]$catchsample$sampleproducttype != TargetProductType &
          !is.na(BioticData[[file]]$catchsample$lengthsampleweight)
        
        if (!all(BioticData[[file]]$catchsample$sampleproducttype[filterCatchSample] %in% spectab$ProductType)){
          missing <- unique(BioticData[[file]]$catchsample$sampleproducttype[filterCatchSample][!(BioticData[[file]]$catchsample$sampleproducttype[filterCatchSample] %in% spectab$ProductType)])
          stop(paste("Not all necessary conversion factors found for species ", species, ". Missing: ", paste(missing, collapse=","), sep=""))
        }
        
        BioticData[[file]]$catchsample$lengthsampleweight[filterCatchSample] <- BioticData[[file]]$catchsample$lengthsampleweight[filterCatchSample] * conversionSample[filterCatchSample]
        BioticData[[file]]$catchsample$sampleproducttype[filterCatchSample] <- TargetProductType
        BioticData[[file]]$catchsample$lengthsamplevolume[filterCatchSample] <- NA
        
        #locate individuals of the species    
        catchKeys <- names(BioticData[[file]]$catchsample)[names(BioticData[[file]]$catchsample) %in% names(BioticData[[file]]$individual)]
        catchIds <- Reduce(BioticData[[file]]$catchsample[,catchKeys, with=F], f = paste)
        catchIds <- catchIds[BioticData[[file]]$catchsample$catchcategory == species]
        indCatchIds <- Reduce(BioticData[[file]]$individual[,catchKeys, with=F], f = paste)
        
        #convert individual weight
        filterIndividual <- indCatchIds %in% catchIds & 
          BioticData[[file]]$individual$individualproducttype != TargetProductType &
          !is.na(BioticData[[file]]$individual$individualweight)
        
        if (!all(BioticData[[file]]$individual$individualproducttype[filterIndividual] %in% spectab$ProductType)){
          missing <- unique(BioticData[[file]]$individual$individualproducttype[filterIndividual][!(BioticData[[file]]$individual$individualproducttype[filterIndividual] %in% spectab$ProductType)])
          stop(paste("Not all necessary conversion factors found for species ", species, ". Missing: ", paste(missing, collapse=","), sep=""))
        }
        
        conversionInd <- spectab$WeightFactor[match(BioticData[[file]]$individual$individualproducttype, spectab$ProductType)]
        BioticData[[file]]$individual$individualweight[filterIndividual] <- BioticData[[file]]$individual$individualweight[filterIndividual] * conversionInd[filterIndividual]
        BioticData[[file]]$individual$individualproducttype[filterIndividual] <- TargetProductType
        BioticData[[file]]$individual$individualvolume[filterIndividual] <- NA
      }
      
    }
  }
 return(BioticData) 
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
  if (any(is.na(StoxLandingData$Landing$CatchDate))){
    stop("Cannot add period when 'CatchDate' is missing for some rows.")
  }
  
  
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.TemporalDefinition(TemporalDefinition))
  StoxLandingData$Landing <- appendTemporal(StoxLandingData$Landing, columnName, TemporalDefinition, "CatchDate")
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
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AddStratumStoxLanding <- function(StoxLandingData, StratumPolygon){
  
  columnName <- "Stratum"    
  latColumn <- "Latitude"    
  lonColumn <- "Longitude"  
  
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  if (!(all(c(latColumn, lonColumn) %in% names(StoxLandingData$Landing)))){
    stop(paste("Could not find columns:", latColumn, "and", lonColumn, "that should be added to StoxLandingData"))
  }
  if (columnName %in% names(StoxLandingData$Landing)){
    stop(paste("Column name", columnName, "already exists."))
  }
  StoxLandingData$Landing <- appendAreaCode(StoxLandingData$Landing, StratumPolygon, latColumn, lonColumn, columnName)
  return(StoxLandingData)
}

#' Adds Stratum to StoxBioticData
#' @description
#'  Adds a column to StoxBioticData with the spatial strata each row belongs to.
#' @details 
#'  The strata are added to the new column 'Stratum'.
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated. Needs postions appended. See details.
#' @param StratumPolygon Definition of spatial strata. See \code{\link[RstoxBase]{StratumPolygon}}
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
#' @details 
#'  The provided Translation should maps values ('Value' in Translation) for the variable 'Gear' ('VariableName' in Translation) in 'StoxBioticData' to a gear group ('NewValue').
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param Translation Translation table (\code{\link[RstoxData]{Translation}}). See details.
#' @return StoxLandingData with column 'GearGroup' appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AddGearGroupStoxLanding <- function(StoxLandingData, Translation){
  if (!is.Translation(Translation)){
    stop("Translation is not a valid Translation table.")
  }
  if (!is.Translation(Translation)){
    stop("Translation is not a valid Translation table.")
  }
  if (!any(Translation$VariableName=="Gear")){
    stop("Translation table contains no translation for variable 'Gear'")
  }
  geardef <- Translation[Translation$VariableName=="Gear",c("Value", "NewValue")]
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  StoxLandingData$Landing<-appendGear(StoxLandingData$Landing, "Gear", geardef, "GearGroup")
  return(StoxLandingData)
}

#' Add Gear group to StoxBioticData
#' @description
#'  Adds a column to StoxBioticData with gear groups
#' @details 
#'  The provided Translation should maps values ('Value' in Translation) for the variable 'Gear' ('VariableName' in Translation) in 'StoxBioticData' to a gear group ('NewValue').
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated.
#' @param Translation Translation table (\code{\link[RstoxData]{Translation}}). See details.
#' @return StoxBioticData with column 'GearGroup' appended. See \code{\link[RstoxData]{StoxBioticData}}.
#' @export
AddGearGroupStoxBiotic <- function(StoxBioticData, Translation){
  if (!is.Translation(Translation)){
    stop("Translation is not a valid Translation table.")
  }
  if (!any(Translation$VariableName=="Gear")){
    stop("Translation table contains no translation for variable 'Gear'")
  }
  geardef <- Translation[Translation$VariableName=="Gear",c("Value", "NewValue")]
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
SetTimeBiotic <- function(BioticData, Time=character(), Overwrite=F){
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
      if (!Overwrite){
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
SetStartDateBiotic <- function(BioticData, Overwrite=F){
 
  for (file in names(BioticData)){
    if ("fishstation" %in% names(BioticData[[file]]) & 
        "stationstartdate" %in% names(BioticData[[file]]$fishstation) &
        "stationstopdate" %in% names(BioticData[[file]]$fishstation)){
      dates <- BioticData[[file]]$fishstation$stationstartdate
      if (!Overwrite){
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
#' @param processData \code{\link[RstoxFDA]{TemporalDefinition}} as returned from this function
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
#'  The neighbour definition must be symmetric
#'  If a is among the neighbours of b, b must also be among the neighbours of a.
#'  Neighbours may be defined even if they are not geographic neighbours.
#'  
#'  For DefinitionMethod 'StratumPolygon':
#'  A \code{\link[RstoxFDA]{CarNeighbours}} table will be calculated from the provided 'StratumPolygon'.
#'  Strata that are geographic neighbours (touching each other) will be considered neighbours.
#'  runing time and correctness of calcuation may depend on the quality and complexity of the 'StratumPolygon'.
#' @param processData data.table() as returned from this function
#' @param DefinitionMethod 'ResourceFile' or 'StratumPolygon'. See details
#' @param FileName path to file for resource 
#' @param StratumPolygon Definition of spatial strata that neighbours should be calculated for (\code{\link[RstoxBase]{StratumPolygon}}).
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
#' @param DefinitionMethod 'ResourceFile'. See details.
#' @param FileName path to resource file
#' @param UseProcessData Bypasses execution of function, if TRUE, and simply returns argument 'ProcessData'
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

#' Read Stock splitting parameters from file
#'  @noRd
readStockSplittingParamteres <- function(resourceFilePath, encoding){
  tab <- readTabSepFile(resourceFilePath,
                        col_types = "ccdddddddd",
                        col_names = c("StockNameCC", "StockNameS", "ProbabilityType1As1",
                                      "ProbabilityType1As5", "ProbabilityType2As2",
                                      "ProbabilityType2As4",	"ProbabilityType4As2",
                                      "ProbabilityType4As4",	"ProbabilityType5As1",
                                      "ProbabilityType5As5"),
                        encoding = encoding)
  
  if (nrow((tab)) != 1){
    stop("Malformed resource file: contains more than one row.")
  }
  
  return(tab)
}

#' checks that probabilities are valid for StockSplittingParamteres
#' report specific errors
#' @noRd
checkProbabilities <- function(tab, tolerance=1e-10){
  if (any(tab[,3:ncol(tab)]<0) | any(tab[,3:ncol(tab)]>1)){
    stop("All probabilities must be in [0,1].")
  }
  sumToOne <- function(values){
    return(abs(1-sum(values)) < tolerance)
  }
  if (!sumToOne(c(tab$ProbabilityType1As1, tab$ProbabilityType1As5))){
    stop("Parameters 'ProbabilityType1As1' and ProbabilityType1As5' must sum to one")
  }
  if (!sumToOne(c(tab$ProbabilityType2As2, tab$ProbabilityType2As4))){
    stop("Parameters 'ProbabilityType2As2' and ProbabilityType2As4' must sum to one")
  }
  if (!sumToOne(c(tab$ProbabilityType4As2, tab$ProbabilityType4As4))){
    stop("Parameters 'ProbabilityType4As2' and ProbabilityType4As4' must sum to one")
  }
  if (!sumToOne(c(tab$ProbabilityType5As1, tab$ProbabilityType5As5))){
    stop("Parameters 'ProbabilityType5As1' and ProbabilityType5As5' must sum to one")
  }
  if (!is.StockSplittingParamteres(tab)){
    stop("The chosen file does not form a valid stock-splitting specifciation.")
  }
}

#' Define Stock Splitting Parameters
#' @description
#'  Defines parameters for the stock-splitting analysis in Reca, including
#'  parameters for misclassifying when determining stock membership of a specimen.
#' @details
#'  For DefinitionMethod 'ResourceFile', definitions are read from a UTF-8 encoded tab separated file with headers and one row
#'  with headers corresponding to column names in \code{\link[RstoxFDA]{StockSplittingParamteres}}.
#'  see \code{\link[RstoxFDA]{StockSplittingParamteres}} for further explanation on the coding system.
#' @param processData data.table() as returned from this function
#' @param FileName path to resource file
#' @param StockNameCC Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param StockNameS Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType1As1 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType5As1 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType2As2 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType4As2 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType2As4 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType4As4 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType1As5 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param ProbabilityType5As5 Variable for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param UseProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return \code{\link[RstoxFDA]{StockSplittingParamteres}}.
#' @export
DefineStockSplittingParamteres <- function(processData, DefinitionMethod=c("ResourceFile", "FunctionParameters"), FileName=character(),
                                           StockNameCC=character(), StockNameS=character(), ProbabilityType1As1=numeric(),
                                           ProbabilityType5As1=numeric(), ProbabilityType2As2=numeric(),
                                           ProbabilityType4As2=numeric(),	ProbabilityType2As4=numeric(),
                                           ProbabilityType4As4=numeric(),	ProbabilityType1As5=numeric(),
                                           ProbabilityType5As5=numeric(),
                                           UseProcessData=F){

  if (UseProcessData){
    return(processData)
  }

  DefinitionMethod <- match.arg(DefinitionMethod, DefinitionMethod)
  
  if (DefinitionMethod == "ResourceFile"){
    encoding <- "UTF-8"
    tab <- readStockSplittingParamteres(FileName, encoding)
    checkProbabilities(tab)
    return(tab)
  }
  else if (DefinitionMethod == "FunctionParameters"){
    
    if (!isGiven(StockNameCC) | !isGiven(StockNameS)){
      stop("Provide names for the stocks ('StockNameCC' and 'StockNameS'.")
    }
    if (!isGiven(ProbabilityType1As1) | !isGiven(ProbabilityType1As5) |
        !isGiven(ProbabilityType2As2) | !isGiven(ProbabilityType2As4) |
        !isGiven(ProbabilityType4As2) | !isGiven(ProbabilityType4As4) |
        !isGiven(ProbabilityType5As1) | !isGiven(ProbabilityType5As5)){
      stop("Provide all stock classification probailities ProbabilityType<X>As<Y>.")
    }
    
    tab <- data.table::data.table(StockNameCC=StockNameCC,
                                  StockNameS=StockNameS,
                                  ProbabilityType1As1=ProbabilityType1As1,
                                  ProbabilityType5As1=ProbabilityType5As1,
                                  ProbabilityType2As2=ProbabilityType2As2,
                                  ProbabilityType4As2=ProbabilityType4As2,
                                  ProbabilityType2As4=ProbabilityType2As4,
                                  ProbabilityType4As4=ProbabilityType4As4,
                                  ProbabilityType1As5=ProbabilityType1As5,
                                  ProbabilityType5As5=ProbabilityType5As5)
    checkProbabilities(tab)
    return(tab)
  }
  
  else{
    stop(paste("DefinitionMethod", DefinitionMethod, "not supported."))
  }
}

#' Define Weight Conversion Factors
#' @description 
#'  Define weight conversion factors for calculating approximate weights for a product type (e.g. round fish)
#'  from the weight of other fish products, such as gutted fish.
#' @details 
#'  For DefinitionMethod 'ResourceFile':
#'  Definitions are read from a tab separated, UTF-8 encoded file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'Description'}{Free-text description of the product}
#'  \item{Column 2: 'Species'}{Identifier for the species that the conversion applies to}
#'  \item{Column 3: 'ProductType'}{Identifier for the type of product that the conversion applies to}
#'  \item{Column 4: 'WeightFactor'}{scalar value that weights for the given 'ProductType' can be multiplied with to approximate the desired product type (w.g. round fish).}
#'  }
#'  
#'  Missing values for WeightFactor are interpreted as NA, and will result in NA for weights after conversion.
#'  
#' @param processData \code{\link[RstoxFDA]{WeightConversionTable}} as returned from this function
#' @param DefinitionMethod 'ResourceFile'. See details.
#' @param FileName path to resource file
#' @param UseProcessData Bypasses execution of function, if TRUE, and simply returns argument 'processData'
#' @return \code{\link[RstoxFDA]{WeightConversionTable}}
#' @export
DefineWeightConversionFactor <- function(processData, DefinitionMethod=c("ResourceFile"), FileName, UseProcessData=F){
  
  if (UseProcessData){
    return(processData)
  }
  
  encoding <- "UTF-8"
  
  if (DefinitionMethod == "ResourceFile"){
    tab <- readTabSepFile(FileName,
                          col_types = "cccd",
                          col_names = c("Description", "Species", "ProductType", "WeightFactor"),
                          encoding = encoding)
    
    dups <- tab[duplicated(tab[,c("Species", "ProductType"), with=F])]
    if (nrow(dups) > 0){
      stop("File contains duplicate definitions for conversionfactors (Species-ProductType): ", paste(paste(dups$Species, dups$ProductType, sep="-"), collapse=","))
    }
    
    return(tab)
  }
  else{
    stop(paste("DefinitionMethod", DefinitionMethod, "is not supported"))
  }
}

