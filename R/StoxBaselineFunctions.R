
#' Issues warning prefixed with StoX:, necessary to be picked up by GUI
#' @noRd
stoxWarning <- function(msg){
  warning(paste("StoX:", msg))
}

#' Check if parameter is given
#' @noRd
isGiven <- function(value=NULL){
  if (length(value) == 0){
    return(FALSE)
  }
  if (length(value) == 1){
    
    if (value == ""){
      return(FALSE)
    }
  }
  if (is.null(value)){
    return(FALSE)
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


#' Read landing files
#' @description 
#'  This function reads multiple landing files (sales-notes) to a list with a list of tables for each file.
#'  It extends upon \code{\link[RstoxData]{ReadLanding}} in that it supports additional input formats.
#'  
#' @details 
#'  Norwegian sales-notes data are archived and curated by the Norwegian Directorate of Fisheries (FDIR).
#'  Data is made public or is transferred to the Institute of Marine Research (IMR) in various formats.
#'  Some of these formats are also archived by the Norwegian Marine Datacenter (NMD) at IMR.
#'  
#'  Some of the supported formats are missing columns supported by \code{\link[RstoxData]{LandingData}},
#'  these columns are set to NA. Likewise some formats have additional columns, not supported by \code{\link[RstoxData]{LandingData}},
#'  these are ignored.
#' 
#'  Formats may be one of the following
#'  \describe{
#'   \item{landingerv2}{XML-files given in the namespace http://www.imr.no/formats/landinger/v2. Default encoding: UTF-8 (enforced by format)}
#'   \item{lss}{Lss format, used for official deliveries from Fdir to IMR (2005-). Default encoding: Latin-1 (iso-8859-1)}
#'   \item{FDIR.2021}{Sales notes from FDIRs open data sets, as formatted in their 2021 release. Default encoding: UTF-8.}
#'  }
#'  
#'  Files in the format 'landingerv2' and 'lss' may be obtained from the NMD landings API at IMR.
#'  
#'  The lss format has been using various naming conventions. Data is read by colmn index, and strict checking of column names is not performed.
#'  
#' @param FileNames The paths of the landing files.
#' @param Format The file format of the landing files.
#' @param FileEncoding encoding for the files that should be read. If not given the default encoding for each format is used.
#' @family IO functions
#' @export
ReadLandingFDA <- function(FileNames, Format=c("landingerv2", "lss", "FDIR.2021"), FileEncoding=c("Default", "UTF-8", "Latin-1")){
  
  if (isGiven(FileEncoding)){
    FileEncoding <- "Default"
  }
  
  FileEncoding <- match.arg(FileEncoding, FileEncoding)
  Format <- match.arg(Format, Format)
  
  if (!isGiven(Format)){
    stop("Argument 'Format' must be provided")
  }
  
  if (Format == "landingerv2"){
    if (isGiven(FileEncoding) & !(FileEncoding %in% c("UTF-8", "Default"))){
      stop("Format landingerv2 only supports encoding UTF-8")
    }
    
    for (file in FileNames){
      if (!endsWith(file, "xml") & !endsWith(file, "zip")){
        stop(paste("File name", file, "does not end with xml or zip. Choose appropriate format."))
      }
    }
    
    return(RstoxData::ReadLanding(FileNames))
  }
  else if (Format == "lss"){
    if (FileEncoding == "Default"){
      FileEncoding <- "Latin-1"
    }
    output <- list()
    for (file in FileNames){
      filepath <- path.expand(file)
      bn <- basename(filepath)
      suppressWarnings(conv <- RstoxData::convertToLandingData(RstoxData::readLssFile(filepath, encoding=FileEncoding, strict=T)))
      output[[bn]] <- conv$ConvertedData
    }
    return(output)
  }
  else if (Format == "FDIR.2021"){
    if (FileEncoding == "Default"){
      FileEncoding <- "UTF-8"
    }
    output <- list()
    for (file in FileNames){
      filepath <- path.expand(file)
      bn <- basename(filepath)
      openfdir <- readFdirOpenLandings(filepath, encoding=FileEncoding)
      lss <- convertToLssData(openFdirData=openfdir)
      conv <- RstoxData::convertToLandingData(lss)
      output[[bn]] <- conv$ConvertedData
    }
    return(output)
        
  }
  else{
    stop(paste("Format", Format, "not recognized."))
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
#' @family spatial coding functions
#' @export
#' @md
AddAreaPositionStoxLanding <- function(StoxLandingData, AreaPosition, LocationVariable = c("None", "Location", "Coastal")){
  
  LocationVariable <- match.arg(LocationVariable, LocationVariable)
  
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

#' return string for warnings with address to biotic catchsample
#' @noRd
getBioticCatchSampleAdress <- function(tab){
  stopifnot(nrow(tab)==1)
  return(paste("startyear/missiontype/platform/missionumber:", paste(tab$startyear[1], tab$missiontype[1], tab$platform[1], tab$missionnumber[1], sep="/"),
  "serialnumber:", tab$serialnumber[1],
  "catchsampleid:", tab$catchsampleid[1]))
}

#' Convert lengths Biotic
#' @description 
#'  Convert lengths to approximate values for a desired length measurement. Typically 'total length'.
#' @details 
#'  Different length measurements may be defined for fish, such as 'total length', 'standard length' or 'fork length'.
#'  For partially processed fish, only some measurements may be available, such as 'head-length'.
#'  When desired length measurment is not measured,
#'  'total length' or some other length measurement may be approximated by a regression model.
#'  
#'  For records where the variable 'catchcategory' on the table 'catchsample' matches the 'Species' variable in 'LengthConversionTable'
#'  this function converts lengths ('length' on the 'Individual'-table) to desired measurement type ('TargetLengthMeasurement')
#'  by matching the 'lengthmeasurement' variable on the corresponding 'catchsample'-table to 'MeasurementType' in 'LengthConversionTable'
#'  and applying the formula L_{desired} = 0.01 \* Alpha + Beta \* L_{MeasurementType}.
#'  The factor of 0.01 is applied because lengths in Biotic is defined i m, while 'Alpha' in 'LengthConversionTable' is defined in cm.
#'  
#'  After conversion the 'lengthmeasurement'-variable will be changed to reflect the converted length.
#'  That is, they will be set to 'TargetLengthMeasurement'.
#'  
#'  These variables are on these tables in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#' @param BioticData \code{\link[RstoxData]{BioticData}} for which lengths should be converted
#' @param LengthConversionTable \code{\link[RstoxFDA]{LengthConversionTable}} with parameters for converting lengths to 'TargetLengthMeasurement'.
#' @param TargetLengthMeasurement The desired length measurement. Typically the code for 'total length'.
#' @return \code{\link[RstoxData]{BioticData}} with converted lengths.
#' @seealso \code{\link[RstoxFDA]{DefineLengthConversionParameters}} for configuration of length parameters.
#' @family nmdbiotic functions
#' @family parameter conversion functions
#' @export
#' @md
ConvertLengthBiotic <- function(BioticData, LengthConversionTable, TargetLengthMeasurement=character()){
  
  if (!isGiven(TargetLengthMeasurement)){
    stop("'TargetLengthMeasurement' must be provided.")
  }
  if (!is.LengthConversionTable(LengthConversionTable)){
    stop("invalid LengthConversionTable")
  }
  
  for (file in names(BioticData)){
    if ("catchsample" %in% names(BioticData[[file]]) &
        "individual" %in% names(BioticData[[file]]) &
        "catchcategory" %in% names(BioticData[[file]]$catchsample) &
        "lengthmeasurement" %in% names(BioticData[[file]]$catchsample) &
        "length" %in% names(BioticData[[file]]$individual)){
      
      indCatch <- merge(BioticData[[file]]$catchsample, BioticData[[file]]$individual, by=names(BioticData[[file]]$catchsample)[names(BioticData[[file]]$catchsample) %in% names(BioticData[[file]]$individual)])
      missing <- unique(indCatch[!is.na(indCatch$length) & is.na(indCatch$lengthmeasurement), c("startyear", "missiontype", "platform", "missionnumber", "serialnumber", "catchsampleid"), with=F])
      if (nrow(missing)>0){
        for (i in 1:nrow(missing)){
          stoxWarning(paste("Length measurements found for 'catchsample' without 'lengthmeasurement' set: ", getBioticCatchSampleAdress(missing[i,])))          
        }
        stop("Could not convert length for unkown length measurement for individuals.")
      }
      
      speciesInData <- unique(BioticData[[file]]$catchsample$catchcategory)
      for (species in speciesInData){
        spectab <- LengthConversionTable[LengthConversionTable$Species == species,]
        
        #locate individuals of species
        catchKeys <- names(BioticData[[file]]$catchsample)[names(BioticData[[file]]$catchsample) %in% names(BioticData[[file]]$individual)]
        catchIds <- Reduce(BioticData[[file]]$catchsample[,catchKeys, with=F], f = paste)
        catchIdsSpecies <- catchIds[BioticData[[file]]$catchsample$catchcategory == species]
        indCatchIds <- Reduce(BioticData[[file]]$individual[,catchKeys, with=F], f = paste)
        
        for (cId in unique(catchIdsSpecies)){
          measurement <- BioticData[[file]]$catchsample$lengthmeasurement[match(cId, catchIds)]
          filterIndividual <- indCatchIds == cId & !is.na(BioticData[[file]]$individual$length)
          
          # no individuals measured, set measurment code if not NA
          if (sum(filterIndividual)==0){
            if (!is.na(BioticData[[file]]$catchsample$lengthmeasurement[match(cId, catchIds)])){
              BioticData[[file]]$catchsample$lengthmeasurement[match(cId, catchIds)] <- TargetLengthMeasurement              
            }
          }
          #individuals measured, but length measurement not set
          else if (is.na(measurement)){
            stop("Could not convert length for unkown length measurement for individuals.")
          }
          # individuals measured and conversion necessary
          else if (measurement != TargetLengthMeasurement){
            
            if (!(measurement %in% spectab$MeasurementType)){
              cs <- BioticData[[file]]$catchsample[match(cId, catchIds),]
              stoxWarning(paste("'lengthmeasurment'", measurement, "found for 'catchsample':", getBioticCatchSampleAdress(cs[1,])))
              stop(paste("Conversion parameters not found for length measurement", measurement, "for species", species))
            }
            
            # set measurement code
            BioticData[[file]]$catchsample$lengthmeasurement[match(cId, catchIds)] <- TargetLengthMeasurement
            
            # convert length
            alpha <- spectab$Alpha[match(measurement, spectab$MeasurementType)]
            beta <- spectab$Beta[match(measurement, spectab$MeasurementType)]
            BioticData[[file]]$individual$length[filterIndividual] <- 0.01*alpha + beta*BioticData[[file]]$individual$length[filterIndividual]
          }
        }
      }
    }
  }
  
  return(BioticData)
}

#' Convert weights Biotic
#' @description 
#'  Convert weights to approximate weight for desired product type. Appropriate when BioticData is read from NMDbiotic.
#' @details 
#'  Different products of fish may be measured and round weight (live weight)
#'  or some other product type may be approximated by multiplying with a scalar conversion factor.
#'  
#'  For records where the variable 'catchcategory' on the table 'catchsample' matches the 'Species' variable in 'WeightConversionTable'
#'  this function converts weights to desired product type ('TargetProductType') by matching 'producttype'-variables to 'ProductType' in 'WeightConversionTable'.
#'  The following weights may be converted, depending on the parameter 'ConversionType':
#'  \describe{
#'   \item{'All' or 'CatchWeights'}{'catchweight' is converted based on 'catchproducttype' both on the table 'catchsample'}
#'   \item{'All' or 'CatchWeights'}{'lengthsampleweight' is converted based on 'sampleproducttype' both on the table 'catchsample'}
#'   \item{'All' or 'IndividualWeight'}{'individualweight' is converted based on 'individualproducttype' both on the table 'individual'} 
#'  }
#'  
#'  After conversion the 'producttype'-variables will be changed to reflect the converted weight.
#'  That is they will be set to 'TargetProductType'.
#'  
#'  If 'WeightConversionTable' contains NAs for 'WeightFactor', converted weights will be NA.
#'  This may be useful for dealing with species whoose weights are not needed in subsequent analysis. 
#'  
#'  The variables 'catchvolume', 'lengthsamplevolume', and 'individualvolume'
#'  are also dependent on product type. These are set to NA as weight is converted.
#'  
#'  These variables are on these tables in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#' @param BioticData \code{\link[RstoxData]{BioticData}} for which weights should be converted
#' @param ConversionType defines which weights to convert, may be 'All', 'CacthWeight', 'SampleWeight', 'IndividualWeight'
#' @param WeightConversionTable \code{\link[RstoxFDA]{WeightConversionTable}} with parameters for converting weights to 'TargetProductType'.
#' @param TargetProductType The desired producttype. Typically the code for Round fish.
#' @return \code{\link[RstoxData]{BioticData}} with converted weights.
#' @seealso \code{\link[RstoxFDA]{DefineWeightConversionFactor}} for configuration of weight conversion parameters.
#' @family nmdbiotic functions
#' @family parameter conversion functions
#' @export
#' @md
ConvertWeightBiotic <- function(BioticData, ConversionType=c("All", "CatchWeights", "IndividualWeight"), WeightConversionTable, TargetProductType=character()){
  
  ConversionType <- match.arg(ConversionType, ConversionType)
  
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
      
      
      if (ConversionType %in% c("All", "CatchWeights")){
        BioticData[[file]]$catchsample$catchproducttype[is.na(BioticData[[file]]$catchsample$catchweight) & !is.na(BioticData[[file]]$catchsample$catchproducttype)] <- TargetProductType        
      }
      if (ConversionType %in% c("All", "CatchWeights")){
        BioticData[[file]]$catchsample$sampleproducttype[is.na(BioticData[[file]]$catchsample$lengthsampleweight) & !is.na(BioticData[[file]]$catchsample$sampleproducttype)] <- TargetProductType        
      }
      if (ConversionType %in% c("All", "IndividualWeight")){
        BioticData[[file]]$individual$individualproducttype[is.na(BioticData[[file]]$individual$individualweight) & !is.na(BioticData[[file]]$individual$individualproducttype)] <- TargetProductType        
      }
      
      speciesInData <- unique(BioticData[[file]]$catchsample$catchcategory)
      
      for (species in speciesInData){
        spectab <- WeightConversionTable[WeightConversionTable$Species == species,]
        
        if (ConversionType %in% c("All", "CatchWeights")){
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
        }
        

        if (ConversionType %in% c("All", "CatchWeights")){
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
        }
        
        if (ConversionType %in% c("All", "IndividualWeight")){
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
  }
 return(BioticData) 
}


#' Set position Biotic
#' @description 
#'  Sets start position based on area codes. Appropriate when BioticData is read from NMDbiotic.
#' @details 
#'  Positions are filled in the columns 'latitudestart' and 'longitudestart' on 'fishstation' 
#'  whenever the value in the column 'system' on 'fishstation' is equal to the parameter 'System'.
#'  
#'  When 'LocationVariable' is specified as 'None' area is looked up from 'AreaPosition', using the row where 'Location' is missing.
#'  When 'LocationVariable' is specified as 'location', 'area' and 'location' in 'BioticData' is looked up against 'Area' and 'Location' in 'AreaPosition'.
#'  
#'  No modifications are done to rows that have missing values for 'system' or 'area' (or 'location', depending on the parameter 'LocationVariable').
#'  Any rows with areas coded in another system than specified by the parameter 'System' will not be changed.
#'  
#'  These columns are on the table 'fishstation' in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#'  
#' @param BioticData \code{\link[RstoxData]{BioticData}} data for which positions should be set
#' @param AreaPosition coordinates for Area and Location codes, see \code{\link[RstoxFDA]{AreaPosition}}
#' @param LocationVariable Specify which column in 'BioticData' should are represented by 'Location' in 'AreaPosition'. See details.
#' @param System identifies the area coding system used. Corresonds to the column 'system' on 'fishstation' in 'BioticData'.
#' @param Overwrite if True any existing values in 'latitudestart' and 'longitudestart' will be overwritten. If False postions with both latitude and longitude will be kept as they were.
#' @return \code{\link[RstoxData]{BioticData}}
#' @seealso \code{\link[RstoxFDA]{DefineAreaPosition}} for configuring definitions of positions for area codes.
#' @export
#' @family nmdbiotic functions
#' @family spatial coding functions
#' @md
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
  
  if (is.null(temporalDefinition$StartYear)){
    temporalDefinition$StartYear <- rep(as.integer(NA), nrow(temporalDefinition))
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
#'  Add a column to StoxLandingData with a temporal category, such as quarters.
#' @details 
#'  A column with a temporal category will be added based on the column 'CatchDate' in 'StoxLandingData'.
#'  Temporal definitions (\code{\link[RstoxFDA]{TemporalDefinition}}) may be produced by
#'  \code{\link[RstoxFDA]{DefinePeriod}}
#'  
#'  Since it may be useful to use different temporal categories for instance for parameterization and reporting
#'  two choices are offered for the name of the added column (see argument 'ColumnName').
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param TemporalDefinition \code{\link[RstoxFDA]{TemporalDefinition}} definition of temporal category.
#' @param ColumnName Name of the added column. Defaults to 'Period'.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @seealso 
#'  \code{\link[RstoxFDA]{DefinePeriod}} for configuring the temporal definition,
#'  \code{\link[RstoxFDA]{AddStratumStoxBiotic}} for similar function for sample data, 
#'  \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of 'Period' as an effect in Reca-estimation,
#'  and \code{\link[RstoxFDA]{ReportFdaSampling}} for use of 'Period' as an aggregation variable when comparing sampling with landed volume.
#' @family temporal coding functions
#' @export
#' @md
AddPeriodStoxLanding <- function(StoxLandingData, TemporalDefinition, ColumnName=c("Period", "ReportPeriod")){
  
  if (!isGiven(ColumnName)){
    ColumnName <- "Period"
  }
  
  columnName=match.arg(ColumnName, ColumnName)
  
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
#'  'Period' will be added based on the column 'DateTime' on the table 'Station' in 'StoxBioticData'.
#'  
#'  Temporal definitions (\code{\link[RstoxFDA]{TemporalDefinition}}) may be produced by
#'  \code{\link[RstoxFDA]{DefinePeriod}}
#' @param StoxBioticData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated with period.
#' @param TemporalDefinition \code{\link[RstoxFDA]{TemporalDefinition}} definiton of temporal category.
#' @return StoxBioticData with column appended. See \code{\link[RstoxData]{StoxBioticData}}.
#'  \code{\link[RstoxFDA]{DefinePeriod}} for configuring the temporal definition,
#'  \code{\link[RstoxFDA]{AddStratumStoxLanding}} for similar function for landing data, 
#'  \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of 'Period' as an effect in Reca-estimation,
#'  and \code{\link[RstoxFDA]{ReportFdaSampling}} for use of 'Period' as an aggregation variable when comparing sampling with landed volume.
#' @family temporal coding functions
#' @export
#' @md
AddPeriodStoxBiotic <- function(StoxBioticData, TemporalDefinition){
  columnName="Period"
  
  if (columnName %in% names(StoxBioticData$Station)){
    stop(paste("The column", columnName, "already exists in StoxLandingData."))
  }
  if (any(is.na(StoxBioticData$Station$DateTime))){
    missing <- StoxBioticData$Station[is.na(StoxBioticData$Station$DateTime),]
    for (i in 1:nrow(missing)){
      stoxWarning(paste("'DateTime' missing from table 'Station' in 'StoxBioticData'. CruiseKey:", missing$CruiseKey[i], ", StationKey:", missing$StationKey[i], "HaulKey:", missing$HaulKey[i]))
    }
    stop("Cannot add 'Period' when 'DateTime' is missing for some rows.")
  }
  
  
  stopifnot(is.TemporalDefinition(TemporalDefinition))
  StoxBioticData$Station <- appendTemporal(StoxBioticData$Station, columnName, TemporalDefinition, "DateTime")
  return(StoxBioticData)
}


#' Adds Strata to StoxLandingData
#' @description
#'  Adds a column to StoxLandingData with the spatial strata each row belongs to.
#' @details
#'  The strata are added to the new column 'Stratum'.
#'  
#'  The Strata may be added as a new column, or to an existing column depending on the 
#'  argument 'ColumnName'. The option 'Stratum' (default) adds strata to a new column 'Stratum'. 
#'  The option 'Area' changes the values in the existing column 'Area'.
#'  
#'  \code{\link[RstoxData]{StoxLandingData}} does not contain columns for positions,
#'  these need to be added as columns 'Latitude' and 'Longitude' before calling this function.
#'  \code{\link[RstoxFDA]{AddAreaPositionStoxLanding}} may be used to append positions, based on area codes.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated. Needs postions appended. See details.
#' @param StratumPolygon Definition of spatial strata. See \code{\link[RstoxBase]{StratumPolygon}}
#' @param ColumnName specifies which column the area should be added to. See details.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @seealso \code{\link[RstoxBase]{DefineStratumPolygon}} for configuring stratum definitions.
#'  \code{\link[RstoxFDA]{AddAreaPositionStoxLanding}} for adding positions to landings,
#'  \code{\link[RstoxFDA]{AddStratumStoxBiotic}} for similar function for sample data, 
#'  \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of 'Stratum' as an effect in Reca-estimation,
#'  \code{\link[RstoxFDA]{DefineCarNeighbours}} for obtaining a neighbour-definition for using 'Stratum' as CAR-effect in Reca-estimation.
#'  and \code{\link[RstoxFDA]{ReportFdaSampling}} for use of 'Stratum' as an aggregation variable when comparing sampling with landed volume.
#' @family spatial coding functions
#' @family Reca functions
#' @export
#' @md
AddStratumStoxLanding <- function(StoxLandingData, StratumPolygon, ColumnName=c("Stratum", "Area")){
  
  ColumnName <- match.arg(ColumnName, ColumnName)
  
  cname <- "cname"    
  latColumn <- "Latitude"    
  lonColumn <- "Longitude"  
  
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  if (!(all(c(latColumn, lonColumn) %in% names(StoxLandingData$Landing)))){
    stop(paste("Could not find columns:", latColumn, "and", lonColumn, "that should be added to StoxLandingData"))
  }
  StoxLandingData$Landing <- appendAreaCode(StoxLandingData$Landing, StratumPolygon, latColumn, lonColumn, cname)
  StoxLandingData$Landing[[ColumnName]] <- StoxLandingData$Landing$cname
  StoxLandingData$Landing$cname <- NULL
  return(StoxLandingData)
}

#' Adds Stratum to StoxBioticData
#' @description
#'  Adds a column 'Stratum' to StoxBioticData with the spatial strata each row belongs to.
#'  
#' @details 
#'  The strata are added to the new column 'Stratum'.
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated. Needs postions appended. See details.
#' @param StratumPolygon Definition of spatial strata. See \code{\link[RstoxBase]{StratumPolygon}}
#' @return StoxBioticData with column appended to data.table 'Station'. See \code{\link[RstoxData]{StoxBioticData}}.
#' @seealso \code{\link[RstoxBase]{DefineStratumPolygon}} for configuring stratum definitions.
#'  \code{\link[RstoxFDA]{AddStratumStoxLanding}} for similar function for landing data, 
#'  \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of 'Stratum' as an effect in Reca-estimation,
#'  \code{\link[RstoxFDA]{DefineCarNeighbours}} for obtaining a neighbour-definition for using 'Stratum' as CAR-effect in Reca-estimation.
#'  and \code{\link[RstoxFDA]{ReportFdaSampling}} for use of 'Stratum' as an aggregation variable when comparing sampling with landed volume.
#' @family spatial coding functions
#' @export
#' @md
AddStratumStoxBiotic <- function(StoxBioticData, StratumPolygon){
  
  columnName <- "Stratum"    
  
  stopifnot("Station" %in% names(StoxBioticData))
  
  missing <- StoxBioticData$Station[is.na(StoxBioticData$Station$Latitude) | is.na(StoxBioticData$Station$Longitude),]
  if (nrow(missing) > 0){
    for (i in 1:nrow(missing)){
      stoxWarning(paste("Position missing from 'Station' in 'StoxBioticData'. CruiseKey:", missing$CruiseKey[i], ", StationKey:", missing$StationKey[i]))      
    }
  }
  
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
#'  Gear groups are defined as translations from a gear code to a gear group.
#'  Translation need to be defined for the VariableName 'Gear' (see \code{\link[RstoxData]{Translation}}). 
#'  The provided Translation should map values ('Gear' in Translation) for the variable 'Gear' in 'StoxBioticData' to a gear group ('NewValue' in Translation).
#'  The gear group will be added to 'StoxLandingData' as the column 'GearGroup'
#'  
#'  For comparison or co-analysis with sample data, a \code{\link[RstoxData]{Translation}} should be defined for \code{\link[RstoxData]{StoxBioticData}}
#'  with the same gear groups.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param Translation Translation table (\code{\link[RstoxData]{Translation}}). See details.
#' @return \code{\link[RstoxData]{StoxLandingData}} with column 'GearGroup' appended.
#' @seealso \code{\link[RstoxData]{DefineTranslation}} for configuring translation to gear groups, 
#'  \code{\link[RstoxFDA]{AddGearGroupStoxBiotic}} for similar function for sample data, 
#'  \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of 'GearGroup' as an effect in Reca-estimation,
#'  and \code{\link[RstoxFDA]{ReportFdaSampling}} for use of 'GearGroup' as an aggregation variable when comparing sampling with landed volume.
#' @family gear coding functions
#' @export
#' @md
AddGearGroupStoxLanding <- function(StoxLandingData, Translation){
  if (!is.Translation(Translation)){
    stop("Translation is not a valid Translation table.")
  }
  if (!is.Translation(Translation)){
    stop("Translation is not a valid Translation table.")
  }
  if (!("Gear" %in% names(Translation))){
    stop("Translation table contains no translation for variable 'Gear'")
  }
  
  geardef <- Translation[,.SD, .SDcols=c("Gear", "NewValue")]
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  StoxLandingData$Landing<-appendGear(StoxLandingData$Landing, "Gear", geardef, "GearGroup")
  return(StoxLandingData)
}

#' Add Gear group to StoxBioticData
#' @description
#'  Adds a column to StoxBioticData with gear groups
#' @details 
#'  Gear groups are defined as translations from a gear code to a gear group.
#'  Translation need to be defined for the VariableName 'Gear' (see \code{\link[RstoxData]{Translation}}). 
#'  The provided Translation should map values ('Gear' in Translation) for the variable 'Gear'on the table 'Haul' in 'StoxBioticData' to a gear group ('NewValue' in Translation).
#'  The gear group will be added to 'StoxBioticData' as the column 'GearGroup'
#'  
#'  For comparison or co-analysis with landing data, a \code{\link[RstoxData]{Translation}} should be defined for \code{\link[RstoxData]{StoxLandingData}}
#'  with the same gear groups.
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated.
#' @param Translation Translation table (\code{\link[RstoxData]{Translation}}). See details.
#' @return StoxBioticData with column 'GearGroup' appended. See \code{\link[RstoxData]{StoxBioticData}}.
#' @seealso \code{\link[RstoxData]{DefineTranslation}} for configuring translation to gear groups, 
#'  \code{\link[RstoxFDA]{AddGearGroupStoxLanding}} for similar function for landing data, 
#'  \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of 'GearGroup' as an effect in Reca-estimation,
#'  and \code{\link[RstoxFDA]{ReportFdaSampling}} for use of 'GearGroup' as an aggregation variable when comparing sampling with landed volume.
#' @family gear coding functions
#' @export
#' @md
AddGearGroupStoxBiotic <- function(StoxBioticData, Translation){
  if (!is.Translation(Translation)){
    stop("Translation is not a valid Translation table.")
  }
  if (!("Gear" %in% names(Translation))){
    stop("Translation table contains no translation for variable 'Gear'")
  }
  if (any(is.na(StoxBioticData$Haul$Gear))){
    missing <- StoxBioticData$Haul[is.na(StoxBioticData$Haul$Gear),]
    for (i in nrow(missing)){
      stoxWarning(paste("'Gear' missing from 'Haul' in 'StoxBioticData'. CruiseKey:", missing$CruiseKey[i], ", StationKey:", missing$StationKey[i], "HaulKey:", missing$HaulKey[i]))      
    }
    
    stop("'StoxBioticData' has missing values for the variable 'Gear' on the table 'Haul'.")
  }
  
  geardef <- Translation[,.SD, .SDcols=c("Gear", "NewValue")]
  StoxBioticData$Haul<-appendGear(StoxBioticData$Haul, "Gear", geardef, "GearGroup")
  return(StoxBioticData)
}

#' Set time Biotic
#' @description 
#'  Set start time to a fixed time for all stations. Appropriate when BioticData is read from NMDbiotic.
#' @details 
#'  Set the column 'stationstarttime' on the table 'fishstation' in \code{\link[RstoxData]{BioticData}} to a fixed time.
#'  
#'  Setting a fixed time to stationstarttime facilitates conversion to \code{\link[RstoxData]{StoxBioticData}} 
#'  using \code{\link[RstoxData]{StoxBiotic}}, and is applicable if hourly resolution of date and time is not necessary
#'  for subsequent analysis.
#'  
#'  'stationstarttime' is on the table 'fishstation' in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#'  
#' @param BioticData \code{\link[RstoxData]{BioticData}} data for which time should be set
#' @param Time character encoding time. Defaults to 12:00:00Z if not given, otherwise provide 
#'   UTC-time formatted as \code{\link[base]{strptime}}-string: \%H:\%M:\%SZ, e.g. 12:00:00Z
#' @param Overwrite if True any existing values in stationstarttime will be overwritten.
#' @return \code{\link[RstoxData]{BioticData}}
#' @seealso \code{\link{RstoxData}{RstoxData::StoxBiotic}} For converting \code{\link[RstoxData]{BioticData}} to \code{\link[RstoxData]{StoxBioticData}}.
#' @family nmdbiotic functions
#' @family temporal coding functions
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

#' Set short gear codes biotic
#' @description 
#'  Set short gear codes for all stations. Appropriate when BioticData is read from NMDbiotic.
#' @details 
#'  Set the column 'gear' on the table 'fishstation' in \code{\link[RstoxData]{BioticData}} to the first two characters / digits, 
#'  for all rows where the gear code currently consist of four characters.
#'  
#'  The gear codes in NMDBiotic are conventionally organised hierarchically, so that the two first characters denote a gear group.
#'  The two character/digit system is more convenient to work with in many circumstances.
#'  
#'  'gear' is on the table 'fishstation' in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#'  
#' @param BioticData \code{\link[RstoxData]{BioticData}} data for which short gear codes should be set
#' @return \code{\link[RstoxData]{BioticData}}
#' @seealso \code{\link{RstoxData}{RstoxData::StoxBiotic}} For converting \code{\link[RstoxData]{BioticData}} to \code{\link[RstoxData]{StoxBioticData}}.
#' @family nmdbiotic functions
#' @family gear coding functions
#' @export
SetShortGearBiotic <- function(BioticData){

  for (file in names(BioticData)){
    if ("fishstation" %in% names(BioticData[[file]]) & "gear" %in% names(BioticData[[file]]$fishstation)){
      
      l4codes <- (!is.na(BioticData[[file]]$fishstation$gear) & nchar(BioticData[[file]]$fishstation$gear)==4)
      
      BioticData[[file]]$fishstation$gear[l4codes] <- substr(BioticData[[file]]$fishstation$gear[l4codes],1,2)
      
    }
  }
  return(BioticData)
}

#' Set startdate Biotic
#' @description 
#'  Set start date to stop date. Appropriate when BioticData is read from NMDbiotic.
#' @details
#'  Set the column 'stationstartdate' on the table 'fishstation' in \code{\link[RstoxData]{BioticData}}
#'  to the value of 'stationstopdate'.
#'  
#'  Setting stationstartdate facilitates conversion to \code{\link[RstoxData]{StoxBioticData}} 
#'  using \code{\link[RstoxData]{StoxBiotic}}, and is applicable if daily resolution of date and time is not critical
#'  for subsequent analysis.
#'  
#'  These columns are on the table 'fishstation' in data originating from NMDBiotic (http://www.imr.no/formats/nmdbiotic/).
#'  For bioticdata that does not conform to this, no modifications are done.
#' @param BioticData \code{\link[RstoxData]{BioticData}} data for which time should be set
#' @param Overwrite if True any existing values in stationstartdate will be overwritten.
#' @return \code{\link[RstoxData]{BioticData}}
#' @seealso \code{\link{RstoxData}{RstoxData::StoxBiotic}} For converting \code{\link[RstoxData]{BioticData}} to \code{\link[RstoxData]{StoxBioticData}}.
#' @family nmdbiotic functions
#' @family temporal coding functions
#' @export
#' @md
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
#'  Define periods, temporal categories such as 'Quarter', 'Month' or some custom defintion.
#' @details
#'  Periods define an association between dates and a categorical variable definition,
#'  such as 'Month', 'Quarter' or some custom definition.
#'  
#'  Periods may be seasonal, which means that dates are associated with the categories based on 
#'  day and month only. This distinction does not matter if all data is from the same year.
#'  The 'TemporalCategory'-options 'Quarter' and 'Month' produce seasonal definitions, so 
#'  that Jan 1st 2015 and Jan 1st 2016 belon to the same category (e.g. "Q1" if 'TemporalCategory'
#'  is 'Quarter').
#'  
#'  In order to make custom seasonal definitions, use the TemporalCategory'-option 'Custom' 
#'  without providing years in 'CustomPeriods': "DD-MM". Periods defined in this way will automatically
#'  "wrap around" so that dates in January may grouped together with dates in December.
#'  
#'  In order to make non-seasonal definitions for Quarter or Month, provide the 'CustomPeriods'
#'  for all years of interest on the format "DD-MM-YYYY". If years are provided the periods do not "wrap around"
#'  and categories are automatically extended to the entire year, if necesesarry.
#'  That is a category starting with 1. Jan is added if not present, and likewise a category ending with 31. Dec.
#'  
#' @param processData \code{\link[RstoxFDA]{TemporalDefinition}} as returned from this function.
#' @param TemporalCategory type of temporal category: 'Quarter', 'Month' or 'Custom', defaults to 'Quarter'
#' @param CustomPeriods provide if temporalCategory is 'Custom', vector of strings formatted as DD-MM or DD-MM-YYYY, giving the start date of each temporal category. See 'details'
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return Temporal Categories, see: \code{\link[RstoxFDA]{TemporalDefinition}}.
#' @seealso \code{\link[RstoxFDA]{AddPeriodStoxBiotic}} and \code{\link[RstoxFDA]{AddPeriodStoxLanding}} for adding Periods to data.
#' @examples 
#'  # Define seasonal periods quarter
#'  DefinePeriod(TemporalCategory = "Quarter")
#'  
#'  # Define two periods "15th March - 15th Sep" and "15th Sep - 15th March"
#'  DefinePeriod(TemporalCategory = "Custom", CustomPeriods = c("15-09", "15-03"))
#' 
#'  # Add years to speficiation to define four periods "15th March 2015 - 31st Dec 2015" ...
#'  DefinePeriod(TemporalCategory = "Custom", CustomPeriods = c("15-09-2105", "15-03-2015"))
#' 
#' @family temporal coding functions
#' @export
#' @md
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
        StartMonth=as.integer(seq(1,12))
    )
  }
  else if (TemporalCategory == "Quarter"){
    output <- data.table::data.table(Period=as.character(
      c("Q1", "Q2", "Q3", "Q4")),
      StartDay=as.integer(rep(1,4)),
      StartMonth=as.integer(c(1,4,7,10))
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
    
    if (all(!is.na(years))){
      output <- data.table::data.table(Period=as.character(
        paste("[", startstr, ", ", endstr, ">", sep="")),
        StartDay=days,
        StartMonth=months,
        StartYear=years
      )
    }
    else{
      stopifnot(all(is.na(years)))
      output <- data.table::data.table(Period=as.character(
        paste("[", startstr, ", ", endstr, ">", sep="")),
        StartDay=days,
        StartMonth=months
      )
    }
    
  }
  else{
    stop(paste("Temporal category", TemporalCategory, "not recognized."))
  }

  return(output)
}

#' Define Area Code Positions
#' @description
#'  Define positions for areas of a spatial coding system.
#' @details
#'  Defines an association between area codes and positions that represent that area code.
#'  For example an area code could be associated with the centre of mass of the area, or
#'  some other point within the area. This may be useful for providing approximate
#'  coordinates when locations are identified by area codes. The soruce of the
#'  area-position association may be a table or a appropriately formatted
#'  polygon-definition (\code{\link[RstoxBase]{StratumPolygon}}). 
#' 
#'  For DefinitionMethod 'ResourceFile':
#'  Definitions are read from a tab separated, UTF-8 encoded file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'Area'}{Area code (key)}
#'  \item{Column 2: 'Location'}{optional subdivision of area. If provided, positions for missing locations should be encoded as well.}
#'  \item{Column 3: 'Latitude'}{WGS84 Latitude, decimal degrees}
#'  \item{Column 4: 'Longitude'}{WGS84 Longitude, decimal degrees}
#'  }
#'  
#'  For DefinitionMethod 'StratumPolygon':
#'  Definitions are extracted from a \code{\link[RstoxBase]{StratumPolygon}}:
#'  'Area' in \code{\link[RstoxFDA]{AreaPosition}} is derived from the column 'StratumName' in \code{\link[RstoxBase]{StratumPolygon}}.
#'  'Location' in \code{\link[RstoxFDA]{AreaPosition}} is encoded as missing.
#'  'Latitude' and 'Longitude' in \code{\link[RstoxFDA]{AreaPosition}} are the coordinates set for each polygon in \code{\link[RstoxBase]{StratumPolygon}}.
#'  
#' @param processData \code{\link[RstoxFDA]{AreaPosition}} as returned from this function.
#' @param DefinitionMethod 'ResourceFile' or 'StratumPolygon', see details.
#' @param FileName path to resource file
#' @param StratumPolygon \code{\link[RstoxBase]{StratumPolygon}} to extract area positions from.
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{AreaPosition}}.
#' @seealso \code{\link[RstoxFDA]{SetAreaPositionsBiotic}} and \code{\link[RstoxFDA]{AddAreaPositionStoxLanding}} for adding positions to data.
#' @family spatial coding functions
#' @export
#' @md
DefineAreaPosition <- function(processData, DefinitionMethod=c("ResourceFile", "StratumPolygon"), FileName=character(), StratumPolygon, UseProcessData=F){

  DefinitionMethod <- match.arg(DefinitionMethod, DefinitionMethod)
  encoding="UTF-8"
  
  if (UseProcessData){
    return(processData)
  }

  if (DefinitionMethod == "ResourceFile"){
    tab <- readTabSepFile(FileName, col_classes = c("character", "character", "numeric", "numeric"), col_names = c("Area", "Location",	"Latitude",	"Longitude"), encoding = encoding)
    
    missingLoc <- tab[is.na(tab[["Location"]]),]
    
    if (length(unique(missingLoc[["Area"]])) != length(unique(tab[["Area"]]))){
      stop("Malformed resource file. Some Area does not have coordinates defined for the case when location is missing.")
    }
    
    return(tab)
  }
  
  if (DefinitionMethod == "StratumPolygon"){
    if (!("StratumName" %in% names(StratumPolygon))){
      stop("'StratumPolygon' must be an RstoxBase::StratumPolygon object.")
    }
    pos <- data.table::data.table(sp::coordinates(StratumPolygon))
    names(pos) <- c("Longitude", "Latitude")
    
    stopifnot(nrow(pos)==nrow(StratumPolygon))
    
    pos$Area <- StratumPolygon$StratumName
    pos$Location <- as.character(NA)
    return(pos)
    
  }
  
  stop(paste("DefinitionMethod", DefinitionMethod, "not supported."))

}

#' Read CAR neighbours from file
#' @noRd
loadCarNeighboursFile <- function(FileName, encoding){
  tab <- readTabSepFile(FileName, col_classes = c("character", "character"), col_names = c("CarValue", "Neighbours"), encoding = encoding)
  
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
  
  #force planar geometry for sf operations, for compability reasons
  sphergeom <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  
  sfpoly <- sf::st_as_sf(StratumPolygon)
  neighbourIndecies <- sf::st_touches(sfpoly, sfpoly)
  

  carValues <- sfpoly$StratumName
  neighbours <- unlist(lapply(neighbourIndecies, function(x){paste(sfpoly$StratumName[x],collapse=",")}))
  
  carTable <- data.table::data.table(CarValues=carValues, Neighbours=neighbours)
  
  sf::sf_use_s2(sphergeom)
  
  return(carTable)
}

#' Define CAR neighbours
#' @description
#'  Define which values of a categorical variable are to be considered neighbours,
#'  when used as a CAR-variable (Conditional AutoRegressive variable) in Reca.
#' @details
#'  A CAR-variable (condititional autoregressive variable) in Reca is typically a spatial variable
#'  reflecting location of a catch. \code{\link[RstoxFDA]{CarNeighbours}} defines
#'  which values (e.g. areas) are to be considered neighbouts in parameterisation.
#'  For spatial variables this is typically configured as geographic neighbours, but
#'  other definitions are possible. Geomtric neighbour may be calcuated from
#'  \code{\link[RstoxBase]{StratumPolygon}} if that is defines a spatial variable,
#'  by selecting the appropriate 'DefinitionMethod'.
#'  
#'  For DefinitionMethod 'ResourceFile':
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'CarValue'}{Value for the CAR-variable (key)}
#'  \item{Column 2: 'Neigbhours'}{Comma-separated list of neighbours (each should occur in Column 1)}
#'  }
#'  The neighbour definition must be symmetric
#'  If a is among the neighbours of b, b must also be among the neighbours of a.
#'  
#'  For DefinitionMethod 'StratumPolygon':
#'  A \code{\link[RstoxFDA]{CarNeighbours}} table will be calculated from the provided 'StratumPolygon'.
#'  Strata that are geographic neighbours (touching each other) will be considered neighbours.
#'  runing time and correctness of calcuation may depend on the quality and complexity of the 'StratumPolygon'.
#' @param processData \code{\link[RstoxFDA]{CarNeighbours}} as returned from this function
#' @param DefinitionMethod 'ResourceFile' or 'StratumPolygon'. See details.
#' @param FileName path to file for resource 
#' @param StratumPolygon Definition of spatial strata that neighbours should be calculated for (\code{\link[RstoxBase]{StratumPolygon}}).
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return Area Neighbour Definition, see: \code{\link[RstoxFDA]{CarNeighbours}}.
#' @seealso \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of the definition in Reca-estimates, and \code{\link[RstoxBase]{DefineStratumPolygon}} for how to define a spatial variable from a strata-definition.
#' @family spatial coding functions
#' @export
#' @md
DefineCarNeighbours <- function(processData,
                                DefinitionMethod = c("ResourceFile", "StratumPolygon"), 
                                FileName=character(), StratumPolygon, UseProcessData = F){
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
#'  Defines probabilities for misreading ages.
#' @details
#'  Defines probabilities for misreading ages, that is assumed probabilites for determining
#'  a fish to be of age 'x' given that it actually is age 'y'.
#'  
#'  Definitions are read from a tab separated file with headers and row names in first column.
#'  All row and column names should be integers representing ages.
#'  The matrix encodes the probability of observing an age (rows), given true age (columns),
#'  and columns must sum to 1.
#' @param processData \code{\link[RstoxFDA]{AgeErrorMatrix}} as returned from this function
#' @param DefinitionMethod 'ResourceFile'. See details.
#' @param FileName path to resource file
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return Age Error Matrix, see: \code{\link[RstoxFDA]{AgeErrorMatrix}}.
#' @seealso \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of age-error matrices in Reca-estimation
#' @family Reca functions
#' @export
#' @md
DefineAgeErrorMatrix <- function(processData, DefinitionMethod=c("ResourceFile"), FileName = character(), UseProcessData=F){
  
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
#' @noRd
readStockSplittingParameters <- function(resourceFilePath, encoding){
  tab <- readTabSepFile(resourceFilePath,
                        col_classes = c(rep("character", 2), rep("numeric", 8)),
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

#' checks that probabilities are valid for StockSplittingParameters
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
  if (!is.StockSplittingParameters(tab)){
    stop("The chosen file does not form a valid stock-splitting specifciation.")
  }
}

#' Define Stock Splitting Parameters
#' @description
#'  Defines parameters for the stock-splitting analysis in Reca, including
#'  parameters for misclassifying when determining stock membership of a specimen.
#'  
#' @details
#'  For DefinitionMethod 'ResourceFile', definitions are read from a UTF-8 encoded tab separated file with headers and one row
#'  with headers corresponding to column names in \code{\link[RstoxFDA]{StockSplittingParameters}}.
#'  see \code{\link[RstoxFDA]{StockSplittingParameters}} for further explanation on the coding system.
#'  
#'  For DefinitionMethod 'FunctionParameters' defintions are constructed from parameters to this function.
#'  
#' @param processData data.table() as returned from this function
#' @param DefinitionMethod 'ResourceFile' or 'FunctionParameters', see details.
#' @param FileName path to resource file
#' @param StockNameCC Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param StockNameS Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType1As1 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType5As1 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType2As2 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType4As2 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType2As4 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType4As4 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType1As5 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param ProbabilityType5As5 Variable for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{StockSplittingParameters}}.
#' @seealso \code{\link[RstoxFDA]{PrepareRecaEstimate}} for use of stock-splitting parameters in Reca-estimation.
#' @family Reca functions
#' @export
#' @md
DefineStockSplittingParameters <- function(processData, DefinitionMethod=c("ResourceFile", "FunctionParameters"), FileName=character(),
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
    tab <- readStockSplittingParameters(FileName, encoding)
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

#' Define Length Conversion Parameters
#' @description 
#'  Define length conversion parameters for approximating one type of length measurement from measurments of another type,
#'  based on a linear regression fit between measurement types. E.g. calculating 'total length' from measured 'fork length'.
#' @details 
#'  For DefinitionMethod 'ResourceFile':
#'  Definitions are read from a tab separated, UTF-8 encoded file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'Description'}{Free-text description of the product}
#'  \item{Column 2: 'Species'}{Identifier for the species that the conversion applies to}
#'  \item{Column 3: 'MeasurmentType'}{Identifier for the type of length measurement of the measured length}
#'  \item{Column 4: 'Alpha'}{scalar value representing the intercept (in cm) of a linear regression fit between length measurements.}
#'  \item{Column 5: 'Beta'}{scalar value representing the slope of a linear regression fit between length measurements.}
#'  }
#'  See also \code{\link[RstoxFDA]{LengthConversionTable}}
#'  
#' @param processData \code{\link[RstoxFDA]{LengthConversionTable}} as returned from this function
#' @param DefinitionMethod 'ResourceFile'. See details.
#' @param FileName path to resource file
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{LengthConversionTable}}
#' @seealso \code{\link[RstoxFDA]{ConvertLengthBiotic}} for applying length conversion to data
#' @family parameter conversion functions
#' @export
#' @md
DefineLengthConversionParameters <- function(processData, DefinitionMethod=c("ResourceFile"), FileName = character(), UseProcessData=F){
  
  if (UseProcessData){
    return(processData)
  }
  
  encoding <- "UTF-8"
  
  if (DefinitionMethod == "ResourceFile"){
    tab <- readTabSepFile(FileName,
                          col_classes = c(rep("character", 3), rep("numeric", 2)),
                          col_names = c("Description", "Species", "MeasurementType", "Alpha", "Beta"),
                          encoding = encoding)
    
    dups <- tab[duplicated(tab[,c("Species", "MeasurementType"), with=F])]
    if (nrow(dups) > 0){
      stop("File contains duplicate definitions for length measurements (MeasurementType): ", paste(paste(dups$Species, dups$MeasurementType, sep="-"), collapse=","))
    }
    
    return(tab)
  }
  else{
    stop(paste("DefinitionMethod", DefinitionMethod, "is not supported"))
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
#'  For DefinitionMethod 'FDIR.VIII.2022', the table \code{\link[RstoxFDA]{FDIR.factors.VIII.2022}} will be used
#'  
#'  Missing values for WeightFactor are interpreted as NA, and will result in NA for weights after conversion.
#'  
#' @param processData \code{\link[RstoxFDA]{WeightConversionTable}} as returned from this function
#' @param DefinitionMethod 'ResourceFile' or 'FDIR.VIII.2022'. See details.
#' @param FileName path to resource file
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{WeightConversionTable}}
#' @seealso \code{\link[RstoxFDA]{ConvertWeightBiotic}} for applying weight conversion to data.
#' @family parameter conversion functions
#' @export
#' @md
DefineWeightConversionFactor <- function(processData, DefinitionMethod=c("ResourceFile", "FDIR.VIII.2022"), FileName = character(), UseProcessData=F){
  
  if (UseProcessData){
    return(processData)
  }
  
  DefinitionMethod <- match.arg(DefinitionMethod, DefinitionMethod)
  
  if (!isGiven(DefinitionMethod)){
    stop("'DefinitionMethod' must be provided.")
  }
  
  if (DefinitionMethod == "FDIR.VIII.2022"){
    return(RstoxFDA::FDIR.factors.VIII.2022)
  }
  
  encoding <- "UTF-8"
  
  if (DefinitionMethod == "ResourceFile"){
    tab <- readTabSepFile(FileName,
                          col_classes =  c(rep("character", 3), "numeric"),
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

#' List Difference
#' @description 
#'  List the difference between a NMDbiotic v 3.x data set and a StoxBiotic data set. Appropriate when BioticData is read from NMDbiotic.
#' @details 
#'  Lists all missions, fishstations, catchsamples and individuals from a NMDbiotic v 3.x data set
#'  that is not present in a given StoxBiotic data set.
#'  
#'  This may be used to summarize the effect of filters and data conversions.
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data set.
#' @param BioticData 
#'  \code{\link[RstoxData]{BioticData}} that has been read from a NMDbiotic v 3.x file
#' @return
#'  \code{\link[RstoxData]{BioticData}} with data that is not found in 'StoxBioticData'.
#' @family nmdbiotic functions
#' @family data QA functions
#' @export
#' @md
ListBioticDifference <- function(StoxBioticData, BioticData){
  return(bioticDiff(BioticData, StoxBioticData))
}

#' Filter outliers
#' @description 
#'  Removes fish records that fall outside an acceptable age-length region
#'  defined by a von-Bertalanffy growth curve:
#'  
#'  length=Linf(1-exp(-K\*age))\*exp(epsilon); epsilon~N(0,sigma^2)
#'  
#'  with parameters corresponding to arguments to this function.
#' @details 
#'  This function is intended to provide the same filtering that is offered in ECA 3.x and ECA 4.x
#'  for removing outliers based on von-Bertalanffy growth curves, and
#'  function arguments are named to correspond to the naming convention used in ECA.
#'  
#'  Records are removed if the length fall outside the range from:
#'  Linf\*(1-exp(-K\*(AGE)))\*exp(kAu\*sigma)
#'  to
#'  Linf\*(1-exp(-K\*(AGE)))\*exp(-kAl\*sigma)
#'  
#'  Age is provided with a resolutation of 1/12 year, which is approximated
#'  by adding M/12 to the integer age, where M is the number of the month of catch (e.g. 2 for february).
#'  For this reason, 'DateTime' must be provided for all 'Stations' in 'StoxBioticData'
#' 
#'  any records with missing length or age is not removed.
#'  
#'  Note that kAl and kAu are given on a log scale, so that the acceptable region
#'  is not symmetric around the growth curve when kAl=kAu.
#'  
#'  Some parameterizations that have been used for some stocks at IMR:
#'  \describe{
#'   \item{cod.27.1-2 (Gadus morhua)}{Linf=232.98028344, K=0.05284384 sigma=0.16180306 kAl=kAu=4}
#'   \item{had.27.1-2 (Melanogrammus aeglefinus)}{Linf=69.7075536, K=0.2158570, sigma=0.1441575 kAl=kAu=4}
#'   \item{pok.27.1-2 (Pollachius virens)}{Linf=100.5282683, K=0.1392982, sigma=0.0996507 kAl=kAu=4}
#'   \item{pelagic stocks North Atlantic}{Linf=37.2490509 K=0.3128122 sigma=0.009681094 kAl=kAu=4}
#'  }
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} to be filtered
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction (removing Samples, Stations, etc. for which all fish records are removed). Default to FALSE.
#' @param Linf Asymptotic length for the von-Bertalanffy growth curve (in cm)
#' @param K The growth coefficient for the von-Bertalanffy growth curve
#' @param sigma The standard deviation of length for the von-Bertalanffy growth curve (in cm)
#' @param kAl Number of standard deviations (on a log scale) that defines the lower limit of the acceptable region
#' @param kAu Number of standard deviations (on a log scale) that defines the upper limit of the acceptable region. Defaults to the same value as kAl.
#' @return \code{\link[RstoxData]{StoxBioticData}} with individuals outside the acceptable region removed.
#' @seealso \code{\link[RstoxFDA]{FilterWeightLengthOutliersStoxBiotic}}
#' @family data QA functions
#' @export
FilterAgeLengthOutliersStoxBiotic <- function(StoxBioticData, 
                                    FilterUpwards = FALSE,
                                    Linf=numeric(),
                                    K=numeric(),
                                    sigma=numeric(),
                                    kAl=numeric(),
                                    kAu=numeric()){
  
  if (!isGiven(kAu)){
    kAu <- kAl
  }
  if (!isGiven(Linf) | !isGiven(K) | !isGiven(sigma) | !isGiven(kAl)){
    stop("All the parameters 'Linf', 'K', 'sigma', and 'kAl' must be provided.")
  }
  
  #make sure temp columns are not taken.
  stopifnot(!any(c("vonBFilter", "month", "IndividualAgeFractional") %in% names(StoxBioticData$Individual)))
  
  #adjust ages to resolution 1/12 yr
  if (any(is.na(StoxBioticData$Station$DateTime))){
    stop("'DateTime' must be provided for all 'Stations' in 'StoxBioticData'")
  }
  stationinfo <- StoxBioticData$Station
  stationinfo$month <- as.numeric(substr(stationinfo$DateTime, 6,7))
  stationinfo <- stationinfo[,c("CruiseKey", "StationKey", "month")]
  StoxBioticData$Individual <- merge(StoxBioticData$Individual, stationinfo)
  StoxBioticData$Individual$IndividualAgeFractional <- StoxBioticData$Individual$IndividualAge + 1/StoxBioticData$Individual$month
  
  StoxBioticData$Individual$vonBFilter <- filterVonBsigmaMask(StoxBioticData$Individual, 
                                                              Linf,K,sigma,kAl,kAu,
                                                              ageCol="IndividualAgeFractional", 
                                                              lengthCol="IndividualTotalLength")
  
  filterExpression <- list()
  filterExpression$Individual <- c(
    'vonBFilter == TRUE'
  )
  
  StoxBioticData <- RstoxData::FilterStoxBiotic(StoxBioticData, filterExpression, FilterUpwards = FilterUpwards)
  StoxBioticData$Individual$vonBFilter <- NULL
  StoxBioticData$Individual$month <- NULL
  StoxBioticData$Individual$IndividualAgeFractional <- NULL
  return(StoxBioticData)

}

#' Filter length-weight outliers
#' @description 
#'  Removes fish records that fall outside an acceptable weight-length region
#'  defined by the log-linear model
#'  
#'  weight=alfa\*length^beta\*exp(epsilon); epsilon~N(0,sigma^2)
#'  
#'  with alfa=exp(logalfa), logalfa and other parameters corresponding to arguments to this function.
#'  
#' @details 
#'  This function is intended to provide the same filtering that is offered in ECA 3.x and ECA 4.x
#'  for removing outliers based on a log-linear weight-length model, and
#'  function arguments are named to correspond to the naming convention used in ECA.
#'  
#'  Records are removed if their weights that outside the range from:
#'  alfa\*L^beta\*exp(kAu\*sigma)
#'  to
#'  alfa\*L^beta\*exp(-kAl\*sigma)
#' 
#'  any records with missing length or weight is not removed.
#'  
#'  Parameters 'logalfa', 'beta' and 'sigma' must be appropriate for weights recorded in gram,
#'  and lengths recorded in cm.
#'  
#'  Note that kAl and kAu are given on a log scale, so that the acceptable region
#'  is not symmetric around the growth curve when kAl=kAu.
#'  
#'  Some parameterizations that have been used for some stocks at IMR:
#'  \describe{
#'   \item{cod.27.1-2 (Gadus morhua)}{logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl = kAu = 4}
#'   \item{had.27.1-2 (Melanogrammus aeglefinus)}{logalfa = -4.9620, beta = 3.0919, sigma = 0.1282, kAl = kAu = 4}
#'   \item{pok.27.1-2 (Pollachius virens)}{logalfa = -4.7718, beta = 3.0287, sigma = 0.1338, kAl = kAu = 4}
#'  }
#'  
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} to be filtered
#' @param FilterUpwards Whether the filter action will propagate in the upwards direction (removing Samples, Stations, etc. for which all fish records are removed). Default to FALSE.
#' @param logalfa The alfa parameter (on a log scale) for the log-linear model. See details for units.
#' @param beta The beta parameter for the log-linear model. See details for units.
#' @param sigma The standard deviation of weight for the log-linear model. See details for units.
#' @param kAl Number of standard deviations (on a log scale) that defines the lower limit of the acceptable region
#' @param kAu Number of standard deviations (on a log scale) that defines the upper limit of the acceptable region
#' @return \code{\link[RstoxData]{StoxBioticData}} with individuals outside the acceptable region removed.
#' @seealso \code{\link[RstoxFDA]{FilterAgeLengthOutliersStoxBiotic}}
#' @family data QA functions
#' @export
#' @md
FilterWeightLengthOutliersStoxBiotic <- function(StoxBioticData,
                                       FilterUpwards=FALSE,
                                       logalfa=numeric(), beta=numeric(), sigma=numeric(), kAl=numeric(), kAu=numeric()){
  if (!isGiven(kAu)){
    kAu <- kAl
  }
  if (!isGiven(logalfa) | !isGiven(beta) | !isGiven(sigma) | !isGiven(kAl)){
    stop("All the parameters 'logalfa', 'beta', 'sigma', and 'kAl' must be provided.")
  }
  
  #make sure temp columns are not taken.
  stopifnot(!any(c("logLinFilter") %in% names(StoxBioticData$Individual)))
  
  
  StoxBioticData$Individual$logLinFilter <- filterLogLinearMask(StoxBioticData$Individual, 
                                                              logalfa,beta,sigma,kAl,kAu=kAl,
                                                              weightCol="IndividualRoundWeight",
                                                              lengthCol="IndividualTotalLength")
  filterExpression <- list()
  filterExpression$Individual <- c(
    'logLinFilter == TRUE'
  )
  
  StoxBioticData <- RstoxData::FilterStoxBiotic(StoxBioticData, filterExpression, FilterUpwards = FilterUpwards)
  StoxBioticData$Individual$logLinFilter <- NULL
  return(StoxBioticData)
}

#' Load common area definitions
#' @description 
#'  Loads standard area definitions often used in analysis of fisheries.
#' @details
#'  This function has the same effect as \code{\link[RstoxBase]{DefineStratumPolygon}}, but loads
#'  the polygons from the RstoxFDA package, rather than requiring them as a resource file.
#' 
#'  The options available are:
#'  \describe{
#'   \item{"FDIR.2017"}{Main areas defined by the Norwegian Directorate of Fisheries, as they where defined to 2017 inclusive. Derived from \code{\link[RstoxFDA]{mainareaFdir2017}}.}
#'   \item{"FDIR.2018"}{Main areas defined by the Norwegian Directorate of Fisheries, as they have been defined from 2018 inclusive. Derived from \code{\link[RstoxFDA]{mainareaFdir2018}}.}
#'   \item{"ICES.2018"}{ICES areas as they have been defined from 2018 inclusive. Areas are provided with finest available spatial resolution (Sub-area, division, sub-division or unit), and the full naming convention is used (e.g. 27.3.d.27 or 27.4.b). Derived from \code{\link[RstoxFDA]{ICESareas}}.}
#'   \item{"ICES.SubArea.2018"}{ICES areas as they have been defined from 2018 inclusive. Areas are provided as 'sub-areas' where they are defined, and the full naming convention is used (e.g. 27.3). Derived from \code{\link[RstoxFDA]{ICESsubArea}}.}
#'   \item{"ICES.Division.2018"}{ICES areas as they have been defined from 2018 inclusive. Areas are provided as 'divisions' where they are defined, and the full naming convention is used (e.g. 27.3.d). Derived from \code{\link[RstoxFDA]{ICESdivision}}.}
#'   \item{"ICES.SubDivision.2018"}{ICES areas as they have been defined from 2018 inclusive. Areas are provided as 'sub-divisions' where they are defined, and the full naming convention is used (e.g. 27.3.d.27). Derived from \code{\link[RstoxFDA]{ICESsubDivision}}.}
#'   \item{"ICES.Unit.2018"}{ICES areas as they have been defined from 2018 inclusive. Areas are provided as 'unit' where they are defined, and the full naming convention is used (e.g. 27.3.d.28.1). Derived from \code{\link[RstoxFDA]{ICESunit}}.}
#'   \item{"ICES.Rectangles.2018"}{ICES rectangles (e.g. 15C2) as they have been defined from 2018 inclusive. Derived from \code{\link[RstoxFDA]{ICESrectangles}}.}
#'   \item{"NAFO"}{NAFO areas. NAFO naming convention is used. Derived from \code{\link[RstoxFDA]{NAFOareas}}.}
#'   \item{"NAFO.FDIR.2017"}{NAFO areas combined with FDIR.2017. The naming convention of the Norwegian Directorate of fisheries is used.}
#'   \item{"NAFO.FDIR.2018"}{NAFO areas combined with FDIR.2018. The naming convention of the Norwegian Directorate of fisheries is used.}
#'  }
#'  
#'  FDIR.2017 corresponds to the area coding referred to as system 2 at IMR.
#'  FDIR.2018 corresponds to the area coding referred to as system 10 at IMR.
#' @param processData \code{\link[RstoxBase]{StratumPolygon}} as returned from this function
#' @param StrataSystem the strata system to load
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxBase]{StratumPolygon}} with the desired strata definition.
#' @family spatial coding functions
#' @export
#' @md
LoadFdaStratumPolygon <- function(processData, StrataSystem=c("FDIR.2017", "FDIR.2018", "ICES.2018", "ICES.SubArea.2018", "ICES.Division.2018", "ICES.SubDivision.2018", "ICES.Unit.2018", "ICES.Rectangles.2018", "NAFO", "NAFO.FDIR.2017", "NAFO.FDIR.2018"), UseProcessData=F){
  
  if (UseProcessData){
    return(processData)
  }
  
  if (StrataSystem == "FDIR.2017"){
    return(RstoxFDA::mainareaFdir2017)
  }
  if (StrataSystem == "FDIR.2018"){
    return(RstoxFDA::mainareaFdir2018)
  }
  if (StrataSystem == "ICES.2018"){
    areas <- RstoxFDA::ICESareas
    areas$StratumName <- RstoxFDA::ICESareas$Area_Full
    areas <- areas[,"StratumName"]
    return(areas)
  }
  if (StrataSystem == "ICES.SubArea.2018"){
    return(RstoxFDA::ICESsubArea)
  }
  if (StrataSystem == "ICES.Division.2018"){
    return(RstoxFDA::ICESdivision)
  }
  if (StrataSystem == "ICES.SubDivision.2018"){
    return(RstoxFDA::ICESsubDivision)
  }
  if (StrataSystem == "ICES.Unit.2018"){
    return(RstoxFDA::ICESunit)
  }
  if (StrataSystem == "ICES.Rectangles.2018"){
    return(RstoxFDA::ICESrectangles)
  }
  if (StrataSystem == "NAFO"){
    areas <- RstoxFDA::NAFOareas
    areas$StratumName <- areas$nafo_names
    areas <- areas[,"StratumName"]
    return(areas)
  }
  if (StrataSystem == "NAFO.FDIR.2017"){
    fdir <- RstoxFDA::mainareaFdir2017
    nafo <- RstoxFDA::NAFOareas
    nafo$StratumName <- nafo$homr
    nafo <- nafo[,"StratumName"]
    return(rbind(nafo, fdir))
  }
  if (StrataSystem == "NAFO.FDIR.2018"){
    fdir <- RstoxFDA::mainareaFdir2018
    nafo <- RstoxFDA::NAFOareas
    nafo$StratumName <- nafo$homr
    nafo <- nafo[,"StratumName"]
    return(rbind(nafo, fdir))
  }
  
  stop("StrataSystem ", StrataSystem, " not recognized.")
  
}
  