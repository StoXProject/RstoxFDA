#
# Prepares example for catch lotteri design parameters
#

#' Reads catch lottery parameters from file as exported by lottery system Pr oct 2023.
#' @param filename path to file with lottery parameters
#' @return ~\code{\link[data.table]{data.table}} with colmns:
#'  \describe{
#'  \item{aar}{year (integer)}
#'  \item{RC}{Radio call signal of vessel (character)}
#'  \item{SQ}{serialnumber for message (integer) identifies message given year, vessel and message recipient}
#'  \item{TM}{ERS message type (character). HIA means departure message sent to IMR catch lottery (determines target species and therefor inclusion in lottery). HIF means catch message sent to IMR catch lottery, also encoding wheter sample is requested, along with lottery parameters}
#'  \item{BD}{Date for start of fishing operation in UTC: YYYYMMDD}
#'  \item{BT}{Tome for start of fishing operation in UTC: HHMM}
#'  \item{Svar}{Code for whether sample is requested. Code 641 means that a sample is requested. 642 means sample is not requested. 643 means no (more) samples will be requested from this trip (until ned departure message).}
#'  \item{i.prob}{Inclusion probability used in sample selection. Assigned also to samples not selected}
#'  \item{lotteri}{Identifier for lottery, sampling frame, all inclusion probabilities are conditioned only on catch being in lottery}
#'  \item{HIF.stratum}{Any stratification used in setting sampling parameters. Stratification is already factored into inclusion probabilities, some other column that goes into inclusion prob calculation depends on HIF.stratum (e.g. kapasitet)}
#'  \item{kvote}{quota / expected total catch, used in calculation of inclusion probabilities}
#'  \item{kapasitet}{sampling capacity / expected number of samples, used in calculation of inclusion probabilities}
#'  \item{lotteri.kg}{reported catch in kg that was used in calculation of inclusion probability}
#'  
#'  The fields RC, SQ, TM, BC and BT are defined in the ERS regulation (https://lovdata.no/dokument/SF/forskrift/2009-12-21-1743).
#'  }
#' @noRd
parseLotteryFile <- function(filename){
  lotteriparams <- data.table::fread(filename, sep = "\t", dec=".", header = T, colClasses = c("integer", "character", "integer", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"))
  return(lotteriparams)
}

#' Prepares PSU design parameters from catch lottery data and associates them with serial number.
#' @description 
#'  Vessels are not always compliant about time-zone usage. Catches are therefore associated with the closest lottery message that does not exceed a difference of 'maxDiff' hours.
#' @param lotteryParams lottery parameters as read by parseLotteryFile
#' @param StoxBiotic ~\code{\link[RstoxData]{StoxBioticData}} with hauls sampled by catch lottery
#' @param platformCodes table relating platform codes to other identifiers. As downloaded from the platform table in NMD reference. Contains columns: "Platformnumber", "syscode", "sysname", "Value", "Description", "Deprecated", "Valid from", "Valid to, "New code"
#' @param maxDiff the highest acceptable difference in time (hours) between StoxBiotic station time and lottery BD and BT time.
#' @noRd
prepDesignParamFile <- function(lotteryParams, StoxBiotic, platformCodes, maxDiff=2){
  #partition lottery messages into set where sample was requested and other set
  lotteryParamsFiltered <- lotteryParams
  lotteryParamsFiltered$lotteryMessage <- paste(lotteryParamsFiltered$RC, lotteryParamsFiltered$BD, lotteryParamsFiltered$BT)
  lotteryParamsFiltered$lotteryDateTime <- as.POSIXct(paste(lotteryParamsFiltered$BD,lotteryParamsFiltered$BT, sep=""), format="%Y%m%d%H%M", tz="UTC")
  sampleRequested <- lotteryParamsFiltered[lotteryParamsFiltered$Svar=="641",]
  sampleNotRequested <- lotteryParamsFiltered[lotteryParamsFiltered$Svar=="642",]
  sampleNotRequestedTrip <- lotteryParamsFiltered[lotteryParamsFiltered$Svar=="631",]
  
  stationTable <- merge(StoxBiotic$Station, StoxBiotic$Haul)

  # annotate biotic data with the RC that was valid at the time of catch.
  ITU <- data.table::data.table(platformCodes[platformCodes$sysname == "ITU Call Sign",])
  ITU$RC <- ITU$Value
  ITU <- ITU[,.SD,.SDcol=c("Platformnumber", "RC", "Valid to")]
  ITU <- merge(ITU, stationTable[,.SD, .SDcol=c("CatchPlatform", "DateTime")], by.x="Platformnumber", by.y="CatchPlatform")
  ITU <- ITU[ITU$`Valid to`>=ITU$DateTime,]
  ITU <- ITU[order(ITU$`Valid to`, decreasing = T),]
  ITU <- ITU[!duplicated(paste(ITU$Platformnumber, ITU$DateTime)),]
  
  stationTable <- merge(stationTable, ITU[,.SD, .SDcol=c("Platformnumber", "RC", "DateTime")], by.x=c("CatchPlatform", "DateTime"), by.y=c("Platformnumber", "DateTime"), all.x=T)
  
  if (any(is.na(stationTable$RC))){
    stop("Could not find ITU call signal for all platforms, for the given dates.")
  }
  
  stationTable <- merge(stationTable, sampleRequested, by="RC", all.y=T)
  stationTable$timeDiff <- abs(difftime(stationTable$DateTime, stationTable$lotteryDateTime, unit="h"))
  stationTable <- stationTable[is.na(stationTable$timeDiff) | stationTable$timeDiff<=maxDiff,]
  stationTable <- stationTable[order(stationTable$timeDiff, decreasing=F),]
  stationTable <- stationTable[is.na(stationTable$timeDiff) | !duplicated(stationTable$lotteryMessage),]

  if (!all(StoxBiotic$Haul$HaulKey %in% stationTable$HaulKey)){
    missing <- StoxBiotic$Haul[!(StoxBiotic$Haul$HaulKey %in% stationTable$HaulKey),]
    warning(paste(nrow(missing), " samples are not requested from lottery and omitted from Design Parameters table."))
  }
  
  stationTable$description <- paste(stationTable$lotteri, stationTable$HIF.stratum, stationTable$lotteryMessage, sep="/")
  stationTable$SelectionProbability <- stationTable$lotteri.kg/(stationTable$kvoteT*1000)  
  stationTable$HHsamplingWeight <- 1 / (stationTable$SelectionProbability * sum(1/stationTable$SelectionProbability))
  selectionTable <- stationTable[,.SD, .SDcol=c("HIF.stratum", "HaulKey", "i.prob", "SelectionProbability", "HHsamplingWeight", "kapasitet", "description")]
  names(selectionTable) <- c("Stratum", "SamplingUnitId", "InclusionProbability", "SelectionProbability", "HHsamplingWeight", "n", "SelectionDescription")
  if (length(unique(stationTable$kapasitet))!=1){
    selectionTable$SelectionProbability <- as.numeric(NA)  
    stationTable$HHsamplingWeight <- as.numeric(NA)
  }
  selectionTable$Order <- as.numeric(NA)
  selectionTable$HTsamplingWeight <- 1 / (selectionTable$InclusionProbability * sum(1/selectionTable$InclusionProbability))
  selectionTable <- selectionTable[, .SD, .SDcol=c("Stratum", "Order", "SamplingUnitId", "InclusionProbability", "HTsamplingWeight", "SelectionProbability", "HHsamplingWeight", "SelectionDescription")]
  selectionTable$SelectionDescription <- as.character(NA) #remove vessel identifying descriptions
  
  stopifnot(length(unique(stationTable$HIF.stratum))==1)  
  sampleTable <- data.table::data.table(Stratum = stationTable$HIF.stratum[[1]], N = sum(!is.na(lotteryParams$i.prob) & lotteryParams$i.prob>0))
  sampleTable$n <- sum(lotteryParams$Svar=="641")
  sampleTable$SelectionMethod <- "Poisson"
  stopifnot(length(unique(stationTable$lotteri))==1)
  sampleTable$FrameDescription <- stationTable$lotteri[[1]]
  
  designTable <- list()
  designTable$sampleTable <- sampleTable
  designTable$selectionTable <- selectionTable
  
  return(designTable)
}

#' Save design table
#' @noRd
saveDesignTable <- function(filename, designTable){
  data.table::fwrite(file = filename, x=merge(designTable$sampleTable, designTable$selectionTable), sep = "\t")
}

lotteryParams <- parseLotteryFile("~/hi_sync/fiskerisampling/fangstprøvelotteri/lotterifiler/example2022.txt")
lotteryParams <- lotteryParams[lotteryParams$lotteri=="Sild2022" & lotteryParams$HIF.stratum=="Nordsjo",]

bioData <- RstoxData::StoxBiotic(RstoxData::ReadBiotic("~/bioticsets/lotterieksempel/biotic_cruiseNumber_19-2022-20_Silde-sampling_2023-07-06T22.00.19.567Z.xml"))
platformCodes <- readxl::read_excel("~/codelists/NMDeksempler/platform.xlsx", 2)

designParams <- prepDesignParamFile(lotteryParams, bioData, platformCodes)
designParamsFile <- "inst/testresources/lotteryParameters/lotteryDesignNSH.txt"
saveDesignTable(designParamsFile, designParams)

#remove potential vessel identifying information
bioData$Station$CatchPlatform <- as.character(NA)
CatchLotteryExample <- bioData
#fix missing catchfractionnumber
filter <- is.na(CatchLotteryExample$Sample$CatchFractionNumber)
CatchLotteryExample$Sample$CatchFractionNumber[filter] <- CatchLotteryExample$Sample$CatchFractionWeight[filter]*CatchLotteryExample$Sample$SampleNumber[filter] / CatchLotteryExample$Sample$SampleWeight[filter]
usethis::use_data(CatchLotteryExample, overwrite = T)

CatchLotterySamplingExample <- RstoxFDA::DefinePSUSamplingParameters(NULL, "ResourceFile", designParamsFile)
usethis::use_data(CatchLotterySamplingExample, overwrite = T)