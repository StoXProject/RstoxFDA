
#' @noRd
bioticDiff <- function(biotic, StoxBiotic){
  
  if (!all(unlist(lapply(biotic, function(x){startsWith(x$metadata$useXsd, "nmdbioticv3")})))){
    stoxWarning("not all biotic files look like NMDbiotic v3.x")
  }
  
  if (!startsWith(StoxBiotic$Cruise$CruiseKey[1], "NA/")){
    stop("Unexpected key component. Possibly not NMDbiotic v3.x")
  }
  
  missingMissions <- biotic[[1]]$mission[0,]
  missingStations <- biotic[[1]]$fishstation[0,]
  missingCatchsamples <- biotic[[1]]$catchsample[0,]
  missingIndividuals <- biotic[[1]]$individual[0,]
  missingAgeDeterminations <- biotic[[1]]$agedetermination[0,]
  missingPrey <- biotic[[1]]$prey[0,]
  missingLF <- biotic[[1]]$preylengthfrequencytable[0,]
  missingCD <- biotic[[1]]$copepodedevstagefrequencytable[0,]
  missingTag <- biotic[[1]]$tag[0,]
  
  for (b in biotic){
    missingMissions <- rbind(missingMissions, b$mission[!((paste(b$mission$CruiseKey, sep="/") %in% StoxBiotic$Cruise$CruiseKey)),])
    
    haulKeys <- paste(StoxBiotic$Haul$CruiseKey, StoxBiotic$Haul$StationKey, StoxBiotic$Haul$HaulKey, sep="/")
    fishstationkeys <- paste("NA", 
                             b$fishstation$missiontype, 
                             b$fishstation$startyear,
                             b$fishstation$platform,
                             b$fishstation$missionnumber,
                             b$fishstation$station,
                             b$fishstation$serialnumber,
                             sep="/")
    missingStations <- rbind(missingStations, b$fishstation[!(fishstationkeys %in% haulKeys),])
    
    #merge with fs to get station id
    cn <- names(b$catchsample)
    cs <- merge(b$catchsample, b$fishstation)
    sampleKeys <- paste(StoxBiotic$Sample$CruiseKey, 
                        StoxBiotic$Sample$StationKey, 
                        StoxBiotic$Sample$HaulKey,
                        StoxBiotic$Sample$SampleKey, sep="/")
    catchsampleKeys <- paste("NA", 
                             cs$missiontype, 
                             cs$startyear,
                             cs$platform,
                             cs$missionnumber,
                             cs$station,
                             cs$serialnumber,
                             cs$catchsampleid,
                             sep="/")
    missingCatchsamples <- rbind(missingCatchsamples, cs[!(catchsampleKeys %in% sampleKeys), .SD, .SDcols = cn])
    
    #merge with fs to get station id
    cn <- names(b$individual)
    ind <- merge(b$individual, b$fishstation, all.x=T)
    stoxIndividualKeys <-  paste(StoxBiotic$Individual$CruiseKey, 
                                 StoxBiotic$Individual$StationKey, 
                                 StoxBiotic$Individual$HaulKey,
                                 StoxBiotic$Individual$SampleKey,
                                 StoxBiotic$Individual$IndividualKey,
                                 sep="/")
    individualKeys <- paste("NA", 
                            ind$missiontype, 
                            ind$startyear,
                            ind$platform,
                            ind$missionnumber,
                            ind$station,
                            ind$serialnumber,
                            ind$catchsampleid,
                            ind$specimenid,
                            sep="/")
    
    missingIndividuals <- rbind(missingIndividuals, ind[!(individualKeys %in% stoxIndividualKeys), .SD, .SDcols =cn])
    
    #merge with fs to get station id
    cn <- names(b$agedetermination)
    age <- merge(b$agedetermination, b$fishstation, all.x=T)
    ageKeys <- paste("NA", 
                     age$missiontype, 
                     age$startyear,
                     age$platform,
                     age$missionnumber,
                     age$station,
                     age$serialnumber,
                     age$catchsampleid,
                     age$specimenid,
                            sep="/")
    
    missingAgeDeterminations <- rbind(missingAgeDeterminations, age[!(ageKeys %in% stoxIndividualKeys), .SD, .SDcols =cn])
    
    missingPrey <- rbind(missingPrey, b$prey)
    missingLF <- rbind(missingLF, b$preylengthfrequencytable)
    missingCD <- rbind(missingCD, b$copepodedevstagefrequencytable)
    missingTag <- rbind(missingTag, b$tag)
  }
  
  output <- list()
  output$mission <- missingMissions
  output$fishstation <- missingStations
  output$catchsample <- missingCatchsamples
  output$individual <- missingIndividuals
  output$agedetermination <- missingAgeDeterminations
  output$prey <- missingPrey
  output$preylengthfrequencytable <- missingLF
  output$copepodedevstagefrequencytable <- missingCD
  output$tag <- missingTag

  return(output)
}