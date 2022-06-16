
#' @noRd
bioticDiff <- function(biotic, StoxBiotic){
  
  if (!all(unlist(lapply(biotic, function(x){startsWith(x$metadata$useXsd, "nmdbioticv3")})))){
    stoxWarning("not all biotic files look like NMDbiotic v3.x")
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
    
    cruiseKeys <- b$mission[,.SD, .SDcols=c("missiontype", "startyear", "platform", "missionnumber", "cruise")]
    missingMissions <- rbind(missingMissions, b$mission[!((paste(b$mission$CruiseKey, sep="/") %in% StoxBiotic$Cruise$CruiseKey)),])
    
    fs <- RstoxData::mergeByIntersect(cruiseKeys, b$fishstation)
    cn <- names(b$fishstation)
    haulKeys <- paste(StoxBiotic$Haul$CruiseKey, StoxBiotic$Haul$StationKey, StoxBiotic$Haul$HaulKey, sep="/")
    fishstationkeys <- paste(fs$cruise, 
                             fs$missiontype, 
                             fs$startyear,
                             fs$platform,
                             fs$missionnumber,
                             fs$station,
                             fs$serialnumber,
                             sep="/")
    missingStations <- rbind(missingStations, fs[!(fishstationkeys %in% haulKeys), .SD, .SDcol=cn])
    
    #merge with fs to get station id
    cn <- names(b$catchsample)
    cs <- RstoxData::mergeByIntersect(b$catchsample, fs)
    sampleKeys <- paste(StoxBiotic$Sample$CruiseKey, 
                        StoxBiotic$Sample$StationKey, 
                        StoxBiotic$Sample$HaulKey,
                        StoxBiotic$Sample$SampleKey, sep="/")
    catchsampleKeys <- paste(cs$cruise, 
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
    ind <- RstoxData::mergeByIntersect(b$individual, fs, all.x=T)
    stoxIndividualKeys <-  paste(StoxBiotic$Individual$CruiseKey, 
                                 StoxBiotic$Individual$StationKey, 
                                 StoxBiotic$Individual$HaulKey,
                                 StoxBiotic$Individual$SampleKey,
                                 StoxBiotic$Individual$IndividualKey,
                                 sep="/")
    individualKeys <- paste(ind$cruise, 
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
    age <- RstoxData::mergeByIntersect(b$agedetermination, fs, all.x=T)
    ageKeys <- paste(age$cruise, 
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