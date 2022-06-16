#
# prepares example activity census for metier annotation based on logbooks
#
prepActivityCensus <- function(logbookErs){
  logb <- RstoxData::readErsFile(logbookErs)
  export <- logb[,c("REDSKAP_FAO", "REDSKAP_NS", "HOVEDART_FAO", "FANGSTART_FAO", "MASKEVIDDE", "STØRSTE_LENGDE", "RUNDVEKT")]
  export$LENGDEGRUPPE <- as.character(cut(export$STØRSTE_LENGDE, c(8,10,12,15,18,24,40,200), labels = c("<8","10-<12", "12-<15","15<-18","18-<24","24-<40","40-<")))
  export$LENGDEGRUPPE[is.na(export$LENGDEGRUPPE)]<-"Unkown"
  export$REDSKAP_FAO[export$REDSKAP_FAO == "SND"] <- "SDN" #correct odd Fdir convention
  agg <- export[, list(RUNDVEKT=sum(RUNDVEKT,rm.na=T)), by=c("REDSKAP_FAO", "REDSKAP_NS", "HOVEDART_FAO", "MASKEVIDDE", "LENGDEGRUPPE", "FANGSTART_FAO")]
  names(agg) <- c("gearFAO", "gearNS", "targetFAO", "meshSize", "vesselLengthCategory", "species", "wholeWeightKg")
  activityCensus <- agg
  usethis::use_data(activityCensus, overwrite=T)
}
prepActivityCensus("~/logbooks/FDIR_HI_ERS_2018_PR_2019-03-04.psv")
