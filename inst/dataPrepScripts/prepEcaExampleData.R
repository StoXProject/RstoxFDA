#
# obtain example files for testing Reca
#

bioticfile <- "../resource_raw_data/biotic_kolmule_2018.xml"
landingfile <- "../resource_raw_data/landing_excerpt_kolmule_2018.xml"

library(RstoxData)
library(RstoxFDA)
BioticData <- RstoxData::ReadBiotic(bioticfile)

# keep only areas that occur in landings
filterExpression <- list()
filterExpression$`biotic_kolmule_2018.xml`$fishstation <- c(
  '!is.na(area) & area != "49" & !is.na(stationstartdate)'
)
BioticData <- filterData(BioticData, filterExpression, propagateUpwards = T)

# set lon lat based on position where missing
fscopy <- BioticData$biotic_kolmule_2018.xml$fishstation
fscopy <- appendPosition(fscopy, areaPolygons = mainareaFdir2018, latColName = "LAT", lonColName = "LON", areaName = "area")
BioticData$biotic_kolmule_2018.xml$fishstation$latitudestart[is.na(BioticData$biotic_kolmule_2018.xml$fishstation$latitudestart)] <- fscopy$LAT[is.na(BioticData$biotic_kolmule_2018.xml$fishstation$latitudestart)]
BioticData$biotic_kolmule_2018.xml$fishstation$longitudestart[is.na(BioticData$biotic_kolmule_2018.xml$fishstation$longitudestart)] <- fscopy$LON[is.na(BioticData$biotic_kolmule_2018.xml$fishstation$longitudestart)]

# set times for fishstations
BioticData$biotic_kolmule_2018.xml$fishstation$stationstarttime <- "12:00:00.000Z"

StoxBioticData <- RstoxData::StoxBiotic(BioticData)
StoxLandingData <- RstoxData::StoxLanding(RstoxData::ReadLanding(landingfile))

saveRDS(StoxBioticData, "inst/testresources/StoxBioticData.rds")
saveRDS(StoxLandingData, "inst/testresources/StoxLandingData.rds")

#
# run eca and export as example data
#
recaDataExample <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, fixedEffects = c(), randomEffects = c())
recaPrediction <- RunRecaEstimate(prep, 50, 5000, 10)$prediction

usethis::use_data(recaPrediction, overwrite = T)
usethis::use_data(recaDataExample, overwrite = T)


#
# make test data with delprøve
#
bioticfile <- "../resource_raw_data/testdelp.xml"
BioticData <- RstoxData::ReadBiotic(bioticfile)
BioticData$testdelp.xml$fishstation$stationstarttime <- "12:00:00.000Z"
StoxBioticDataDelpr <- RstoxData::StoxBiotic(BioticData)
saveRDS(StoxBioticDataDelpr, "inst/testresources/StoxBioticDelpr.rds")

