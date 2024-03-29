#
# regenerate example files used in unit tests (testthat)
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
recaDataExample <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())
recaPrediction <- RstoxFDA:::RunRecaEstimate(recaDataExample, 50, 5000, 10)$prediction

usethis::use_data(recaPrediction, overwrite = T)
usethis::use_data(recaDataExample, overwrite = T)

#' example results for testing reporting
fpath <- RstoxFDA:::makeTempDirReca()
recaParam <- RstoxFDA::ParameterizeRecaModels(recaDataExample, 10, 5000, 10, ResultDirectory = fpath)
recaPredictionFlat <- RstoxFDA::RunRecaModels(recaParam, StoxLandingData)
recaPredictionFlatAggvar <- RstoxFDA::RunRecaModels(recaParam, StoxLandingData, GroupingVariables = c("Gear", "Area"))
saveRDS(recaPredictionFlat, "inst/testresources/recaPredictionFlat.rds")
saveRDS(recaPredictionFlatAggvar, "inst/testresources/recaPredictionDecomp.rds")
RstoxFDA:::removeTempDirReca(fpath)

#
# make test data with delprøve
#
bioticfile <- "../resource_raw_data/testdelp.xml"
BioticData <- RstoxData::ReadBiotic(bioticfile)
BioticData$testdelp.xml$fishstation$stationstarttime <- "12:00:00.000Z"
StoxBioticDataDelpr <- RstoxData::StoxBiotic(BioticData)
saveRDS(StoxBioticDataDelpr, "inst/testresources/StoxBioticDelpr.rds")


#
# prep example for result conversion
#

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData <- AddStratumStoxBiotic(StoxBioticData, StratumPolygon = mainareaFdir2018)
StoxBioticData$Station$Stratum[StoxBioticData$Station$Stratum !="43"] <- "4"
StoxBioticData$Station$Stratum[StoxBioticData$Station$Stratum !="4"] <- "3"
StoxBioticData$Station$Quarter <- quarters(StoxBioticData$Station$DateTime)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxLandingData$Landing$Stratum <- substr(StoxLandingData$Landing$Area,1,1)
StoxLandingData$Landing$Stratum[!(StoxLandingData$Landing$Stratum %in% c("3","4"))] <- "4"
StoxLandingData$Landing$Quarter <- quarters(StoxLandingData$Landing$CatchDate)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Stratum", "Quarter"))
est <- RstoxFDA:::RunRecaEstimate(prep, 10, 5000, 1)
saveRDS(est, "inst/testresources/ecaResult.rds")

