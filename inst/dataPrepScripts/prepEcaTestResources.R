#' example results for testing reporting
fpath <- makeTempDirReca()
param <- ParameterizeRecaModels(prep, 10, 100, ResultDirectory = fpath)
recaParam <- RstoxFDA::ParameterizeRecaModels(recaDataExample, 10, 200, 10, ResultDirectory = fpath)
recaPredictionFlat <- RstoxFDA::RunRecaModels(recaParam, StoxLandingData)
recaPredictionFlatAggvar <- RstoxFDA::RunRecaModels(recaParam, StoxLandingData, GroupingVariables = c("Gear", "Area"))
saveRDS(recaPredictionFlat, "inst/testresources/recaPredictionFlat.rds")
saveRDS(recaPredictionFlatAggvar, "inst/testresources/recaPredictionDecomp.rds")
removeTempDirReca(fpath)

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


#
# prep example data for report of stock splitted predictions
#
exampleProject <- "~/workspace/stox3examples/StoxRecaCoastalCodMini"
RstoxAPI::runModel(projectPath = exampleProject, modelName = "baseline")
ss<-RstoxAPI::runModel(projectPath = exampleProject, modelName = "analysis")
saveRDS(ss$PreredictAreaQuarter, "inst/testresources/stocksplitpred.rds")
