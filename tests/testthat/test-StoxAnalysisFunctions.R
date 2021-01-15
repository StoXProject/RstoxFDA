context("test-StoxAnalysisFunctions: RunRecaEstimate")
data(recaDataExample)
result <- RunRecaEstimate(recaDataExample, 100, 100, thin=1)
expect_true(all(c("input", "fit", "prediction", "covariateMaps") %in% names(result)))
expect_equal(dim(result$prediction$TotalCount)[3], 100)

library(RstoxData)
context("test-StoxAnalysisFunctions: PrepareRecaEstimate")
files <- system.file("testresources","biotic_v3_example.xml", package="RstoxFDA")
inputdata <- RstoxData::ReadBiotic(files)
StoxBioticData <- RstoxData::StoxBiotic(inputdata)
StoxBioticData$Individual <- StoxBioticData$Individual[!is.na(StoxBioticData$Individual$IndividualTotalLength),]

stoxLandingXml <- system.file("testresources","landing.xml", package="RstoxFDA")
StoxLandingData <- RstoxData::StoxLanding(RstoxData::ReadLanding(stoxLandingXml))

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, fixedEffects = c(), randomEffects = c())
checkEcaObj(prep)
expect_equal(length(prep$CovariateMaps$randomEffects$AgeLength$CatchId), length(unique(StoxBioticData$Individual$HaulKey)))
#example contains only one station with lengthmeas
expect_equal(prep$CovariateMaps$randomEffects$AgeLength$CatchId[[1]]$catchId, StoxBioticData$Individual$HaulKey[[1]])