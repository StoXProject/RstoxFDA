context("test-StoxAnalysisFunctions: RunRecaEstimate")

library(RstoxData)
context("test-StoxAnalysisFunctions: PrepareRecaEstimate simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, fixedEffects = c(), randomEffects = c())
checkEcaObj(prep)
expect_equal(length(prep$CovariateMaps$randomEffects$AgeLength$CatchId), length(unique(StoxBioticData$Individual$HaulKey)))

context("test-StoxAnalysisFunctions: RunRecaEstimate simple case")
result <- RunRecaEstimate(prep, 10, 50, thin=1)
expect_true(all(c("input", "fit", "prediction", "covariateMaps") %in% names(result)))
expect_equal(dim(result$prediction$TotalCount)[3], 10)


context("test-StoxAnalysisFunctions: PrepareRecaEstimate, missing sample dates")
StoxBioticData$Station$DateTime[1] <- NA
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, fixedEffects = c(), randomEffects = c()))


context("test-StoxAnalysisFunctions: PrepareRecaEstimate with  with random effect Area")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData <- AddStratumStoxBiotic(StoxBioticData, StratumPolygon = mainareaFdir2018, columnName = "Area")

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, fixedEffects = c(), randomEffects = c("Area"))
expect_true("Area" %in% names(prep$Landings$AgeLengthCov))

context("test-StoxAnalysisFunctions: RunRecaEstimate with random effect Area")
est <- RunRecaEstimate(prep, 10, 50, 0)
expect_true("Area" %in% names(est$fit$ProportionAtAge$Intercept$cov))

