
library(RstoxData)
context("ParameterizeRecaModels: simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())
checkEcaObj(prep)

paramOut <- ParameterizeRecaModels(prep, 10, 50, 1, "~/temp/ecatest")
expect_true(c("FitLengthGivenAge") %in% names(paramOut))
expect_equal(length(paramOut$FitLengthGivenAge), 4)
expect_true(is.RecaParameterData((paramOut)))

results <- RunRecaModels(paramOut)
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(is.RecaCatchAtAge(results))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate missing arguments")
expect_error(ParameterizeRecaModels(prep, 10, 50, 1, "~/temp/ecatest", Lgamodel = NULL), "Parameter 'Lgamodel' must be provided.")


context("test-StoxAnalysisFunctions: PrepareRecaEstimate simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())
checkEcaObj(prep)
expect_equal(length(prep$CovariateMaps$randomEffects$AgeLength$CatchId), length(unique(StoxBioticData$Individual$HaulKey)))


prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), MinAge=1, MaxAge=30)

context("test-StoxAnalysisFunctions: RunRecaEstimate simple case")
result <- RunRecaEstimate(prep, 10, 50, Thin=1)
expect_true(all(c("input", "fit", "prediction", "covariateMaps") %in% names(result)))
expect_equal(dim(result$prediction$TotalCount)[3], 10)


context("test-StoxAnalysisFunctions: PrepareRecaEstimate, missing sample dates")
StoxBioticData$Station$DateTime[1] <- NA
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c()))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate, stratified samples (nFish), missing CatchFractionCount")
StoxBioticDataDelp <- readRDS(system.file("testresources","StoxBioticDelpr.rds", package="RstoxFDA"))
expect_error(PrepareRecaEstimate(StoxBioticDataDelp, StoxLandingData, FixedEffects = c(), RandomEffects = c()))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate, stratified samples (nFish)")
StoxBioticDataDelp$Sample$CatchFractionCount[2] <- 3000
prep <- PrepareRecaEstimate(StoxBioticDataDelp, StoxLandingData, FixedEffects = c(), RandomEffects = c())

context("test-StoxAnalysisFunctions: RunRecaEstimate, stratified samples (nFish)")
est <- RunRecaEstimate(prep, 10, 50, 0)

context("test-StoxAnalysisFunctions: PrepareRecaEstimate with  with random effect Area")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData <- AddStratumStoxBiotic(StoxBioticData, StratumPolygon = mainareaFdir2018)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxLandingData$landings$Stratum <- StoxLandingData$landings$Area

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Stratum"))
expect_true("Stratum" %in% names(prep$Landings$AgeLengthCov))

context("test-StoxAnalysisFunctions: RunRecaEstimate with random effect Area")
est <- RunRecaEstimate(prep, 10, 50, 0)
expect_true("Stratum" %in% names(est$fit$ProportionAtAge$Intercept$cov))

context("RunRecaEstimate not providing burnin")
expect_error(RunRecaEstimate(prep, 10))




