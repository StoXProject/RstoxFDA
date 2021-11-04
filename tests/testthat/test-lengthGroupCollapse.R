StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Haul$Gear <- StoxLandingData$Landing$Gear[sample.int(20,45, replace=T)]

fpath <- makeTempDirReca()

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear"))
paramOut <- ParameterizeRecaModels(prep, 10, 50, 1, fpath)

context("RunRecaModels: Test collapse Length")
resultsWlength <- RunRecaModels(paramOut, StoxLandingData, GroupingVariables = "Gear", CollapseLength = F)
results <- RunRecaModels(paramOut, GroupingVariables = "Gear", StoxLandingData)

removeTempDirReca(fpath)

expect_gt(nrow(resultsWlength$CatchAtAge), nrow(results$CatchAtAge))


reportLAAwLength <- ReportRecaLengthAtAge(resultsWlength)
reportLAAwoLength <- ReportRecaLengthAtAge(results)
expect_equal(reportLAAwLength$FdaReport$MeanIndividualLength, reportLAAwoLength$FdaReport$MeanIndividualLength)

reportCAAwLength <- ReportRecaCatchAtAge(resultsWlength)
reportCAAwoLength <- ReportRecaCatchAtAge(results)
expect_equal(reportCAAwLength$FdaReport$CatchAtAge, reportCAAwoLength$FdaReport$CatchAtAge)

reportWAAwLength <- ReportRecaLengthAtAge(resultsWlength)
reportWAAwoLength <- ReportRecaLengthAtAge(results)
expect_equal(reportWAAwLength$FdaReport$MeanIndividualWeight, reportWAAwoLength$FdaReport$MeanIndividualWeight)
