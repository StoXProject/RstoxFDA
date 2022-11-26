StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Haul$Gear <- StoxLandingData$Landing$Gear[sample.int(20,45, replace=T)]

fpath <- RstoxFDA:::makeTempDirReca()

prep <- RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear"))
paramOut <- RstoxFDA::ParameterizeRecaModels(prep, 10, 50, 1, fpath)

#context("RunRecaModels: Test collapse Length")
resultsWlength <- RstoxFDA::RunRecaModels(paramOut, StoxLandingData, GroupingVariables = "Gear", CollapseLength = F)
results <- RstoxFDA::RunRecaModels(paramOut, GroupingVariables = "Gear", StoxLandingData,CollapseLength = T)

RstoxFDA:::removeTempDirReca(fpath)

expect_true(nrow(resultsWlength$CatchAtAge) > nrow(results$CatchAtAge))

reportLAAwLength <- RstoxFDA::ReportRecaLengthAtAge(resultsWlength)
reportLAAwoLength <- RstoxFDA::ReportRecaLengthAtAge(results)
expect_equal(reportLAAwLength$FdaReport$MeanIndividualLength, reportLAAwoLength$FdaReport$MeanIndividualLength)

reportCAAwLength <- RstoxFDA::ReportRecaCatchAtAge(resultsWlength)
reportCAAwoLength <- RstoxFDA::ReportRecaCatchAtAge(results)
expect_equal(reportCAAwLength$FdaReport$CatchAtAge, reportCAAwoLength$FdaReport$CatchAtAge)

reportWAAwLength <- RstoxFDA::ReportRecaLengthAtAge(resultsWlength)
reportWAAwoLength <- RstoxFDA::ReportRecaLengthAtAge(results)
expect_equal(reportWAAwLength$FdaReport$MeanIndividualWeight, reportWAAwoLength$FdaReport$MeanIndividualWeight)

reportCALwLength <- RstoxFDA::ReportRecaCatchAtLength(resultsWlength)
reportCALwoLength <- RstoxFDA::ReportRecaCatchAtLength(results)
expect_equal(nrow(reportCALwLength$NbyLength), 166)
expect_equal(nrow(reportCALwoLength$NbyLength), 2)
expect_equal(sum(reportCALwoLength$NbyLength$CatchAtLength), sum(reportCALwoLength$NbyLength$CatchAtLength))

reportCAALwLength <- RstoxFDA::ReportRecaCatchAtLengthAndAge(resultsWlength)
reportCAALwoLength <- RstoxFDA::ReportRecaCatchAtLengthAndAge(results)
expect_equal(nrow(reportCAALwLength$NbyLength), 2158)
expect_equal(nrow(reportCAALwoLength$NbyLength), 26)
expect_equal(sum(reportCAALwoLength$NbyLength$CatchAtLength), sum(reportCAALwLength$NbyLength$CatchAtLength))
