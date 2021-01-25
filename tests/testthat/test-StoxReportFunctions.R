context("Test StoxReportFunctions: ReportFdaSampling")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$Quarter <- quarters(StoxBioticData$Station$DateTime)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxLandingData$landings$Quarter <- quarters(StoxLandingData$landings$CatchDate)

SamplingReport <- ReportFdaSampling(StoxBioticData, StoxLandingData, AggregationVariables = c("Quarter"))
expect_true(is.ReportFdaSamplingData(SamplingReport))
expect_true(all(!is.na(SamplingReport$FisheriesSampling$LandedRoundWeight)))

#Default gear is different coding system for stoxbiotic and landing
unlanded <- ReportFdaSampling(StoxBioticData, StoxLandingData)
expect_true(is.data.table(unlanded$AggregationVariables))
expect_true("Gear" %in% unlanded$AggregationVariables$AggregationVariables)
expect_true(any(is.na(unlanded$FisheriesSampling$LandedRoundWeight)))
expect_error(ReportFdaSampling(StoxBioticData, StoxLandingData, AggregationVariables = c("Quarter", "Nonesene")), "All 'AggregationVariables' must be present in 'StoxLandingData'. Missing: Nonesene")

