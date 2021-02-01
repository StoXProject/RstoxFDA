context("Test StoxReportFunctions: ReportFdaSampling")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$Quarter <- quarters(StoxBioticData$Station$DateTime)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxLandingData$Landing$Quarter <- quarters(StoxLandingData$Landing$CatchDate)

SamplingReport <- ReportFdaSampling(StoxBioticData, StoxLandingData, AggregationVariables = c("Quarter"))
expect_true(is.ReportFdaSamplingData(SamplingReport))
expect_true(all(!is.na(SamplingReport$FisheriesSampling$LandedRoundWeight)))

#Default gear is different coding system for stoxbiotic and landing
unlanded <- ReportFdaSampling(StoxBioticData, StoxLandingData)
expect_true(is.data.table(unlanded$AggregationVariables))
expect_true("Gear" %in% unlanded$AggregationVariables$AggregationVariables)
expect_true(any(is.na(unlanded$FisheriesSampling$LandedRoundWeight)))
expect_error(ReportFdaSampling(StoxBioticData, StoxLandingData, AggregationVariables = c("Quarter", "Nonesene")), "All 'AggregationVariables' must be present in 'StoxLandingData'. Missing: Nonesene")


# Report Catch at age
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

catchAtAgeReportDecomp <- ReportRecaCatchAtAge(catchAtAgeDecomp)
catchAtAgeReportFlat <- ReportRecaCatchAtAge(catchAtAgeFlat)
expect_true(is.ReportRecaCatchAtAgeData(catchAtAgeReportDecomp))
expect_true(is.ReportRecaCatchAtAgeData(catchAtAgeReportFlat))

diff <- sum(catchAtAgeReportFlat$CatchAtAge$CatchAtAge) - sum(catchAtAgeReportDecomp$CatchAtAge$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlat$CatchAtAge$CatchAtAge))
expect_lt(reldiff, .001)
expect_equal(length(catchAtAgeReportFlat$AggregationVariables$AggregationVariables), 0)
expect_equal(ncol(catchAtAgeReportFlat$CatchAtAge), 5)

expect_equal(length(catchAtAgeReportDecomp$AggregationVariables$AggregationVariables), 2)
expect_equal(ncol(catchAtAgeReportDecomp$CatchAtAge), 7)

