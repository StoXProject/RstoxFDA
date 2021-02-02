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

expect_true(is.ReportRecaData(catchAtAgeReportDecomp))
expect_true(is.ReportRecaData(catchAtAgeReportFlat))

diff <- sum(catchAtAgeReportFlat$RecaReport$CatchAtAge) - sum(catchAtAgeReportDecomp$RecaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlat$RecaReport$CatchAtAge))
expect_lt(reldiff, .001)
expect_equal(length(catchAtAgeReportFlat$AggregationVariables$AggregationVariables), 0)
expect_equal(ncol(catchAtAgeReportFlat$RecaReport), 6)

expect_equal(length(catchAtAgeReportDecomp$AggregationVariables$AggregationVariables), 2)
expect_equal(ncol(catchAtAgeReportDecomp$RecaReport), 8)

#test plusgroup
catchAtAgeReportDecompPlusGr <- ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5)
diff <- sum(catchAtAgeReportDecomp$RecaReport$CatchAtAge) - sum(catchAtAgeReportDecompPlusGr$RecaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportDecompPlusGr$RecaReport$CatchAtAge))
expect_lt(reldiff, .001)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$RecaReport), 40)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$AggregationVariables), 2)

catchAtAgeReportFlatPlusGr <- ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup=5)
diff <- sum(catchAtAgeReportFlat$RecaReport$CatchAtAge) - sum(catchAtAgeReportFlatPlusGr$RecaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlatPlusGr$RecaReport$CatchAtAge))
expect_lt(reldiff, .001)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$RecaReport), 4)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$AggregationVariables), 0)


# Report Mean length
MeanWeightReportDecomp <- ReportRecaWeightAtAge(catchAtAgeDecomp)
expect_true(is.ReportRecaData(MeanWeightReportDecomp))
MeanLengthReportDecomp <- ReportRecaLengthAtAge(catchAtAgeDecomp)
expect_true(is.ReportRecaData(MeanLengthReportDecomp))

