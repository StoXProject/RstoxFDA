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

# Report Mean weight
MeanWeightReportDecomp <- ReportRecaWeightAtAge(catchAtAgeDecomp)
expect_true(is.ReportRecaData(MeanWeightReportDecomp))

# Report Mean weight Plus gr
MeanWeightReportDecompPlusGr <- ReportRecaWeightAtAge(catchAtAgeDecomp, PlusGroup=5)

expect_lt(nrow(MeanWeightReportDecompPlusGr$RecaReport), nrow(MeanWeightReportDecomp$RecaReport))

#ages not in plusgroup should be equal for calculation w and wo plusgroups
expect_equal(MeanWeightReportDecomp$RecaReport$MeanIndividualWeight[MeanWeightReportDecomp$RecaReport$Age<5],
             MeanWeightReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$RecaReport$Age<5])

# mean for plus group should be larger than oldes age not in plus group
expect_true(all(MeanWeightReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$RecaReport$Age==5] >
          MeanWeightReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$RecaReport$Age==4]))

#mean for plusgr should be larger than lowest age in plusgr
expect_true(all(MeanWeightReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$RecaReport$Age==5] >
          MeanWeightReportDecomp$RecaReport$MeanIndividualWeight[MeanWeightReportDecomp$RecaReport$Age==5]))
#mean for plusgr should be smaller than largest age in plusgr
# beware of artifacts for small age groups (convergence or data issues). Using age group 13, rather than 14
expect_true(all(MeanWeightReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$RecaReport$Age==5] <
          MeanWeightReportDecomp$RecaReport$MeanIndividualWeight[MeanWeightReportDecomp$RecaReport$Age==13]))



# Report Mean length
MeanLengthReportDecomp <- ReportRecaLengthAtAge(catchAtAgeDecomp)
expect_true(is.ReportRecaData(MeanLengthReportDecomp))

# Report Mean length Plus gr
MeanLengthReportDecompPlusGr <- ReportRecaLengthAtAge(catchAtAgeDecomp, PlusGroup=5)

expect_lt(nrow(MeanLengthReportDecompPlusGr$RecaReport), nrow(MeanLengthReportDecomp$RecaReport))

#ages not in plusgroup should be equal for calculation w and wo plusgroups
expect_equal(MeanLengthReportDecomp$RecaReport$MeanIndividualWeight[MeanLengthReportDecomp$RecaReport$Age<5],
             MeanLengthReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$RecaReport$Age<5])

# mean for plus group should be larger than oldes age not in plus group
expect_true(all(MeanLengthReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$RecaReport$Age==5] >
                  MeanLengthReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$RecaReport$Age==4]))

#mean for plusgr should be larger than lowest age in plusgr
expect_true(all(MeanLengthReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$RecaReport$Age==5] >
                  MeanLengthReportDecomp$RecaReport$MeanIndividualWeight[MeanLengthReportDecomp$RecaReport$Age==5]))
#mean for plusgr should be smaller than largest age in plusgr
# beware of artifacts for small age groups (convergence or data issues). Using age group 13, rather than 14
expect_true(all(MeanLengthReportDecompPlusGr$RecaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$RecaReport$Age==5] <
                  MeanLengthReportDecomp$RecaReport$MeanIndividualWeight[MeanLengthReportDecomp$RecaReport$Age==13]))




browser()
# Report SOP
ReportFdaSOP(catchAtAgeReportDecompPlusGr, )

