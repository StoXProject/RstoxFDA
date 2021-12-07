
context("Test StoxReportFunctions: ReportRecaParameterStatistics")
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Individual <- StoxBioticData$Individual[StoxBioticData$Individual$IndividualAge<4,]
StoxBioticData$Haul$Gear <- StoxLandingData$Landing$Gear[c(1:20, 1:20, 1:5)]
StoxBioticData$Station$Area <- StoxLandingData$Landing$Area[c(1:20, 1:20, 1:5)]
prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear", "Area"), CellEffect = c("All"), MinAge = 2, MaxAge = 3)

fpath1 <- makeTempDirReca("chain1")
fpath2 <- makeTempDirReca("chain2")
fpath3 <- makeTempDirReca("chain3")

paramOut1 <- ParameterizeRecaModels(prep, 10, 50, 1, ResultDirectory = fpath1)
paramOut2 <- ParameterizeRecaModels(prep, 10, 50, 1, ResultDirectory = fpath2)

paramSummary <- ReportRecaParameterStatistics(paramOut1, NULL)
paramSummary <- ReportRecaParameterStatistics(paramOut2, paramSummary)
expect_true(is.ParameterizationSummaryData(paramSummary))

removeTempDirReca(fpath1)
removeTempDirReca(fpath2)

convergence <- ReportParameterConvergence(paramSummary)
expect_true(is.ParameterConvergenceData(convergence))
expect_true(nrow(convergence$ConvergenceReport) < 433)
expect_true(nrow(convergence$ConvergenceReport) > 0)

#construct three identical chains, should signal convergence
paramSummary <- ReportRecaParameterStatistics(paramOut1, NULL)
paramOut1$GlobalParameters$GlobalParameters$resultdir="B"
paramSummary <- ReportRecaParameterStatistics(paramOut1, paramSummary)
paramOut1$GlobalParameters$GlobalParameters$resultdir="C"
paramSummary <- ReportRecaParameterStatistics(paramOut1, paramSummary)

context("Check Gelman-Rubin for equal chains")
convergence <- ReportParameterConvergence(paramSummary, Tolerance = 0)
expect_equal(nrow(convergence$ConvergenceReport), 433)
expect_true(all(abs(convergence$ConvergenceReport$GelmanRubinR-1)<.1))


context("Test StoxReportFunctions: ReportRecaCatchStatistics")
predictiondatafile <- system.file("testresources","stocksplitpred.rds", package="RstoxFDA")
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(predictiondatafile)

catchReportDecomp <- ReportRecaCatchStatistics(catchAtAgeDecomp)
expect_true(!any(duplicated(catchReportDecomp$MeanAge$MeanIndividualAge)))
expect_equal(nrow(catchReportDecomp$MeanAge), nrow(catchReportDecomp$MeanWeight))
expect_equal(nrow(catchReportDecomp$MeanAge), nrow(catchReportDecomp$MeanLength))
expect_equal(nrow(catchReportDecomp$TotalWeight), nrow(catchReportDecomp$MeanLength))
expect_equal(nrow(catchReportDecomp$TotalNumber), nrow(catchReportDecomp$MeanLength))
expect_equal(nrow(catchReportDecomp$GroupingVariables), 3)
expect_true(all(((catchReportDecomp$TotalWeight$TotalWeight / catchReportDecomp$TotalNumber$TotalNumber) - catchReportDecomp$MeanWeight$MeanIndividualWeight) <.02))
catchReportFlat <- ReportRecaCatchStatistics(catchAtAgeFlat)
expect_true(nrow(catchReportFlat$MeanAge) == 1)
expect_true(nrow(catchReportFlat$MeanWeight) == 1)
expect_true(nrow(catchReportFlat$MeanLength) == 1)
expect_true(nrow(catchReportFlat$TotalWeight) == 1)
expect_true(nrow(catchReportFlat$TotalNumber) == 1)
expect_true(nrow(catchReportFlat$GroupingVariables) == 0)

context("Test StoxReportFunctions: ReportFdaSampling")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$Quarter <- quarters(StoxBioticData$Station$DateTime)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxLandingData$Landing$Quarter <- quarters(StoxLandingData$Landing$CatchDate)

SamplingReport <- ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Quarter"))
expect_true(is.ReportFdaSamplingData(SamplingReport))
expect_true(all(!is.na(SamplingReport$FisheriesSampling$LandedRoundWeight)))

#Default gear is different coding system for stoxbiotic and landing
unlanded <- ReportFdaSampling(StoxBioticData, StoxLandingData)
expect_true(is.data.table(unlanded$GroupingVariables))
expect_true("Gear" %in% unlanded$GroupingVariables$GroupingVariables)
expect_true(any(is.na(unlanded$FisheriesSampling$LandedRoundWeight)))
expect_error(ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Quarter", "Nonesene")), "All 'GroupingVariables' must be present in 'StoxLandingData'. Missing: Nonesene")


# Report Catch at age
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

catchAtAgeReportDecomp <- ReportRecaCatchAtAge(catchAtAgeDecomp)
catchAtAgeReportFlat <- ReportRecaCatchAtAge(catchAtAgeFlat)

expect_true(is.ReportFdaByAgeData(catchAtAgeReportDecomp))
expect_true(is.ReportFdaByAgeData(catchAtAgeReportFlat))

diff <- sum(catchAtAgeReportFlat$FdaReport$CatchAtAge) - sum(catchAtAgeReportDecomp$FdaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlat$FdaReport$CatchAtAge))

expect_lt(reldiff, .001)
expect_equal(length(catchAtAgeReportFlat$GroupingVariables$GroupingVariables), 0)
expect_equal(ncol(catchAtAgeReportFlat$FdaReport), 6)

expect_equal(length(catchAtAgeReportDecomp$GroupingVariables$GroupingVariables), 2)
expect_equal(ncol(catchAtAgeReportDecomp$FdaReport), 8)

#test plusgroup
catchAtAgeReportDecompPlusGr <- ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5)
diff <- sum(catchAtAgeReportDecomp$FdaReport$CatchAtAge) - sum(catchAtAgeReportDecompPlusGr$FdaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportDecompPlusGr$FdaReport$CatchAtAge))
expect_lt(reldiff, .001)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$FdaReport), 40)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$GroupingVariables), 2)

catchAtAgeReportFlatPlusGr <- ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup=5)
diff <- sum(catchAtAgeReportFlat$FdaReport$CatchAtAge) - sum(catchAtAgeReportFlatPlusGr$FdaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlatPlusGr$FdaReport$CatchAtAge))
expect_lt(reldiff, .001)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$FdaReport), 4)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$GroupingVariables), 0)

# Report Mean weight
MeanWeightReportDecomp <- ReportRecaWeightAtAge(catchAtAgeDecomp)
expect_true(is.ReportFdaByAgeData(MeanWeightReportDecomp))

# Report Mean weight Plus gr
MeanWeightReportDecompPlusGr <- ReportRecaWeightAtAge(catchAtAgeDecomp, PlusGroup=5)

expect_lt(nrow(MeanWeightReportDecompPlusGr$FdaReport), nrow(MeanWeightReportDecomp$FdaReport))

#ages not in plusgroup should be equal for calculation w and wo plusgroups
expect_equal(MeanWeightReportDecomp$FdaReport$MeanIndividualWeight[MeanWeightReportDecomp$FdaReport$Age<5],
             MeanWeightReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$FdaReport$Age<5])

# mean for plus group should be larger than oldes age not in plus group
expect_true(all(MeanWeightReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$FdaReport$Age==5] >
          MeanWeightReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$FdaReport$Age==4]))

#mean for plusgr should be larger than lowest age in plusgr
expect_true(all(MeanWeightReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$FdaReport$Age==5] >
          MeanWeightReportDecomp$FdaReport$MeanIndividualWeight[MeanWeightReportDecomp$FdaReport$Age==5]))
#mean for plusgr should be smaller than largest age in plusgr
# beware of artifacts for small age groups (convergence or data issues). Using age group 13, rather than 14

expect_true(all(MeanWeightReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanWeightReportDecompPlusGr$FdaReport$Age==5] <
          MeanWeightReportDecomp$FdaReport$MeanIndividualWeight[MeanWeightReportDecomp$FdaReport$Age==13]))



# Report Mean length
MeanLengthReportDecomp <- ReportRecaLengthAtAge(catchAtAgeDecomp)
expect_true(is.ReportFdaByAgeData(MeanLengthReportDecomp))

# Report Mean length Plus gr
MeanLengthReportDecompPlusGr <- ReportRecaLengthAtAge(catchAtAgeDecomp, PlusGroup=5)

expect_lt(nrow(MeanLengthReportDecompPlusGr$FdaReport), nrow(MeanLengthReportDecomp$FdaReport))

#ages not in plusgroup should be equal for calculation w and wo plusgroups
expect_equal(MeanLengthReportDecomp$FdaReport$MeanIndividualWeight[MeanLengthReportDecomp$FdaReport$Age<5],
             MeanLengthReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$FdaReport$Age<5])

# mean for plus group should be larger than oldes age not in plus group
expect_true(all(MeanLengthReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$FdaReport$Age==5] >
                  MeanLengthReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$FdaReport$Age==4]))

#mean for plusgr should be larger than lowest age in plusgr
expect_true(all(MeanLengthReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$FdaReport$Age==5] >
                  MeanLengthReportDecomp$FdaReport$MeanIndividualWeight[MeanLengthReportDecomp$FdaReport$Age==5]))
#mean for plusgr should be smaller than largest age in plusgr
# beware of artifacts for small age groups (convergence or data issues). Using age group 13, rather than 14
expect_true(all(MeanLengthReportDecompPlusGr$FdaReport$MeanIndividualWeight[MeanLengthReportDecompPlusGr$FdaReport$Age==5] <
                  MeanLengthReportDecomp$FdaReport$MeanIndividualWeight[MeanLengthReportDecomp$FdaReport$Age==13]))

context("Test SOP")

sopTab <- ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear", "Area"))
expect_true(is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport
expect_true(all(abs(sopTab$RelativeDifference) < 0.02))

sopTab <- ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear"))
expect_true(is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport

expect_true(all(abs(sopTab$RelativeDifference) < 0.006))

sopTab <- ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData)
expect_true(is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport

expect_true(all(abs(sopTab$RelativeDifference) < 0.005))
expect_true(nrow(sopTab) == 1)

# Check that NAs are reported for incomplete landings
SL <- StoxLandingData
SL$Landing <- SL$Landing[SL$Landing$Gear != 53,]

sopTab <- ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, SL, GroupingVariables = c("Gear", "Area"))
expect_true(is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport
expect_true(all(is.na(sopTab$RelativeDifference[sopTab$Gear==53])))
expect_true(all(!is.na(sopTab$RelativeDifference[sopTab$Gear==11])))

# Check that NAs are reported for incomplete estimates (and incomplete landings)
catchAtAgeReportDecompPlusGr$FdaReport$Gear[catchAtAgeReportDecompPlusGr$FdaReport$Gear==53] <- 52
MeanWeightReportDecompPlusGr$FdaReport$Gear[MeanWeightReportDecompPlusGr$FdaReport$Gear==53] <- 52
sopTab <- ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear", "Area"))
expect_true(is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport
expect_true(all(is.na(sopTab$RelativeDifference[sopTab$Gear==52])))
expect_true(all(is.na(sopTab$LandedWeight[sopTab$Gear==52])))
expect_true(all(is.na(sopTab$RelativeDifference[sopTab$Gear==53])))
expect_true(all(is.na(sopTab$TotalWeightEstimated[sopTab$Gear==53])))
expect_true(all(!is.na(sopTab$RelativeDifference[sopTab$Gear==11])))


#
# check reports with stock splitting
#
predictiondatafile <- system.file("testresources","stocksplitpred.rds", package="RstoxFDA")
predictiondata <- readRDS(predictiondatafile)
resultPlusgr<-ReportRecaWeightAtAge(predictiondata, PlusGroup = 10)
result<-ReportRecaWeightAtAge(predictiondata)
expect_true(all(result$FdaReport[result$FdaReport$Age<10,] == resultPlusgr$FdaReport[resultPlusgr$FdaReport$Age<10,]))
