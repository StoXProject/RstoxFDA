
#context("Test StoxReportFunctions: ReportRecaParameterStatistics")
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Individual <- StoxBioticData$Individual[StoxBioticData$Individual$IndividualAge<4,]
StoxBioticData$Haul$Gear <- StoxLandingData$Landing$Gear[c(1:20, 1:20, 1:5)]
StoxBioticData$Station$Area <- StoxLandingData$Landing$Area[c(1:20, 1:20, 1:5)]
prep <- RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear", "Area"), CellEffect = c("All"), MinAge = 2, MaxAge = 3)

fpath1 <- RstoxFDA:::makeTempDirReca("chain1")
fpath2 <- RstoxFDA:::makeTempDirReca("chain2")
fpath3 <- RstoxFDA:::makeTempDirReca("chain3")

paramOut1 <- RstoxFDA::ParameterizeRecaModels(prep, 10, 50, 1, ResultDirectory = fpath1)
paramOut2 <- RstoxFDA::ParameterizeRecaModels(prep, 10, 50, 1, ResultDirectory = fpath2)

paramSummary <- RstoxFDA::ReportRecaParameterStatistics(paramOut1)
paramSummary <- RstoxFDA::ReportRecaParameterStatistics(paramOut2, paramSummary, AppendReport = TRUE)
expect_true(RstoxFDA::is.ParameterizationSummaryData(paramSummary))

RstoxFDA:::removeTempDirReca(fpath1)
RstoxFDA:::removeTempDirReca(fpath2)
RstoxFDA:::removeTempDirReca(fpath3)

convergence <- RstoxFDA::ReportParameterConvergence(paramSummary)

expect_true(RstoxFDA::is.ParameterConvergenceData(convergence))
expect_true(nrow(convergence$ConvergenceReport) < 433)
expect_true(nrow(convergence$ConvergenceReport) > 0)

#construct three identical chains, should signal convergence
paramSummary <- RstoxFDA::ReportRecaParameterStatistics(paramOut1)
paramOut1$GlobalParameters$GlobalParameters$resultdir="B"
paramSummary <- RstoxFDA::ReportRecaParameterStatistics(paramOut1, paramSummary, AppendReport = T)
paramOut1$GlobalParameters$GlobalParameters$resultdir="C"
paramSummary <- RstoxFDA::ReportRecaParameterStatistics(paramOut1, paramSummary, AppendReport = T)

#context("Check Gelman-Rubin for equal chains")
convergence <- RstoxFDA::ReportParameterConvergence(paramSummary, Tolerance = 0)
expect_equal(nrow(convergence$ConvergenceReport), 433)
expect_true(all(abs(convergence$ConvergenceReport$GelmanRubinR-1)<.1))


#context("Test StoxReportFunctions: ReportRecaCatchStatistics")
predictiondatafile <- system.file("testresources","stocksplitpred.rds", package="RstoxFDA")
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(predictiondatafile)

catchReportDecomp <- RstoxFDA::ReportRecaCatchStatistics(catchAtAgeDecomp)
expect_true(!any(duplicated(catchReportDecomp$MeanAge$MeanIndividualAge)))
expect_equal(nrow(catchReportDecomp$MeanAge), nrow(catchReportDecomp$MeanWeight))
expect_equal(nrow(catchReportDecomp$MeanAge), nrow(catchReportDecomp$MeanLength))
expect_equal(nrow(catchReportDecomp$TotalWeight), nrow(catchReportDecomp$MeanLength))
expect_equal(nrow(catchReportDecomp$TotalNumber), nrow(catchReportDecomp$MeanLength))
expect_equal(nrow(catchReportDecomp$GroupingVariables), 3)
expect_true(all(((catchReportDecomp$TotalWeight$TotalWeight / catchReportDecomp$TotalNumber$TotalNumber) - catchReportDecomp$MeanWeight$MeanIndividualWeight) <.02))
catchReportFlat <- RstoxFDA::ReportRecaCatchStatistics(catchAtAgeFlat)
expect_true(nrow(catchReportFlat$MeanAge) == 1)
expect_true(nrow(catchReportFlat$MeanWeight) == 1)
expect_true(nrow(catchReportFlat$MeanLength) == 1)
expect_true(nrow(catchReportFlat$TotalWeight) == 1)
expect_true(nrow(catchReportFlat$TotalNumber) == 1)
expect_true(nrow(catchReportFlat$GroupingVariables) == 0)

#context("Test StoxReportFunctions: ReportRecaCatchStatistics units")
catchReportFlat <- RstoxFDA::ReportRecaCatchStatistics(catchAtAgeFlat, DecimalMeanWeight = 3, DecimalMeanLength = 1)
expect_equal(RstoxData::getUnit(catchReportFlat$MeanAge$MeanIndividualAge), "age-year")
expect_equal(RstoxData::getUnit(catchReportFlat$MeanWeight$SD), "mass-g")
expect_equal(RstoxData::getUnit(catchReportFlat$MeanLength$Low), "length-mm")
expect_equal(RstoxData::getUnit(catchReportFlat$TotalWeight$High), "mass-kg")
expect_equal(RstoxData::getUnit(catchReportFlat$TotalNumber$TotalNumber), "cardinality-N")

catchReportFlatOU <- RstoxFDA::ReportRecaCatchStatistics(catchAtAgeFlat, UnitTotalNumber = "10^3 individuals", DecimalTotalNumber = 6, DecimalTotalWeight = 6, UnitTotalWeight = "kiloton", UnitMeanWeight = "kg", UnitMeanLength = "cm", DecimalMeanLength = 2)
expect_equal(RstoxData::getUnit(catchReportFlatOU$MeanAge$MeanIndividualAge), "age-year")
expect_equal(RstoxData::getUnit(catchReportFlatOU$MeanWeight$MeanIndividualWeight), "mass-kg")
expect_equal(RstoxData::getUnit(catchReportFlatOU$MeanLength$High), "length-cm")
expect_equal(RstoxData::getUnit(catchReportFlatOU$TotalWeight$Low), "mass-kt")
expect_equal(RstoxData::getUnit(catchReportFlatOU$TotalNumber$SD), "cardinality-kN")

expect_equal(catchReportFlatOU$MeanAge$MeanIndividualAge, catchReportFlat$MeanAge$MeanIndividualAge)
expect_equal(catchReportFlatOU$MeanWeight$MeanIndividualWeight[1]*1000, catchReportFlat$MeanWeight$MeanIndividualWeight[1])
expect_true(abs(catchReportFlatOU$MeanLength$MeanIndividualLength[1]*10 - catchReportFlat$MeanLength$MeanIndividualLength[1])/catchReportFlat$MeanLength$MeanIndividualLength[1] < 1e-2)
expect_equal(catchReportFlatOU$TotalWeight$TotalWeight[1]*1e6, catchReportFlat$TotalWeight$TotalWeight[1])
expect_equal(catchReportFlatOU$TotalNumber$TotalNumber[1]*1e3, catchReportFlat$TotalNumber$TotalNumber[1])

#context("Test StoxReportFunctions: ReportFdaLandings")
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
report <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Area"))
expect_equal(ncol(report$FisheriesLandings),2)
report <- RstoxFDA::ReportFdaLandings(StoxLandingData)
expect_equal(ncol(report$FisheriesLandings),2)
expect_equal(nrow(report$FisheriesLandings),1)
report <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Area", "Gear"), Decimals = -3)
expect_equal(ncol(report$FisheriesLandings),3)
reportT <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Area", "Gear"), Unit = "ton")
expect_equal(reportT$FisheriesLandings$LandedRoundWeight[1], report$FisheriesLandings$LandedRoundWeight[1]/1000)
reportDate <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("CatchDate"), Unit = "ton")
expect_true(all(sort(reportDate$FisheriesLandings$CatchDate)==reportDate$FisheriesLandings$CatchDate))

#context("Test StoxReportFunctions: ReportFdaSampling")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$Quarter <- quarters(StoxBioticData$Station$DateTime)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxLandingData$Landing$Quarter <- quarters(StoxLandingData$Landing$CatchDate)

SamplingReport <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Quarter"))
expect_true(RstoxFDA::is.ReportFdaSamplingData(SamplingReport))
expect_true(all(!is.na(SamplingReport$FisheriesSampling$LandedRoundWeight)))
expect_equal(RstoxData::getUnit(SamplingReport$FisheriesSampling$WeightOfSampledCatches), "mass-kg")
expect_true(is.na(RstoxData::getUnit(SamplingReport$FisheriesSampling$Catches)))

SamplingReportKt <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Quarter"), Unit="kiloton", Decimals = 6)
expect_equal(SamplingReportKt$FisheriesSampling$LandedRoundWeight[1:2], SamplingReport$FisheriesSampling$LandedRoundWeight[1:2]/1e6)
expect_equal(RstoxData::getUnit(SamplingReportKt$FisheriesSampling$WeightOfSampledCatches), "mass-kt")
expect_equal(RstoxData::getUnit(SamplingReportKt$FisheriesSampling$LandedRoundWeight), "mass-kt")
expect_true(is.na(RstoxData::getUnit(SamplingReportKt$FisheriesSampling$Catches)))

SamplingReportRounded <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Quarter"), Decimals=-6)
expect_true(all(SamplingReportRounded$FisheriesSampling$LandedRoundWeight != SamplingReport$FisheriesSampling$LandedRoundWeight))
expect_true(all(SamplingReportRounded$FisheriesSampling$WeightOfSampledCatches != SamplingReport$FisheriesSampling$WeightOfSampledCatches))


#Default gear is different coding system for stoxbiotic and landing
unlanded <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear"))
expect_true(data.table::is.data.table(unlanded$GroupingVariables))
expect_true("Gear" %in% unlanded$GroupingVariables$GroupingVariables)
expect_true(any(is.na(unlanded$FisheriesSampling$LandedRoundWeight)))
expect_error(RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Quarter", "Nonesene")), "All 'GroupingVariables' must be present in 'StoxLandingData'. Missing: Nonesene")


#context("Report Catch At Age")
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

catchAtAgeReportDecomp <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp)
catchAtAgeReportFlat <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat)

expect_true(RstoxFDA::is.ReportFdaByAgeData(catchAtAgeReportDecomp))
expect_true(RstoxFDA::is.ReportFdaByAgeData(catchAtAgeReportFlat))

diff <- sum(catchAtAgeReportFlat$FdaReport$CatchAtAge) - sum(catchAtAgeReportDecomp$FdaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlat$FdaReport$CatchAtAge))

expect_true(reldiff < .001)
expect_equal(length(catchAtAgeReportFlat$GroupingVariables$GroupingVariables), 0)
expect_equal(ncol(catchAtAgeReportFlat$FdaReport), 6)

expect_equal(length(catchAtAgeReportDecomp$GroupingVariables$GroupingVariables), 2)
expect_equal(ncol(catchAtAgeReportDecomp$FdaReport), 8)

#context("Report Catch At Length")
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

catchAtLengthReportDecomp <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeDecomp)
catchAtLengthReportFlat <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat)

expect_true(nrow(catchAtLengthReportDecomp$FdaReport) > nrow(catchAtLengthReportFlat$FdaReport))
#check relative different to caa
reld <- (sum(catchAtLengthReportDecomp$FdaReport$CatchAtLength) - sum(catchAtAgeReportDecomp$FdaReport$CatchAtAge)) / sum(catchAtLengthReportDecomp$FdaReport$CatchAtLength)
expect_true(abs(reld) < 1e-6)
#check relative different to flat
reld <- (sum(catchAtLengthReportDecomp$FdaReport$CatchAtLength) - sum(catchAtLengthReportFlat$FdaReport$CatchAtLength)) / sum(catchAtLengthReportDecomp$FdaReport$CatchAtLength)
expect_true(abs(reld) < 1e-2)

catchAtLengthReportFlatIntervalDefault <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat)
catchAtLengthReportFlatInterval5 <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat,LengthInterval = 5)
catchAtLengthReportFlatIntervalp6 <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat,LengthInterval = .6)
expect_warning(catchAtLengthReportFlatIntervalp1 <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat,LengthInterval = .1), "StoX: Length interval is specified lower than the available resolution")
expect_equal(sum(catchAtLengthReportFlatIntervalp1$FdaReport$CatchAtLength), sum(catchAtLengthReportFlatIntervalp1$FdaReport$CatchAtLength))
expect_true(sum(catchAtLengthReportFlatInterval5$FdaReport$SD) < sum(catchAtLengthReportFlatIntervalDefault$FdaReport$SD))
rdiff <- (sum(catchAtAgeReportFlat$FdaReport$CatchAtAge) - sum(catchAtLengthReportFlatInterval5$FdaReport$CatchAtLength)) / sum(catchAtAgeReportFlat$FdaReport$CatchAtAge)
expect_true(abs(rdiff) < 1e-6)

sumU25p5 <- sum(catchAtLengthReportFlatInterval5$FdaReport$CatchAtLength[catchAtLengthReportFlatInterval5$FdaReport$Length<30])
sumU25pD <- sum(catchAtLengthReportFlatIntervalDefault$FdaReport$CatchAtLength[catchAtLengthReportFlatIntervalDefault$FdaReport$Length<25])
rdiff <- (sumU25p5 - sumU25pD)/sumU25p5
expect_true(abs(rdiff) < .1)

catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeReportFlat <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat)
catchAtLengthAndAgeReportFlat <- RstoxFDA:::ReportRecaCatchAtLengthAndAge(catchAtAgeFlat)
rdiff <- (sum(catchAtAgeReportFlat$FdaReport$CatchAtAge) - sum(catchAtLengthAndAgeReportFlat$FdaReport$CatchAtAge)) / sum(catchAtLengthAndAgeReportFlat$FdaReport$CatchAtAge)
expect_true(abs(rdiff) < 1e-6)
catchAtLengthAndAgeReportFlatPlG <- RstoxFDA:::ReportRecaCatchAtLengthAndAge(catchAtAgeFlat, PlusGroup = 5, LengthInterval = 10)
rdiff <- (sum(catchAtAgeReportFlat$FdaReport$CatchAtAge) - sum(catchAtLengthAndAgeReportFlatPlG$FdaReport$CatchAtAge)) / sum(catchAtLengthAndAgeReportFlatPlG$FdaReport$CatchAtAge)
expect_true(abs(rdiff) < 1e-6)

catchAtLengthAndAgeReportFlatPlG <- RstoxFDA:::ReportRecaCatchAtLengthAndAge(catchAtAgeFlat, PlusGroup = 5, LengthInterval = 10)

#test Digits
catchAtAgeD <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, Decimals=-3)
expect_true(all(catchAtAgeD$FdaReport$SD[1] != catchAtAgeReportFlat$FdaReport$SD[1]))
catchAtAgeD <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, Decimals=0)
expect_true(all(catchAtAgeD$FdaReport$SD == catchAtAgeReportFlat$FdaReport$SD))

#test plusgroup
catchAtAgeReportDecompPlusGr <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5)
diff <- sum(catchAtAgeReportDecomp$FdaReport$CatchAtAge) - sum(catchAtAgeReportDecompPlusGr$FdaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportDecompPlusGr$FdaReport$CatchAtAge))
expect_true(reldiff < .001)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$FdaReport), 40)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$GroupingVariables), 2)

catchAtAgeReportFlatPlusGr <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup=5)
diff <- sum(catchAtAgeReportFlat$FdaReport$CatchAtAge) - sum(catchAtAgeReportFlatPlusGr$FdaReport$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlatPlusGr$FdaReport$CatchAtAge))
expect_true(reldiff <.001)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$FdaReport), 4)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$GroupingVariables), 0)
expect_equal(RstoxData::getUnit(catchAtAgeReportFlatPlusGr$FdaReport$CatchAtAge, property = "name"), "individuals")

catchAtAgeReportMi <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup=5, Unit = "10^6 individuals", Decimals = 6)
expect_equal(RstoxData::getUnit(catchAtAgeReportMi$FdaReport$CatchAtAge, property = "symbol"), "MN")
expect_equal(catchAtAgeReportMi$FdaReport$CatchAtAge[1:3]*1e6, catchAtAgeReportFlatPlusGr$FdaReport$CatchAtAge[1:3])
expect_equal(catchAtAgeReportMi$FdaReport$SD[1:3]*1e6, catchAtAgeReportFlatPlusGr$FdaReport$SD[1:3])


# Report Mean weight

MeanWeightReportDecomp <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimals = 4, Unit = "kg")
expect_true(RstoxFDA::is.ReportFdaByAgeData(MeanWeightReportDecomp))
expect_equal(RstoxData::getUnit(MeanWeightReportDecomp$FdaReport$MeanIndividualWeight), "mass-kg")

MeanWeightReportDecimal <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=4)
MeanWeightReportDecimalG <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=1, Unit="g")
expect_equal(MeanWeightReportDecimalG$FdaReport$MeanIndividualWeight[1:2], MeanWeightReportDecomp$FdaReport$MeanIndividualWeight[1:2]*1000)

MeanWeightReportTk <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=4, Threshold = 1000)
MeanWeightReportT10k <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=4, Threshold = 10000)
expect_true(sum(is.na(MeanWeightReportTk$FdaReport$MeanIndividualWeight)) < sum(is.na(MeanWeightReportT10k$FdaReport$MeanIndividualWeight)))

# Report Mean weight Plus gr
MeanWeightReportDecomp <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimals = 6)
expect_true(MeanWeightReportDecimal$FdaReport$Low[1] != MeanWeightReportDecomp$FdaReport$Low[1])
MeanWeightReportDecompPlusGr <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6)

expect_true(nrow(MeanWeightReportDecompPlusGr$FdaReport) < nrow(MeanWeightReportDecomp$FdaReport))

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
MeanLengthReportDecomp <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Unit="cm")
expect_true(RstoxFDA::is.ReportFdaByAgeData(MeanLengthReportDecomp))
expect_true(!all(nchar(as.character(MeanLengthReportDecomp$FdaReport$MeanIndividualLength[MeanLengthReportDecomp$FdaReport$MeanIndividualLength>0]))>5))
expect_equal(RstoxData::getUnit(MeanLengthReportDecomp$FdaReport$MeanIndividualLength), "length-cm")

MeanLengthReportDecompMM <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Unit = "mm", Decimals=0)
expect_equal(RstoxData::getUnit(MeanLengthReportDecompMM$FdaReport$MeanIndividualLength), "length-mm")
expect_equal(MeanLengthReportDecomp$FdaReport$MeanIndividualLength[3:4]*10, MeanLengthReportDecompMM$FdaReport$MeanIndividualLength[3:4])
expect_equal(MeanLengthReportDecomp$FdaReport$Low[3:4]*10, MeanLengthReportDecompMM$FdaReport$Low[3:4])

MeanLengthReportDecimals <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Decimals = 4)
expect_true(all(nchar(as.character(MeanLengthReportDecimals$FdaReport$MeanIndividualLength[MeanLengthReportDecimals$FdaReport$MeanIndividualLength>0]))>5))

MeanLengthReportTk <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Decimals = 4, Threshold = 1000)
expect_true(all(is.na(MeanLengthReportTk$FdaReport$MeanIndividualLength) == is.na(MeanWeightReportTk$FdaReport$MeanIndividualWeight)))

# Report Mean length Plus gr
MeanLengthReportDecompPlusGr <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, PlusGroup=5)

expect_true(nrow(MeanLengthReportDecompPlusGr$FdaReport) < nrow(MeanLengthReportDecomp$FdaReport))

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

#context("Test SOP w NAs")
catchAtAgeReportDecompPlusGr <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6)
MeanWeightReportDecompPlusGr <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6, Threshold = 1000)
expect_true(sum(is.na(MeanWeightReportDecompPlusGr$FdaReport$MeanIndividualWeight))>1)
sopTabNa <- RstoxFDA::ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear", "Area"))
expect_true(RstoxFDA::is.ReportFdaSOP(sopTabNa))
sopTabNa <- sopTabNa$SopReport
expect_true(any(sopTabNa$Difference<0))

#context("Test SOP")
catchAtAgeReportDecompPlusGrKi <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6, Unit = "10^3 individuals")
catchAtAgeReportDecompPlusGr <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6)
MeanWeightReportDecompPlusGr <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6)
sopTab <- RstoxFDA::ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear", "Area"))
sopTabKi <- RstoxFDA::ReportFdaSOP(catchAtAgeReportDecompPlusGrKi, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear", "Area"), UnitFraction = "%")
expect_true(RstoxFDA::is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport
sopTabKi <- sopTabKi$SopReport
expect_true(all(abs(sopTab$RelativeDifference) < 0.02))

#check that the number different may come from NAs
expect_equal(sum(sopTabNa$RelativeDifference!=0), sum(sopTabNa$Difference != sopTab$Difference))

expect_equal(RstoxData::getUnit(sopTab$TotalWeightEstimated), "mass-kg")
expect_equal(RstoxData::getUnit(sopTab$LandedWeight), "mass-kg")
expect_equal(RstoxData::getUnit(sopTab$Difference), "mass-kg")
expect_equal(RstoxData::getUnit(sopTab$RelativeDifference), "fraction-decimal")

expect_equal(RstoxData::getUnit(sopTabKi$TotalWeightEstimated), "mass-kg")
expect_equal(RstoxData::getUnit(sopTabKi$LandedWeight), "mass-kg")
expect_equal(RstoxData::getUnit(sopTabKi$Difference), "mass-kg")
expect_equal(RstoxData::getUnit(sopTabKi$RelativeDifference), "fraction-percent")

expect_equal(sopTabKi$RelativeDifference[1:2]/100, sopTab$RelativeDifference[1:2])
expect_equal(sopTabKi$TotalWeightEstimated[1:2], sopTab$TotalWeightEstimated[1:2])
expect_equal(sopTabKi$LandedWeight[1:2], sopTab$LandedWeight[1:2])

sopTab <- RstoxFDA::ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear"))
expect_true(RstoxFDA::is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport

expect_true(all(abs(sopTab$RelativeDifference) < 0.006))

sopTab <- RstoxFDA::ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData)
expect_true(RstoxFDA::is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport

expect_true(all(abs(sopTab$RelativeDifference) < 0.005))
expect_true(nrow(sopTab) == 1)

# Check that NAs are reported for incomplete landings
SL <- StoxLandingData
SL$Landing <- SL$Landing[SL$Landing$Gear != 53,]

sopTab <- RstoxFDA::ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, SL, GroupingVariables = c("Gear", "Area"))
expect_true(RstoxFDA::is.ReportFdaSOP(sopTab))
sopTab <- sopTab$SopReport
expect_true(all(is.na(sopTab$RelativeDifference[sopTab$Gear==53])))
expect_true(all(!is.na(sopTab$RelativeDifference[sopTab$Gear==11])))

# Check that NAs are reported for incomplete estimates (and incomplete landings)
catchAtAgeReportDecompPlusGr$FdaReport$Gear[catchAtAgeReportDecompPlusGr$FdaReport$Gear==53] <- 52
MeanWeightReportDecompPlusGr$FdaReport$Gear[MeanWeightReportDecompPlusGr$FdaReport$Gear==53] <- 52
sopTab <- RstoxFDA::ReportFdaSOP(catchAtAgeReportDecompPlusGr, MeanWeightReportDecompPlusGr, StoxLandingData, GroupingVariables = c("Gear", "Area"))
expect_true(RstoxFDA::is.ReportFdaSOP(sopTab))
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
resultPlusgr<-RstoxFDA::ReportRecaWeightAtAge(predictiondata, PlusGroup = 10)
result<-RstoxFDA::ReportRecaWeightAtAge(predictiondata)
expect_true(all(result$FdaReport[result$FdaReport$Age<10,] == resultPlusgr$FdaReport[resultPlusgr$FdaReport$Age<10,]))