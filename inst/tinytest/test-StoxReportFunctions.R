
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


#Report covariances
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

catchAtAgeCovarDecomp <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeDecomp)
expect_equal(nrow(catchAtAgeCovarDecomp$CovarianceNbyAge), nrow(catchAtAgeCovarDecomp$Variables)**2)
expect_equal(nrow(catchAtAgeCovarDecomp$Variables),130)
catchAtAgeCovarFlat <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeFlat)
expect_equal(nrow(catchAtAgeCovarFlat$CovarianceNbyAge), nrow(catchAtAgeCovarFlat$Variables)**2)
expect_equal(nrow(catchAtAgeCovarFlat$Variables),13)

#compare with SD from regular report
catchAtAgeReportFlat <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat)
expect_true(all(((catchAtAgeReportFlat$NbyAge$SD - sqrt(catchAtAgeCovarFlat$CovarianceNbyAge$Covariance[catchAtAgeCovarFlat$FdaCovariances$VariableId1==catchAtAgeCovarFlat$FdaCovariances$VariableId2]))/catchAtAgeReportFlat$NbyAge$SD)<1e-3))

catchAtAgeReportFlat <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, Unit = "10^3 individuals", Decimals = 6)
catchAtAgeCovarFlat <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeFlat, Unit = "10^3 individuals", Decimals = 6)
expect_true(all(((catchAtAgeReportFlat$NbyAge$SD - sqrt(catchAtAgeCovarFlat$CovarianceNbyAge$Covariance[catchAtAgeCovarFlat$FdaCovariances$VariableId1==catchAtAgeCovarFlat$FdaCovariances$VariableId2]))/catchAtAgeReportFlat$NbyAge$SD)<1e-3))


#context("Report Catch At Age")
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

catchAtAgeReportDecomp <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp)
catchAtAgeReportFlat <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat)

expect_true(RstoxFDA::is.ReportFdaByAgeData(catchAtAgeReportDecomp))
expect_true(RstoxFDA::is.ReportFdaByAgeData(catchAtAgeReportFlat))

diff <- sum(catchAtAgeReportFlat$NbyAge$CatchAtAge) - sum(catchAtAgeReportDecomp$NbyAge$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlat$NbyAge$CatchAtAge))

expect_true(reldiff < .001)
expect_equal(length(catchAtAgeReportFlat$GroupingVariables$GroupingVariables), 0)
expect_equal(ncol(catchAtAgeReportFlat$NbyAge), 6)

expect_equal(length(catchAtAgeReportDecomp$GroupingVariables$GroupingVariables), 2)
expect_equal(ncol(catchAtAgeReportDecomp$NbyAge), 8)

#context("Report Catch At Length")
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

catchAtLengthReportDecomp <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeDecomp)
catchAtLengthReportFlat <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat)

expect_true(nrow(catchAtLengthReportDecomp$NbyLength) > nrow(catchAtLengthReportFlat$NbyLength))
#check relative different to caa
reld <- (sum(catchAtLengthReportDecomp$NbyLength$CatchAtLength) - sum(catchAtAgeReportDecomp$NbyAge$CatchAtAge)) / sum(catchAtLengthReportDecomp$NbyLength$CatchAtLength)
expect_true(abs(reld) < 1e-6)
#check relative different to flat
reld <- (sum(catchAtLengthReportDecomp$NbyLength$CatchAtLength) - sum(catchAtLengthReportFlat$NbyLength$CatchAtLength)) / sum(catchAtLengthReportDecomp$NbyLength$CatchAtLength)
expect_true(abs(reld) < 1e-2)

catchAtLengthReportFlatIntervalDefault <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat)
catchAtLengthReportFlatInterval5 <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat,LengthInterval = 5)
catchAtLengthReportFlatIntervalp6 <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat,LengthInterval = .6)
expect_warning(catchAtLengthReportFlatIntervalp1 <- RstoxFDA::ReportRecaCatchAtLength(catchAtAgeFlat,LengthInterval = .1), "StoX: Length interval is specified lower than the available resolution")
expect_equal(sum(catchAtLengthReportFlatIntervalp1$NbyLength$CatchAtLength), sum(catchAtLengthReportFlatIntervalp1$NbyLength$CatchAtLength))
expect_true(sum(catchAtLengthReportFlatInterval5$NbyLength$SD) < sum(catchAtLengthReportFlatIntervalDefault$NbyLength$SD))
rdiff <- (sum(catchAtAgeReportFlat$NbyAge$CatchAtAge) - sum(catchAtLengthReportFlatInterval5$NbyLength$CatchAtLength)) / sum(catchAtAgeReportFlat$NbyAge$CatchAtAge)
expect_true(abs(rdiff) < 1e-6)

sumU25p5 <- sum(catchAtLengthReportFlatInterval5$NbyLength$CatchAtLength[catchAtLengthReportFlatInterval5$NbyLength$Length<30])
sumU25pD <- sum(catchAtLengthReportFlatIntervalDefault$NbyLength$CatchAtLength[catchAtLengthReportFlatIntervalDefault$NbyLength$Length<25])
rdiff <- (sumU25p5 - sumU25pD)/sumU25p5
expect_true(abs(rdiff) < .1)

catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeReportFlat <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat)
catchAtLengthAndAgeReportFlat <- RstoxFDA:::ReportRecaCatchAtLengthAndAge(catchAtAgeFlat)
browser()
rdiff <- (sum(catchAtAgeReportFlat$NbyAge$CatchAtAge) - sum(catchAtLengthAndAgeReportFlat$NbyLengthAge$CatchAtAgeLength)) / sum(catchAtLengthAndAgeReportFlat$NbyLengthAge$CatchAtAgeLength)
expect_true(abs(rdiff) < 1e-6)
catchAtLengthAndAgeReportFlatPlG <- RstoxFDA:::ReportRecaCatchAtLengthAndAge(catchAtAgeFlat, PlusGroup = 5, LengthInterval = 10)
rdiff <- (sum(catchAtAgeReportFlat$NbyAge$CatchAtAge) - sum(catchAtLengthAndAgeReportFlatPlG$NbyLengthAge$CatchAtAgeLength)) / sum(catchAtLengthAndAgeReportFlatPlG$NbyLengthAge$CatchAtAgeLength)
expect_true(abs(rdiff) < 1e-6)

catchAtLengthAndAgeReportFlatPlG <- RstoxFDA:::ReportRecaCatchAtLengthAndAge(catchAtAgeFlat, PlusGroup = 5, LengthInterval = 10)

#test Digits
catchAtAgeD <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, Decimals=-3)
expect_true(all(catchAtAgeD$NbyAge$SD[1] != catchAtAgeReportFlat$NbyAge$SD[1]))
catchAtAgeD <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, Decimals=0)
expect_true(all(catchAtAgeD$NbyAge$SD == catchAtAgeReportFlat$NbyAge$SD))

#test plusgroup
catchAtAgeReportDecompPlusGr <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5)
diff <- sum(catchAtAgeReportDecomp$NbyAge$CatchAtAge) - sum(catchAtAgeReportDecompPlusGr$NbyAge$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportDecompPlusGr$NbyAge$CatchAtAge))
expect_true(reldiff < .001)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$NbyAge), 40)
expect_equal(nrow(catchAtAgeReportDecompPlusGr$GroupingVariables), 2)

catchAtAgeReportFlatPlusGr <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup=5)
diff <- sum(catchAtAgeReportFlat$NbyAge$CatchAtAge) - sum(catchAtAgeReportFlatPlusGr$NbyAge$CatchAtAge)
reldiff <- abs(diff/sum(catchAtAgeReportFlatPlusGr$NbyAge$CatchAtAge))
expect_true(reldiff <.001)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$NbyAge), 4)
expect_equal(nrow(catchAtAgeReportFlatPlusGr$GroupingVariables), 0)
expect_equal(RstoxData::getUnit(catchAtAgeReportFlatPlusGr$NbyAge$CatchAtAge, property = "name"), "individuals")

catchAtAgeReportMi <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup=5, Unit = "10^6 individuals", Decimals = 6)
expect_equal(RstoxData::getUnit(catchAtAgeReportMi$NbyAge$CatchAtAge, property = "symbol"), "MN")
expect_equal(catchAtAgeReportMi$NbyAge$CatchAtAge[1:3]*1e6, catchAtAgeReportFlatPlusGr$NbyAge$CatchAtAge[1:3])
expect_equal(catchAtAgeReportMi$NbyAge$SD[1:3]*1e6, catchAtAgeReportFlatPlusGr$NbyAge$SD[1:3])


# Report Mean weight

MeanWeightReportDecomp <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimals = 4, Unit = "kg")
expect_true(RstoxFDA::is.ReportFdaByAgeData(MeanWeightReportDecomp))
expect_equal(RstoxData::getUnit(MeanWeightReportDecomp$MeanWeightByAge$MeanIndividualWeight), "mass-kg")

MeanWeightReportDecimal <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=4)
MeanWeightReportDecimalG <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=1, Unit="g")
expect_equal(MeanWeightReportDecimalG$MeanWeightByAge$MeanIndividualWeight[1:2], MeanWeightReportDecomp$MeanWeightByAge$MeanIndividualWeight[1:2]*1000)

MeanWeightReportTk <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=4, Threshold = 1000)
MeanWeightReportT10k <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimal=4, Threshold = 10000)
expect_true(sum(is.na(MeanWeightReportTk$MeanWeightByAge$MeanIndividualWeight)) < sum(is.na(MeanWeightReportT10k$MeanWeightByAge$MeanIndividualWeight)))

# Report Mean weight Plus gr
MeanWeightReportDecomp <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimals = 6)
expect_true(MeanWeightReportDecimal$MeanWeightByAge$Low[1] != MeanWeightReportDecomp$MeanWeightByAge$Low[1])
MeanWeightReportDecompPlusGr <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6)

expect_true(nrow(MeanWeightReportDecompPlusGr$MeanWeightByAge) < nrow(MeanWeightReportDecomp$MeanWeightByAge))

#ages not in plusgroup should be equal for calculation w and wo plusgroups
expect_equal(MeanWeightReportDecomp$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecomp$MeanWeightByAge$Age<5],
             MeanWeightReportDecompPlusGr$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecompPlusGr$MeanWeightByAge$Age<5])

# mean for plus group should be larger than oldes age not in plus group
expect_true(all(MeanWeightReportDecompPlusGr$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecompPlusGr$MeanWeightByAge$Age==5] >
          MeanWeightReportDecompPlusGr$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecompPlusGr$MeanWeightByAge$Age==4]))

#mean for plusgr should be larger than lowest age in plusgr
expect_true(all(MeanWeightReportDecompPlusGr$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecompPlusGr$MeanWeightByAge$Age==5] >
          MeanWeightReportDecomp$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecomp$MeanWeightByAge$Age==5]))
#mean for plusgr should be smaller than largest age in plusgr
# beware of artifacts for small age groups (convergence or data issues). Using age group 13, rather than 14

expect_true(all(MeanWeightReportDecompPlusGr$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecompPlusGr$MeanWeightByAge$Age==5] <
          MeanWeightReportDecomp$MeanWeightByAge$MeanIndividualWeight[MeanWeightReportDecomp$MeanWeightByAge$Age==13]))



# Report Mean length
MeanLengthReportDecomp <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Unit="cm")
expect_true(RstoxFDA::is.ReportFdaByAgeData(MeanLengthReportDecomp))
expect_true(!all(nchar(as.character(MeanLengthReportDecomp$MeanLengthByAge$MeanIndividualLength[MeanLengthReportDecomp$MeanLengthByAge$MeanIndividualLength>0]))>5))
expect_equal(RstoxData::getUnit(MeanLengthReportDecomp$MeanLengthByAge$MeanIndividualLength), "length-cm")

MeanLengthReportDecompMM <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Unit = "mm", Decimals=0)
expect_equal(RstoxData::getUnit(MeanLengthReportDecompMM$MeanLengthByAge$MeanIndividualLength), "length-mm")
expect_equal(MeanLengthReportDecomp$MeanLengthByAge$MeanIndividualLength[3:4]*10, MeanLengthReportDecompMM$MeanLengthByAge$MeanIndividualLength[3:4])
expect_equal(MeanLengthReportDecomp$MeanLengthByAge$Low[3:4]*10, MeanLengthReportDecompMM$MeanLengthByAge$Low[3:4])

MeanLengthReportDecimals <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Decimals = 4)
expect_true(all(nchar(as.character(MeanLengthReportDecimals$MeanLengthByAge$MeanIndividualLength[MeanLengthReportDecimals$MeanLengthByAge$MeanIndividualLength>0]))>5))

MeanLengthReportTk <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Decimals = 4, Threshold = 1000)
expect_true(all(is.na(MeanLengthReportTk$MeanLengthByAge$MeanIndividualLength) == is.na(MeanWeightReportTk$MeanLengthByAge$MeanIndividualWeight)))

# Report Mean length Plus gr
MeanLengthReportDecompPlusGr <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, PlusGroup=5)

expect_true(nrow(MeanLengthReportDecompPlusGr$MeanLengthByAge) < nrow(MeanLengthReportDecomp$MeanLengthByAge))

#ages not in plusgroup should be equal for calculation w and wo plusgroups
expect_equal(MeanLengthReportDecomp$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecomp$MeanLengthByAge$Age<5],
             MeanLengthReportDecompPlusGr$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecompPlusGr$MeanLengthByAge$Age<5])

# mean for plus group should be larger than oldes age not in plus group
expect_true(all(MeanLengthReportDecompPlusGr$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecompPlusGr$MeanLengthByAge$Age==5] >
                  MeanLengthReportDecompPlusGr$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecompPlusGr$MeanLengthByAge$Age==4]))

#mean for plusgr should be larger than lowest age in plusgr
expect_true(all(MeanLengthReportDecompPlusGr$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecompPlusGr$MeanLengthByAge$Age==5] >
                  MeanLengthReportDecomp$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecomp$MeanLengthByAge$Age==5]))
#mean for plusgr should be smaller than largest age in plusgr
# beware of artifacts for small age groups (convergence or data issues). Using age group 13, rather than 14
expect_true(all(MeanLengthReportDecompPlusGr$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecompPlusGr$MeanLengthByAge$Age==5] <
                  MeanLengthReportDecomp$MeanLengthByAge$MeanIndividualWeight[MeanLengthReportDecomp$MeanLengthByAge$Age==13]))

#context("Test SOP w NAs")
catchAtAgeReportDecompPlusGr <- RstoxFDA::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6)
MeanWeightReportDecompPlusGr <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, PlusGroup=5, Decimals = 6, Threshold = 1000)
expect_true(sum(is.na(MeanWeightReportDecompPlusGr$MeanWeightByAge$MeanIndividualWeight))>1)
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
catchAtAgeReportDecompPlusGr$NbyAge$Gear[catchAtAgeReportDecompPlusGr$NbyAge$Gear==53] <- 52
MeanWeightReportDecompPlusGr$MeanWeightByAge$Gear[MeanWeightReportDecompPlusGr$MeanWeightByAge$Gear==53] <- 52
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
