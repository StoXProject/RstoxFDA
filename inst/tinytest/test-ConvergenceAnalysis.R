# ECA tests are only run if Reca is installed.
if (nchar(system.file(package="Reca"))>0){
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
}
