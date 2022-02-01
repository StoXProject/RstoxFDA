StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

tab1 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear", "CatchDate"), Unit = "kg")
tab2 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear","Area", "CatchDate"), Unit = "ton")
RstoxFDA:::PlotFisheriesOverviewTemporal(tab1)
RstoxFDA:::PlotFisheriesOverviewTemporal(tab2)

tab3 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Area"), Unit = "ton")
RstoxFDA:::PlotFisheriesOverviewSpatial(tab3, RstoxFDA::mainareaFdir2018)

RstoxFDA:::PlotFisheriesOverviewTable(tab3)
RstoxFDA:::PlotFisheriesOverviewTable(tab2)


catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))
catchAtAgeCovarDecomp <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeDecomp, PlusGroup = 7)
catchAtAgeCovarFlat <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeFlat, PlusGroup = 7)
RstoxFDA:::PlotCatcAtAgeCovariances(catchAtAgeCovarFlat)
RstoxFDA:::PlotCatcAtAgeCovariances(catchAtAgeCovarDecomp)
