StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

tab1 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear", "CatchDate"), Unit = "kg")
tab2 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear","Area", "CatchDate"), Unit = "ton")
RstoxFDA:::FisheriesOverviewTemporal(tab1)
RstoxFDA:::FisheriesOverviewTemporal(tab2)

tab3 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Area"), Unit = "ton")
RstoxFDA:::FisheriesOverviewSpatial(tab3, RstoxFDA::mainareaFdir2018)

RstoxFDA:::FisheriesOverviewTable(tab3)
RstoxFDA:::FisheriesOverviewTable(tab2)
