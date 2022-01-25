StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

tab1 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear", "CatchDate"), Unit = "kg")
tab2 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear","Area", "CatchDate"), Unit = "ton")
FisheriesOverviewTemporal(tab1)
FisheriesOverviewTemporal(tab2)

tab3 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Area"), Unit = "ton")
FisheriesOverviewSpatial(tab3, RstoxFDA::mainareaFdir2018)

FisheriesOverviewTable(tab3)
FisheriesOverviewTable(tab2)
