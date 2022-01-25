StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

tab1 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear", "CatchDate"), Unit = "kg")
tab2 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear","Area", "CatchDate"), Unit = "ton")
FisheriesOverviewTemporal(tab1)
FisheriesOverviewTemporal(tab2)

FisheriesOverviewSpatial(tab2)
browser()