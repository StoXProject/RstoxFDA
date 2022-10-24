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

#test caa
catchAtAgeCovarDecomp <- RstoxFDA:::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup = 7)
catchAtAgeCovarFlat <- RstoxFDA:::ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup = 7)
RstoxFDA:::PlotCatcAtAgeTotals(catchAtAgeCovarFlat)
RstoxFDA:::PlotCatcAtAgeTotals(catchAtAgeCovarDecomp)


#test covar
catchAtAgeCovarDecomp <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeDecomp, PlusGroup = 7)
catchAtAgeCovarFlat <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeFlat, PlusGroup = 7)
RstoxFDA:::PlotCatcAtAgeCovariances(catchAtAgeCovarFlat)
RstoxFDA:::PlotCatcAtAgeCovariances(catchAtAgeCovarDecomp)

# test sammpling overview
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station <- RstoxFDA:::appendAreaCode(StoxBioticData$Station, RstoxFDA::mainareaFdir2018, "Latitude", "Longitude", "Area")
StoxBioticData$Haul$Gear <- "53"
StoxBioticData$Station$Quarter <- quarters(StoxBioticData$Station$DateTime)
StoxLandingData$Landing$Quarter <- quarters(StoxLandingData$Landing$CatchDate)

# test with one grouping variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear"), Unit = "ton")
RstoxFDA:::PlotSamplingOverviewCell(tab, "Gear")

# test with two grouping variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area"), Unit = "ton")
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area")

# test with three grouping variable, esnure that No landings example is included
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton")
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area")

# test with three grouping variable, 
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8)

# test with non-default Measurement 
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8, Measurement = "WeightMeasurements")

# test with wrong Measurement 
expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8, Measurement = "wrong"), "Does not recognize option for measurement: wrong")

# test with sampling variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = "Platform")
expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8), "Cell plot cannot be constructed when sampling report has sampling variables")
