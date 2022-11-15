#
# test plotMeanWeightAtAge
#
catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

MeanWeightReportDecomp <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeDecomp, Decimals = 4, Unit = "kg")
RstoxFDA:::PlotMeanWeightAtAge(MeanWeightReportDecomp)
MeanWeightReport <- RstoxFDA::ReportRecaWeightAtAge(catchAtAgeFlat, Decimals = 4, Unit = "g")
RstoxFDA:::PlotMeanWeightAtAge(MeanWeightReport)
expect_error(RstoxFDA:::PlotMeanWeightAtAge(catchAtAgeFlat), "Malformed argument: 'ReportFdaWeightAtAgeData'")

MeanLengthReportDecomp <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeDecomp, Decimals = 4, Unit = "cm")
RstoxFDA:::PlotMeanLengthAtAge(MeanLengthReportDecomp)
MeanLengthReport <- RstoxFDA::ReportRecaLengthAtAge(catchAtAgeFlat, Decimals = 4, Unit = "m")
RstoxFDA:::PlotMeanLengthAtAge(MeanLengthReport)
expect_error(RstoxFDA:::PlotMeanLengthAtAge(catchAtAgeFlat), "Malformed argument: 'ReportFdaLengthAtAgeData'")


#
# test traceplot
#

catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

RstoxFDA:::PlotPosteriorTraces(catchAtAgeFlat)

# test w and wo collapselength
RstoxFDA:::PlotPosteriorTraces(catchAtAgeFlat, LengthInterval = 20, LegendLimit = 1)

# test different options for parameter
RstoxFDA:::PlotPosteriorTraces(catchAtAgeFlat, Parameter = "MeanWeight")
RstoxFDA:::PlotPosteriorTraces(catchAtAgeDecomp, Parameter = "MeanLength")

# test with decomp
RstoxFDA:::PlotPosteriorTraces(catchAtAgeDecomp)
# test with stock splitting
predictiondatafile <- readRDS(system.file("testresources","stocksplitpred.rds", package="RstoxFDA"))
expect_error(RstoxFDA:::PlotPosteriorTraces(system.file("testresources","stocksplitpred.rds", package="RstoxFDA"), Nclust = 10), "'RecaCatchAtAge' is not correctly formatted.")
RstoxFDA:::PlotPosteriorTraces(predictiondatafile, Nclust = 10)


#
# test ReportFdaLandings
#

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

expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, c("Area", "Gear"), MinVessels = 7, MinCatches = 8), "Choose at most one column variable. 'ColumnVariable' must be one of the variables in 'GroupingVariables'")
expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, c(), MinVessels = 7, MinCatches = 8), "Argument 'ColumnVariable' must be provided.")

# test with non-default Measurement 
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8, Measurement = "WeightMeasurements")

# test with wrong Measurement 
expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8, Measurement = "wrong"), "Does not recognize option for measurement: wrong")

# test with sampling variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = "Platform")
expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8), "Cell plot cannot be constructed when sampling report")


#
# test sampling coverage plot
#
# test with one grouping variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear"), Unit = "ton")
RstoxFDA:::PlotSamplingCoverage(tab)

# test with three grouping variable, 
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear", "Area", "Quarter"), Unit = "ton")
RstoxFDA:::PlotSamplingCoverage(tab, Cumulative = T)

# test with different color scheme
RstoxFDA:::PlotSamplingCoverage(tab, ColorScheme = "Gradient", SamplingUnit = "Catches")

#test with wrong sampling unit
expect_error(RstoxFDA:::PlotSamplingCoverage(tab, SamplingUnit = "wrong"), "Does not recognize option wrong for 'SamplingUnit'")

#test with wrong measurement
expect_error(RstoxFDA:::PlotSamplingCoverage(tab, Measurement = "wrong"), "Does not recognize option wrong for 'Measurement'")

# test with sampling variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = "IndividualSex")
expect_error(RstoxFDA:::PlotSamplingCoverage(tab), "Coverage plot cannot be constructed when sampling report has sampling variables")


#test sampling variable plot
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = "IndividualSex")
RstoxFDA:::PlotSamplingVariables(tab)
RstoxFDA:::PlotSamplingVariables(tab, Quantity = "AgeReadings")
expect_error(RstoxFDA:::PlotSamplingVariables(tab, Quantity = "wrong"), "Does not recognize option wrong for 'Quantity'")

tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = c("IndividualSex", "Platform"))
RstoxFDA:::PlotSamplingVariables(tab)

#test sampling variable plot wo grouping variables
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c(), Unit = "ton", SamplingVariables = c("IndividualSex", "Platform"))
RstoxFDA:::PlotSamplingVariables(tab)

