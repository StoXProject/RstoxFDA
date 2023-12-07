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

# test based on reported bug
mwal <- readRDS(system.file("testresources", "meanWeightReportIssue.rds", package = "RstoxFDA"))
RstoxFDA:::PlotMeanWeightAtAge(mwal)

#
# test traceplot
#

catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

RstoxFDA:::PlotPosteriorTraces(catchAtAgeFlat)

# test w and wo collapselength
RstoxFDA:::PlotPosteriorTraces(catchAtAgeFlat, LengthInterval = 20)

# test different options for parameter
RstoxFDA:::PlotPosteriorTraces(catchAtAgeFlat, Parameter = "MeanWeight")
RstoxFDA:::PlotPosteriorTraces(catchAtAgeDecomp, Parameter = "MeanLength", Legend=F, UseDefaultPlotSettings = F)

# test with decomp
RstoxFDA:::PlotPosteriorTraces(catchAtAgeDecomp, CatLimit = 1000, UseDefaultPlotSettings = F)
RstoxFDA:::PlotPosteriorTraces(catchAtAgeDecomp)
# test with stock splitting
predictiondatafile <- readRDS(system.file("testresources","stocksplitpred.rds", package="RstoxFDA"))
expect_error(RstoxFDA:::PlotPosteriorTraces(system.file("testresources","stocksplitpred.rds", package="RstoxFDA")), "'RecaCatchAtAge' is not correctly formatted.")
RstoxFDA:::PlotPosteriorTraces(predictiondatafile, Nclust = 10, UseDefaultPlotSettings = F)
expect_warning(RstoxFDA:::PlotPosteriorTraces(predictiondatafile, Nclust = 10), "Argument 'Nclust' is ignored because default settings are chosen.")


#
# test ReportFdaLandings
#

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

tab1 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear", "CatchDate"), Unit = "kg")
tab2 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Gear","Area", "CatchDate"), Unit = "ton")
RstoxFDA:::PlotFisheriesOverviewTemporal(tab1)
RstoxFDA:::PlotFisheriesOverviewTemporal(tab2)

tab3 <- RstoxFDA::ReportFdaLandings(StoxLandingData, GroupingVariables = c("Area"), Unit = "kg")
RstoxFDA:::PlotFisheriesOverviewSpatial(tab3, RstoxFDA::mainareaFdir2018)
RstoxFDA:::PlotFisheriesOverviewSpatial(tab3, RstoxFDA::mainareaFdir2018, AreaLabels = T)

RstoxFDA:::PlotFisheriesOverviewTable(tab3)
RstoxFDA:::PlotFisheriesOverviewTable(tab2)


catchAtAgeFlat <- readRDS(system.file("testresources", "recaPredictionFlat.rds", package="RstoxFDA"))
catchAtAgeDecomp <- readRDS(system.file("testresources", "recaPredictionDecomp.rds", package="RstoxFDA"))

#test caa

catchAtAgeCovarDecomp <- RstoxFDA:::ReportRecaCatchAtAge(catchAtAgeDecomp, PlusGroup = 7)
catchAtAgeCovarFlat <- RstoxFDA:::ReportRecaCatchAtAge(catchAtAgeFlat, PlusGroup = 7)

RstoxFDA:::PlotCatchAtAgeTotals(catchAtAgeCovarFlat)
RstoxFDA:::PlotCatchAtAgeTotals(catchAtAgeCovarDecomp)


#test covar
catchAtAgeCovarDecomp <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeDecomp, PlusGroup = 7)
catchAtAgeCovarFlat <- RstoxFDA:::ReportRecaCatchAtAgeCovariance(catchAtAgeFlat, PlusGroup = 7)
RstoxFDA:::PlotCatchAtAgeCovariances(catchAtAgeCovarFlat)
RstoxFDA:::PlotCatchAtAgeCovariances(catchAtAgeCovarDecomp)

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

RstoxFDA:::PlotSamplingOverviewCell(tab, "Gear", UseDefaultColorSettings = T)

# test with two grouping variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area"), Unit = "ton")
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area")

# test with three grouping variable, esnure that No landings example is included
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton")
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area")

# test with three grouping variable, 
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8, UseDefaultColorSettings = F)

expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, c("Area", "Gear"), MinVessels = 7, MinCatches = 8, UseDefaultColorSettings = F), "Choose at most one column variable. 'ColumnVariable' must be one of the variables in 'GroupingVariables'")
expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, c(), MinVessels = 7, MinCatches = 8, UseDefaultColorSettings = F), "Argument 'ColumnVariable' must be provided.")

# test with non-default Measurement 
RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8, Measurement = "WeightMeasurements", UseDefaultColorSettings = F)

# test with wrong Measurement 
expect_error(RstoxFDA:::PlotSamplingOverviewCell(tab, "Area", MinVessels = 7, MinCatches = 8, Measurement = "wrong", UseDefaultColorSettings = F), "Does not recognize option")

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

RstoxFDA:::PlotSamplingCoverage(tab, Cumulative = T, OtherPercentage = 30)
RstoxFDA:::PlotSamplingCoverage(tab, Cumulative = T, OtherPercentage = 100)

# test with different color scheme
RstoxFDA:::PlotSamplingCoverage(tab, ColorScheme = "Gradient", SamplingUnit = "Catches")

#test with wrong sampling unit
expect_error(RstoxFDA:::PlotSamplingCoverage(tab, SamplingUnit = "wrong"), "Does not recognize option")

#test with wrong measurement
expect_error(RstoxFDA:::PlotSamplingCoverage(tab, Measurement = "wrong"), "Does not recognize option")

# test with sampling variable
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = "IndividualSex")
expect_error(RstoxFDA:::PlotSamplingCoverage(tab), "Coverage plot cannot be constructed when sampling report has sampling variables")


#test sampling variable plot
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = "IndividualSex")

RstoxFDA:::PlotSamplingVariables(tab)
RstoxFDA:::PlotSamplingVariables(tab, Landings = T)
RstoxFDA:::PlotSamplingVariables(tab, Quantity = "AgeReadings")
expect_error(RstoxFDA:::PlotSamplingVariables(tab, Quantity = "wrong"), "Does not recognize option")

tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c("Gear","Area","Quarter"), Unit = "ton", SamplingVariables = c("IndividualSex", "Platform"))
RstoxFDA:::PlotSamplingVariables(tab)

#test sampling variable plot wo grouping variables
tab <- RstoxFDA::ReportFdaSampling(StoxBioticData, StoxLandingData, GroupingVariables = c(), Unit = "ton", SamplingVariables = c("IndividualSex", "Platform"))
RstoxFDA:::PlotSamplingVariables(tab)
expect_error(RstoxFDA:::PlotSamplingVariables(tab, Landings = T), "ReportFdaSamplingData does not partition the fishery. Cannot plot total landings on secondary axis. Consider setting argument 'Landings' to False.")

