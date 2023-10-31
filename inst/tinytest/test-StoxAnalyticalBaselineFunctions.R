designParamsFile <- system.file("testresources", "lotteryParameters", "lotteryDesignNSH.txt", package="RstoxFDA")

#regular read:
designParams <- RstoxFDA::DefineMultiStageSamplingParameters(NULL, "ResourceFile", designParamsFile)
expect_true(RstoxFDA:::is.MultiStageSamplingParametersData(designParams))
expect_equal(nrow(designParams$SelectionTable), 64)
expect_equal(nrow(designParams$SampleTable), 1)
expect_equal(ncol(designParams$StratificationVariables), 1)
expect_equal(nrow(designParams$StratificationVariables), 1)

#define from data
suppressWarnings(StoxBioticData <- RstoxData::StoxBiotic(RstoxData::ReadBiotic(system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA"))))
designParamsSB <- RstoxFDA::DefineMultiStageSamplingParameters(NULL, "AdHocStoxBiotic", StoxBioticData=StoxBioticData, SamplingUnitId = "Individual", StratificationColumns = c("SpeciesCategoryKey"))
expect_true(RstoxFDA:::is.MultiStageSamplingParametersData(designParamsSB))
#compare names of output with stratification variables to output without

expect_true(all(names(designParamsSB$SampleTable) == names(designParams$SampleTable)))
expect_true(all(names(designParamsSB$SelectionTable) == names(designParams$SelectionTable)))
expect_equal(nrow(designParamsSB$SelectionTable), 75)
expect_equal(nrow(designParamsSB$SampleTable), 2)
expect_equal(ncol(designParamsSB$StratificationVariables), 2)
expect_equal(nrow(designParamsSB$StratificationVariables), 2)
