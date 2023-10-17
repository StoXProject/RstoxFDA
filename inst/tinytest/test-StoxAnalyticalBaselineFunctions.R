designParamsFile <- system.file("testresources", "lotteryParameters", "lotteryDesignNSH.txt", package="RstoxFDA")

#regular read:
designParams <- RstoxFDA::DefineSamplingDesignParameters(NULL, "ResourceFile", designParamsFile)
expect_equal(nrow(designParams$selectionTable), 64)
expect_equal(nrow(designParams$sampleTable), 1)
expect_equal(ncol(designParams$stratificationVariables), 1)
expect_equal(nrow(designParams$stratificationVariables), 1)

#define from data
suppressWarnings(StoxBioticData <- RstoxData::StoxBiotic(RstoxData::ReadBiotic(system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA"))))
designParamsSB <- RstoxFDA::DefineSamplingDesignParameters(NULL, "AdHocStoxBiotic", StoxBioticData=StoxBioticData, SamplingUnitId = "Individual", StratificationColumns = c("SpeciesCategoryKey"))

#compare names of output with stratification variables to output without

expect_true(all(names(designParamsSB$sampleTable) == names(designParams$sampleTable)))
expect_true(all(names(designParamsSB$selectionTable) == names(designParams$selectionTable)))
browser()
expect_equal(nrow(designParamsSB$selectionTable), 75)
expect_equal(nrow(designParamsSB$sampleTable), 2)
expect_equal(ncol(designParamsSB$stratificationVariables), 2)
expect_equal(nrow(designParamsSB$stratificationVariables), 2)
