designParamsFile <- system.file("testresources", "lotteryParameters", "lotteryDesignNSH.txt", package="RstoxFDA")

#regular read:
designParams <- RstoxFDA::DefineSamplingDesignParameters(NULL, "ResourceFile", designParamsFile)
expect_equal(nrow(designParams$selectionTable), 64)
expect_equal(nrow(designParams$sampleTable), 1)