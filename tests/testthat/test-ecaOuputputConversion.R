context("test-StoxAnalysisFunctions: tests RecaResult conversion")
ecaResult <- readRDS(system.file("testresources","ecaResult.rds", package="RstoxFDA"))

stoxFit <- recaFit2Stox(ecaResult$fit, ecaResult$covariateMaps)
ecaFitConverted <- stox2recaFit(stoxFit)
expect_equal(ecaResult$fit, ecaFitConverted)
