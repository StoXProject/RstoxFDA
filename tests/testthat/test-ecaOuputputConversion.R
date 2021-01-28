context("test-StoxAnalysisFunctions: tests RecaResult conversion")
ecaResult <- readRDS(system.file("testresources","ecaResult.rds", package="RstoxFDA"))

stoxFit <- recaFit2Stox(ecaResult$fit, ecaResult$covariateMaps)


expect_true(is.data.table(stoxFit$FitProportionAtAge$LogLikelihood))
ecaFitConverted <- stox2recaFit(stoxFit)
expect_equal(ecaResult$fit, ecaFitConverted)

stoxPrediction <- ecaResult2Stox(ecaResult$prediction)

expect_equal(nrow(stoxPrediction$CatchAtAge), 130*length(ecaResult$prediction$LengthIntervalsLog))
expect(nrow(stoxPrediction$MeanWeight), 130)
expect(nrow(stoxPrediction$MeanLength), 130)
