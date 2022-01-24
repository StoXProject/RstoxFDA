#context("test-StoxAnalysisFunctions: tests RecaResult conversion")
ecaResult <- readRDS(system.file("testresources","ecaResult.rds", package="RstoxFDA"))

stoxFit <- RstoxFDA:::recaFit2Stox(ecaResult$fit, ecaResult$covariateMaps)


expect_true(data.table::is.data.table(stoxFit$FitProportionAtAge$LogLikelihood))
ecaFitConverted <- RstoxFDA:::stox2recaFit(stoxFit)
expect_equal(ecaResult$fit, ecaFitConverted)

stoxPrediction <- RstoxFDA:::ecaResult2Stox(ecaResult$prediction)

expect_equal(nrow(stoxPrediction$CatchAtAge), 130*length(ecaResult$prediction$LengthIntervalsLog))
expect_equal(nrow(stoxPrediction$MeanWeight), 130)
expect_equal(nrow(stoxPrediction$MeanLength), 130)

#context("Test conversionfunctions: test carconversion")
carfile <- system.file("testresources","mainarea_neighbour_correct_codes.txt", package="RstoxFDA")
car <- RstoxFDA::DefineCarNeighbours(NULL, FileName = carfile)

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticData$Station$Area <- StoxLandingData$Landing$Area[c(7,8,13,4,3,4,11,20,4,5,6,20,4,12,3,3,10,4,1,20,11,5,11,5,15,8,14,7,10,6,13,16,11,14,19,20,2,19,11,16,15,5,11,11,9)]
car$Neighbours[9] <- paste(car$Neighbours[9], "30", sep=",")
car$Neighbours[29] <- paste(car$Neighbours[29], "08", sep=",")

prepCar <- RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), CarEffect = "Area", UseCarEffect = T, CarNeighbours = car)
#context("Test convert covariate maps")
ecaprepDummy <- list()
ecaprepDummy <- RstoxFDA:::convertCovariateMap2PrepReca(prepCar)
mc <- RstoxFDA:::convertCovariateMap2Stox(ecaprepDummy)

expect_equal(sort(names(mc)), sort(names(prepCar[names(prepCar) %in% names(mc)])))

#context("Test convert car neigbours")
recaNeib <- RstoxFDA:::convertCarNeighbours2reca(prepCar$AgeLength$CARNeighbours, ecaprepDummy$CovariateMaps)
stoxNeib <- RstoxFDA:::convertCarNeighbours2stox(recaNeib, ecaprepDummy$CovariateMaps)
expect_equal(prepCar$AgeLength$CARNeighbours, stoxNeib)


