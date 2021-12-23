context("Test ParameterizeRecaModels cache")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())

fpath <- makeTempDirReca()
# check that cache works
paramOut <- ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=155, )
expect_warning(paramOut2 <- ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=155, UseCache=T), "Using cached data for ParameterizeRecaModels")
expect_true(identical(paramOut, paramOut2))
# check that cache fails when arguments are changed
expect_error(ParameterizeRecaModels(prep, 10, 51, 1, fpath, Seed=155, UseCache=T), "Arguments or data are not identical to cached run. Re-run with UseCache=FALSE.")
# check that cache fails when data are changed
prep2 <- prep
prep2$AgeLength$DataMatrix$lengthCM[1]<-5
expect_error(ParameterizeRecaModels(prep2, 10, 50, 1, fpath, Seed=155,  UseCache=T), "Arguments or data are not identical to cached run. Re-run with UseCache=FALSE.")
# check that failed runs didnt touch cache files
expect_warning(paramOut3 <- ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=155, UseCache=T), "Using cached data for ParameterizeRecaModels")
expect_true(identical(paramOut, paramOut2))
# check that new run overwrites cahce files
paramOut4 <- ParameterizeRecaModels(prep, 10, 52, 1, fpath, Seed=156)
expect_warning(paramOut5 <- ParameterizeRecaModels(prep, 10, 52, 1, fpath, Seed=156, UseCache=T), "Using cached data for ParameterizeRecaModels")
expect_true(identical(paramOut4, paramOut5))
removeTempDirReca(fpath)

fpath <- makeTempDirReca()
# check that halts with error when no cache is found
expect_error(ParameterizeRecaModels(prep, 10, 52, 1, fpath, Seed=156, UseCache=T), "No cached input found. Re-run with UseCache=FALSE.")
removeTempDirReca(fpath)

context("PrepRecaEstimate: Missing values warnings")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticData$Cruise$Cruise[1] <- NA
expect_error(expect_warning(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Cruise"))))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$CatchPlatform[1] <- NA
expect_error(expect_warning(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("CatchPlatform"))))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Haul$Gear[1] <- NA
expect_error(expect_warning(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Gear"), RandomEffects = c())))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Sample$CatchFractionCount[1] <- NA
expect_error(expect_warning(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("CatchFractionCount"), RandomEffects = c())))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Individual$IndividualTotalLength[1] <- NA
expect_error(expect_warning(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$DateTime[1] <- NA
expect_error(expect_warning(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())), regexp = "Cannot proceed with missing values for Reca-effects*")


context("PrepRecaEstimate: StocSplitting")
manual <- DefineStockSplittingParameters(DefinitionMethod = "FunctionParameters",
                                         StockNameCC="S1", StockNameS="S2", ProbabilityType1As1=.8,
                                         ProbabilityType1As5=.2, ProbabilityType2As2=.6,
                                         ProbabilityType2As4=.4,	ProbabilityType4As2=.4,
                                         ProbabilityType4As4=.6,	ProbabilityType5As1=.2,
                                         ProbabilityType5As5=.8)
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Individual$otolithtype <- c(rep(c(1,5), 1045), c(1,5,1))

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), UseStockSplitting=T, UseStockSplittingError=T, StockSplittingParameters=manual)
expect_true(prep$GlobalParameters$GlobalParameters$CC)
expect_true(prep$GlobalParameters$GlobalParameters$CCerror)
expect_true(is.StockSplittingParameters(prep$AgeLength$StockSplittingParameters))
expect_true(is.null(prep$AgeLength$CCerrorList))
fpath <- makeTempDirReca()
print("Parameterize CC")
param <- ParameterizeRecaModels(prep, 100, 500, ResultDirectory = fpath, Seed = 100)

context("check that age group names are set correct for stock splitting")
expect_equal(sum(is.na(param$FitProportionAtAge$constant$Age)), 0)
expect_equal(param$FitProportionAtAge$constant$Age[1], "S1 2")


context("Check that back conversion to eca objects works fine with stock splitting")
ecafit <- stox2recaFit(param)

result <- RunRecaModels(param, StoxLandingData = StoxLandingData, Seed = 100)
expect_true("Stock" %in% result$GroupingVariables$GroupingVariables)
resultAgg <- RunRecaModels(param, StoxLandingData = StoxLandingData, GroupingVariables = c("Gear"))
expect_true(all(c("Stock", "Gear") %in% resultAgg$GroupingVariables$GroupingVariables))

removeTempDirReca(fpath)
expect_true(is.StockSplittingParameters(param$AgeLength$StockSplittingParameters))
expect_true(is.null(prep$AgeLength$CCerrorList))
expect_equal(param$AgeLength$StockSplittingParameters, manual)
expect_true(is.RecaCatchAtAge(result))
expect_true("Stock" %in% names(result$CatchAtAge))
expect_true("Stock" %in% names(result$MeanLength))
expect_true("Stock" %in% names(result$MeanWeight))

context("PrepRecaEstimate: AgerrorMatrix")
ageerorfile <- system.file("testresources","AgeErrorHirstEtAl2012.txt", package="RstoxFDA")
ageerror <- DefineAgeErrorMatrix(FileName = ageerorfile)
expect_true(is.AgeErrorMatrix(ageerror))
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), UseAgingError = T, AgeErrorMatrix = ageerror, MinAge = 0, MaxAge = 14)
expect_true(!is.null(prep$AgeLength$AgeErrorMatrix))
est <- RunRecaEstimate(prep, 10, 50)

context("PrepareRecaEstimate: configuration tests")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxLandingData$Landing$NewConst <- 1
StoxBioticData$Station$NewConst <- 1


expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("NewConst"), RandomEffects = c()), "Only one level for categorical covariate NewConst")
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("NewConst")), "Only one level for categorical covariate NewConst")

StoxBioticData$Station$Area <- StoxLandingData$Landing$Area[c(7,8,13,4,3,4,11,20,4,5,6,20,4,12,3,3,10,4,1,20,11,5,11,5,15,8,14,7,10,6,13,16,11,14,19,20,2,19,11,16,15,5,11,11,9)]
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Area"), RandomEffects = c("Area")), "Some random effects are also specified as fixed effects: Area")
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"), CarEffect = "Area"), "UseCarEffect is False, while the parameter 'CarEffect' is given")
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"), CarEffect = "Area", UseCarEffect = T, CarNeighbours = list()), "The CAR effect Area is also specified as fixed effect or random effect")

#check CAR value cehcks
carfile <- system.file("testresources","mainarea_neighbour_correct_codes.txt", package="RstoxFDA")
car <- DefineCarNeighbours(NULL, FileName = carfile)
expect_true(is.CarNeighbours(car))
car$Neighbours[9] <- paste(car$Neighbours[9], "30", sep=",")
car$Neighbours[29] <- paste(car$Neighbours[29], "08", sep=",")
prepCar <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), CarEffect = "Area", UseCarEffect = T, CarNeighbours = car)

fpath <- makeTempDirReca()
paramOut <- ParameterizeRecaModels(prepCar, 10, 50, 1, fpath)
result <- RunRecaModels(paramOut, StoxLandingData)
removeTempDirReca(fpath)
expect_true(is.RecaParameterData(paramOut))
expect_true(is.RecaCatchAtAge(result))


context("ParameterizeRecaModels: simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())

fpath <- makeTempDirReca()
paramOut <- ParameterizeRecaModels(prep, 10, 50, 1, fpath)

expect_true(c("FitLengthGivenAge") %in% names(paramOut))
expect_equal(length(paramOut$FitLengthGivenAge), 4)
expect_true(is.RecaParameterData((paramOut)))

context("test-StoxAnalysisFunctions: RunRecaModels")
results <- RunRecaModels(paramOut, StoxLandingData)
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(is.RecaCatchAtAge(results))

context("test-StoxAnalysisFunctions: RunRecaModels with GroupingVariables")
results <- RunRecaModels(paramOut, StoxLandingData, GroupingVariables = c("Area", "Usage"))
expect_equal(length(unique(paste(results$CatchAtAge$Area, results$CatchAtAge$Usage))), length(unique(paste(StoxLandingData$Landing$Area, StoxLandingData$Landing$Usage))))

expect_true(is.RecaCatchAtAge(results))
removeTempDirReca(fpath)

context("test-StoxAnalysisFunctions: RunRecaModels wirh random effects in landings")

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Haul$Gear <- StoxLandingData$Landing$Gear[sample.int(20,45, replace=T)]

fpath <- makeTempDirReca()

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear"))
paramOut <- ParameterizeRecaModels(prep, 50, 50, 1, fpath, Seed=100)

results <- RunRecaModels(paramOut, StoxLandingData, Seed=100)

expect_true("Gear" %in% names(paramOut$Landings$AgeLengthCov))
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(is.RecaCatchAtAge(results))

paramOut <- ParameterizeRecaModels(prep, 10, 50, 1, fpath)
results <- RunRecaModels(paramOut, StoxLandingData, GroupingVariables = "Gear")
expect_true("Gear" %in% names(paramOut$Landings$AgeLengthCov))
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(is.RecaCatchAtAge(results))

context("RunRecaModels: Test collapse Length")
resultsWlength <- RunRecaModels(paramOut, StoxLandingData, GroupingVariables = "Gear", CollapseLength = F)
expect_true("Age" %in% names(resultsWlength$CatchAtAge))
expect_true(is.RecaCatchAtAge(resultsWlength))
expect_equal(nrow(results$CatchAtAge)*2*results$CatchAtAge$Length[1], nrow(resultsWlength$CatchAtAge))
expect_equal(length(unique(results$CatchAtAge$Length)),1)
expect_gt(length(unique(resultsWlength$CatchAtAge$Length)), 1)

context("RunRecaModels: Test collapse Length wo Aggregation")
results <- RunRecaModels(paramOut, StoxLandingData)
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(is.RecaCatchAtAge(results))

resultsWlength <- RunRecaModels(paramOut, StoxLandingData, CollapseLength = F)
expect_true("Age" %in% names(resultsWlength$CatchAtAge))
expect_true(is.RecaCatchAtAge(resultsWlength))
expect_equal(nrow(results$CatchAtAge)*2*results$CatchAtAge$Length[1], nrow(resultsWlength$CatchAtAge))
expect_equal(length(unique(results$CatchAtAge$Length)),1)
expect_gt(length(unique(resultsWlength$CatchAtAge$Length)), 1)


context("test-StoxAnalysisFunctions: PrepareRecaEstimate missing arguments")
expect_error(ParameterizeRecaModels(prep, 10, 50, 1, fpath, Lgamodel = NULL), "Parameter 'Lgamodel' must be provided.")

removeTempDirReca(fpath)

context("test-StoxAnalysisFunctions: PrepareRecaEstimate simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())
expect_equal(length(prep$CovariateMaps$CovariateMaps_randomEffects_AgeLength_catchSample$values), length(unique(StoxBioticData$Individual$HaulKey)))

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), MinAge=1, MaxAge=30)

context("test-StoxAnalysisFunctions: RunRecaEstimate simple case")
result <- RunRecaEstimate(prep, 10, 50, Thin=1)
expect_true(all(c("input", "fit", "prediction", "covariateMaps") %in% names(result)))
expect_equal(dim(result$prediction$TotalCount)[3], 10)


context("test-StoxAnalysisFunctions: PrepareRecaEstimate, missing sample dates")
StoxBioticData$Station$DateTime[1] <- NA
expect_error(suppressWarnings(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate, stratified samples (nFish), missing CatchFractionCount")
StoxBioticDataDelp <- readRDS(system.file("testresources","StoxBioticDelpr.rds", package="RstoxFDA"))
expect_error(suppressWarnings(PrepareRecaEstimate(StoxBioticDataDelp, StoxLandingData, FixedEffects = c(), RandomEffects = c())))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate, stratified samples (nFish)")
StoxBioticDataDelp$Sample$CatchFractionCount[2] <- 3000
prep <- PrepareRecaEstimate(StoxBioticDataDelp, StoxLandingData, FixedEffects = c(), RandomEffects = c())

#context("test-StoxAnalysisFunctions: RunRecaEstimate, stratified samples (nFish)")
#to few iterations to converge consistently. removing test
#est <- RunRecaEstimate(prep, 10, 200, 0)

context("test-StoxAnalysisFunctions: PrepareRecaEstimate with  with random effect Area")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticData$Station$Area <- c(rep(StoxLandingData$Landing$Area[10], 20), rep(StoxLandingData$Landing$Area[20], 25))
StoxBioticData$Station$GG <- c(rep(StoxLandingData$Landing$Gear[10], 20), rep(StoxLandingData$Landing$Gear[20], 25))
StoxLandingData$Landing$GG <- StoxLandingData$Landing$Gear

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"))
expect_true("Area" %in% names(prep$Landings$AgeLengthCov))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate cellEffect")
prepCell <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area", "GG"), CellEffect = "All")
expect_equal(prepCell$AgeLength$info$interaction[prepCell$AgeLength$info$covariate=="Area"], 1)
expect_equal(prepCell$AgeLength$info$interaction[prepCell$AgeLength$info$covariate=="GG"], 1)

fpath <- makeTempDirReca()
paramOut <- ParameterizeRecaModels(prepCell, 10, 50, 1, fpath, Seed = 451)
expect_true("cell" %in% names(paramOut$FitProportionAtAge))
removeTempDirReca(fpath)


context("test-StoxAnalysisFunctions: RunRecaEstimate with random effect Area")
est <- RunRecaEstimate(prep, 10, 100, 0, Seed = 112)
expect_true("Area" %in% names(est$fit$ProportionAtAge$Intercept$cov))

context("RunRecaEstimate not providing burnin")
expect_error(RunRecaEstimate(prep, 10))
