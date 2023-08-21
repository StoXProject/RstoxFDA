#context("Test ParameterizeRecaModels cache")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticDataWDupl <- StoxBioticData
StoxBioticDataWDupl$Station <- rbind(StoxBioticDataWDupl$Station, StoxBioticDataWDupl$Station)
expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticDataWDupl, StoxLandingData, FixedEffects = c(), RandomEffects = c()), "Malformed StoxBioticData.")

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())

#test non-linear setting
prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())
fpath <- RstoxFDA:::makeTempDirReca()
paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=99, Lgamodel = "log-linear")
paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=99, Lgamodel = "non-linear")
RstoxFDA:::removeTempDirReca(fpath)

fpath <- RstoxFDA:::makeTempDirReca()
# check that seed works
paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=99)
paramOut2 <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed = paramOut$GlobalParameters$GlobalParameters$seed)
paramOut3 <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed = paramOut$GlobalParameters$GlobalParameters$seed+1)

expect_equal(paramOut2$FitProportionAtAge, paramOut$FitProportionAtAge)
expect_equal(paramOut2$FitLengthGivenAge, paramOut$FitLengthGivenAge)
expect_equal(paramOut2$FitWeightGivenLength, paramOut$FitWeightGivenLength)
expect_true(!all(paramOut$FitWeightGivenLength$fish$tau_Intercept == paramOut3$FitWeightGivenLength$fish$tau_Intercept))


# check that cache works
paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=155)
expect_warning(paramOut2 <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=155, UseCache=T), "Using cached data for ParameterizeRecaModels")
expect_true(identical(paramOut, paramOut2))
# check that cache fails when arguments are changed
expect_error(RstoxFDA:::ParameterizeRecaModels(prep, 10, 51, 1, fpath, Seed=155, UseCache=T), "Arguments or data are not identical to cached run. Re-run with UseCache=FALSE.")
# check that cache fails when data are changed
prep2 <- prep
prep2$AgeLength$DataMatrix$lengthCM[1]<-5
expect_error(RstoxFDA:::ParameterizeRecaModels(prep2, 10, 50, 1, fpath, Seed=155,  UseCache=T), "Arguments or data are not identical to cached run. Re-run with UseCache=FALSE.")
# check that failed runs didnt touch cache files
expect_warning(paramOut3 <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=155, UseCache=T), "Using cached data for ParameterizeRecaModels")
expect_true(identical(paramOut, paramOut2))
# check that new run overwrites cahce files
paramOut4 <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 52, 1, fpath, Seed=156)
expect_warning(paramOut5 <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 52, 1, fpath, Seed=156, UseCache=T), "Using cached data for ParameterizeRecaModels")
expect_true(identical(paramOut4, paramOut5))
RstoxFDA:::removeTempDirReca(fpath)

fpath <- RstoxFDA:::makeTempDirReca()
# check that halts with error when no cache is found
expect_error(RstoxFDA:::ParameterizeRecaModels(prep, 10, 52, 1, fpath, Seed=156, UseCache=T), "No cached input found. Re-run with UseCache=FALSE.")
RstoxFDA:::removeTempDirReca(fpath)

#context("PrepRecaEstimate: Missing values warnings")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticData$Cruise$Cruise[1] <- NA
expect_warning(expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Cruise"))))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$CatchPlatform[1] <- NA
expect_warning(expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("CatchPlatform"))))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Haul$Gear[1] <- NA
expect_warning(expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Gear"), RandomEffects = c())))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Sample$CatchFractionNumber[1] <- NA
expect_warning(expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("CatchFractionNumber"), RandomEffects = c())))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Individual$IndividualTotalLength[1] <- NA
expect_warning(expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())))
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Station$DateTime[1] <- NA
expect_warning(expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c()),"Cannot proceed with missing values for Reca-effects"))

#context("PrepRecaEstimate: Missing cell warnings")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticData$Station$Area <- NA
StoxBioticData$Station$Area <- c(StoxLandingData$Landing$Area[1:20], StoxLandingData$Landing$Area[1:20], StoxLandingData$Landing$Area[1:5])
expect_warning(expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Gear", "Area"), RandomEffects = c())))

#context("PrepRecaEstimate: StockSplitting")
manual <- RstoxFDA:::DefineStockSplittingParameters(DefinitionMethod = "FunctionParameters",
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

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), UseStockSplitting=T, UseStockSplittingError=T, StockSplittingParameters=manual)
expect_true(prep$GlobalParameters$GlobalParameters$CC)
expect_true(prep$GlobalParameters$GlobalParameters$CCerror)
expect_true(RstoxFDA:::is.StockSplittingParameters(prep$AgeLength$StockSplittingParameters))
expect_true(is.null(prep$AgeLength$CCerrorList))
fpath <- RstoxFDA:::makeTempDirReca()
#make sure it works with trailing "/" on path
pathWtrailing <- paste0(fpath, "/")
param <- RstoxFDA:::ParameterizeRecaModels(prep, 100, 400, ResultDirectory = pathWtrailing, Seed = 100)

pathWsubDir <- file.path(fpath, "subdir")
param <- RstoxFDA:::ParameterizeRecaModels(prep, 100, 400, ResultDirectory = pathWsubDir, Seed = 100)

#context("check that age group names are set correct for stock splitting")
expect_equal(sum(is.na(param$FitProportionAtAge$constant$Age)), 0)
expect_equal(param$FitProportionAtAge$constant$Age[1], "S1 2")


#context("Check that back conversion to eca objects works fine with stock splitting")
ecafit <- RstoxFDA:::stox2recaFit(param)

result <- RstoxFDA:::RunRecaModels(param, StoxLandingData = StoxLandingData)

expect_true("Stock" %in% result$GroupingVariables$GroupingVariables)
resultAgg <- RstoxFDA:::RunRecaModels(param, StoxLandingData = StoxLandingData, GroupingVariables = c("Gear"))
expect_true(all(c("Stock", "Gear") %in% resultAgg$GroupingVariables$GroupingVariables))

RstoxFDA:::removeTempDirReca(fpath)
expect_true(RstoxFDA:::is.StockSplittingParameters(param$AgeLength$StockSplittingParameters))
expect_true(is.null(prep$AgeLength$CCerrorList))
expect_equal(param$AgeLength$StockSplittingParameters, manual)
expect_true(RstoxFDA::is.RecaCatchAtAge(result))
expect_true("Stock" %in% names(result$CatchAtAge))
expect_true("Stock" %in% names(result$MeanLength))
expect_true("Stock" %in% names(result$MeanWeight))

#stock splitting w warning
StoxBioticData$Individual$otolithtype[1] <- 9
expect_warning(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), UseStockSplitting=T, UseStockSplittingError=T, StockSplittingParameters=manual), "StoX: Some aged fish does not have Otolithtype set, or have it set to an unrecognized value. This may slow down Stox processing of Reca results.")
StoxBioticData$Individual$IndividualAge[1] <- NA
expect_silent(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), UseStockSplitting=T, UseStockSplittingError=T, StockSplittingParameters=manual))

#context("PrepRecaEstimate: AgerrorMatrix")
ageerorfile <- system.file("testresources","AgeErrorHirstEtAl2012.txt", package="RstoxFDA")
ageerror <- RstoxFDA::DefineAgeErrorMatrix(FileName = ageerorfile)
expect_true(RstoxFDA::is.AgeErrorMatrix(ageerror))
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), UseAgingError = T, AgeErrorMatrix = ageerror, MinAge = 0, MaxAge = 14)
expect_true(!is.null(prep$AgeLength$AgeErrorMatrix))
expect_warning(est <- RstoxFDA::RunRecaEstimate(prep, 10, 50))

#context("PrepareRecaEstimate: configuration tests")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxLandingData$Landing$NewConst <- 1
StoxBioticData$Station$NewConst <- 1


expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("NewConst"), RandomEffects = c()), "Only one level for categorical covariate NewConst")
expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("NewConst")), "Only one level for categorical covariate NewConst")

StoxBioticData$Station$Area <- StoxLandingData$Landing$Area[c(7,8,13,4,3,4,11,20,4,5,6,20,4,12,3,3,10,4,1,20,11,5,11,5,15,8,14,7,10,6,13,16,11,14,19,20,2,19,11,16,15,5,11,11,9)]
expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Area"), RandomEffects = c("Area")), "Some random effects are also specified as fixed effects: Area")
expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"), CarEffect = "Area"), "UseCarEffect is False, while the parameter 'CarEffect' is given")
expect_error(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"), CarEffect = "Area", UseCarEffect = T, CarNeighbours = list()), "The CAR effect Area is also specified as fixed effect or random effect")

#check CAR value cehcks
carfile <- system.file("testresources","mainarea_neighbour_correct_codes.txt", package="RstoxFDA")
car <- RstoxFDA::DefineCarNeighbours(NULL, FileName = carfile)
expect_true(RstoxFDA::is.CarNeighbours(car))
car$Neighbours[9] <- paste(car$Neighbours[9], "30", sep=",")
car$Neighbours[29] <- paste(car$Neighbours[29], "08", sep=",")
prepCar <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), CarEffect = "Area", UseCarEffect = T, CarNeighbours = car)

fpath <- RstoxFDA:::makeTempDirReca()
paramOut <- RstoxFDA:::ParameterizeRecaModels(prepCar, 10, 50, 1, fpath, Seed=42)
result <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData)
RstoxFDA:::removeTempDirReca(fpath)
expect_true(RstoxFDA::is.RecaParameterData(paramOut))
expect_true(RstoxFDA::is.RecaCatchAtAge(result))


#context("ParameterizeRecaModels: simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())

fpath <- RstoxFDA:::makeTempDirReca()
paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=43)

expect_true(c("FitLengthGivenAge") %in% names(paramOut))
expect_equal(length(paramOut$FitLengthGivenAge), 4)
expect_true(RstoxFDA::is.RecaParameterData((paramOut)))

#context("test-StoxAnalysisFunctions: RunRecaModels")
results <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData)
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(RstoxFDA::is.RecaCatchAtAge(results))

#context("test-StoxAnalysisFunctions: RunRecaModels with GroupingVariables")
results <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData, GroupingVariables = c("Area", "Usage"))
expect_equal(length(unique(paste(results$CatchAtAge$Area, results$CatchAtAge$Usage))), length(unique(paste(StoxLandingData$Landing$Area, StoxLandingData$Landing$Usage))))
expect_true(RstoxFDA::is.RecaCatchAtAge(results))
expect_warning(RstoxFDA:::RunRecaModels(paramOut, StoxLandingData, GroupingVariables = c("Area", "Usage"), CollapseLength = F), "StoX: Producing estimates for all length groups in combination with age and several 'GroupingVariables'. This may exhaust memory, consider the option 'CollapseLength'")
RstoxFDA:::removeTempDirReca(fpath)

#context("test-StoxAnalysisFunctions: RunRecaModels with random effects in landings")

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxBioticData$Haul$Gear <- StoxLandingData$Landing$Gear[sample.int(20,45, replace=T)]

fpath <- RstoxFDA:::makeTempDirReca()

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear"))

#check that seed works
paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 50, 50, 1, fpath, Seed=111)
seed <- sample.int(.Machine$integer.max, 1)
results <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData)
paramOut2 <- RstoxFDA:::ParameterizeRecaModels(prep, 50, 50, 1, fpath, Seed=paramOut$GlobalParameters$GlobalParameters$seed)
results2 <- RstoxFDA:::RunRecaModels(paramOut2, StoxLandingData)
paramOut3 <- RstoxFDA:::ParameterizeRecaModels(prep, 50, 50, 1, fpath, Seed=paramOut$GlobalParameters$GlobalParameters$seed+1)
results3 <- RstoxFDA:::RunRecaModels(paramOut3, StoxLandingData)
expect_equal(results2, results)
expect_true(!all(results$CatchAtAge == results3$CatchAtAge))
#/ seed

paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 50, 50, 1, fpath, Seed=100)
results <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData)

expect_true("Gear" %in% names(paramOut$Landings$AgeLengthCov))
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(RstoxFDA::is.RecaCatchAtAge(results))

paramOut <- RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, fpath, Seed=44)
results <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData, GroupingVariables = "Gear")
expect_true("Gear" %in% names(paramOut$Landings$AgeLengthCov))
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(RstoxFDA::is.RecaCatchAtAge(results))

#context("RunRecaModels: Test collapse Length")
resultsWlength <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData, GroupingVariables = "Gear", CollapseLength = F)
expect_true("Age" %in% names(resultsWlength$CatchAtAge))
expect_true(RstoxFDA::is.RecaCatchAtAge(resultsWlength))
expect_equal(nrow(results$CatchAtAge)*2*results$CatchAtAge$Length[1], nrow(resultsWlength$CatchAtAge))
expect_equal(length(unique(results$CatchAtAge$Length)),1)
expect_true(length(unique(resultsWlength$CatchAtAge$Length))> 1)

#context("RunRecaModels: Test collapse Length wo Aggregation")
results <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData)
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(RstoxFDA::is.RecaCatchAtAge(results))

resultsWlength <- RstoxFDA:::RunRecaModels(paramOut, StoxLandingData, CollapseLength = F)
expect_true("Age" %in% names(resultsWlength$CatchAtAge))
expect_true(RstoxFDA::is.RecaCatchAtAge(resultsWlength))
expect_equal(nrow(results$CatchAtAge)*2*results$CatchAtAge$Length[1], nrow(resultsWlength$CatchAtAge))
expect_equal(length(unique(results$CatchAtAge$Length)),1)
expect_true(length(unique(resultsWlength$CatchAtAge$Length))> 1)


#context("test-StoxAnalysisFunctions: PrepareRecaEstimate missing arguments")
expect_error(RstoxFDA:::ParameterizeRecaModels(prep, 10, 50, 1, ResultDirectory = NULL), "Argument 'ResultDirectory' must be provided.")

RstoxFDA:::removeTempDirReca(fpath)

#context("test-StoxAnalysisFunctions: PrepareRecaEstimate simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())
expect_equal(length(prep$CovariateMaps$CovariateMaps_randomEffects_AgeLength_catchSample$values), length(unique(StoxBioticData$Individual$HaulKey)))

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), MinAge=1, MaxAge=30)

#context("test-StoxAnalysisFunctions: RunRecaEstimate simple case")
expect_warning(result <- RstoxFDA::RunRecaEstimate(prep, 10, 50, Thin=1))
expect_true(all(c("input", "fit", "prediction", "covariateMaps") %in% names(result)))
expect_equal(dim(result$prediction$TotalCount)[3], 10)


#context("test-StoxAnalysisFunctions: PrepareRecaEstimate, missing sample dates")
StoxBioticData$Station$DateTime[1] <- NA
expect_error(suppressWarnings(RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())))

#context("test-StoxAnalysisFunctions: PrepareRecaEstimate, stratified samples (nFish), missing CatchFractionNumber")
StoxBioticDataDelp <- readRDS(system.file("testresources","StoxBioticDelpr.rds", package="RstoxFDA"))
expect_error(suppressWarnings(RstoxFDA:::PrepareRecaEstimate(StoxBioticDataDelp, StoxLandingData, FixedEffects = c(), RandomEffects = c())))

#context("test-StoxAnalysisFunctions: PrepareRecaEstimate, stratified samples (nFish)")
StoxBioticDataDelp$Sample$CatchFractionNumber[2] <- 3000
prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticDataDelp, StoxLandingData, FixedEffects = c(), RandomEffects = c())

##context("test-StoxAnalysisFunctions: RunRecaEstimate, stratified samples (nFish)")
#to few iterations to converge consistently. removing test
#est <- RunRecaEstimate(prep, 10, 200, 0)

#context("test-StoxAnalysisFunctions: PrepareRecaEstimate with  with random effect Area")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

StoxBioticData$Station$Area <- c(rep(StoxLandingData$Landing$Area[10], 20), rep(StoxLandingData$Landing$Area[20], 25))
StoxBioticData$Station$GG <- c(rep(StoxLandingData$Landing$Gear[10], 20), rep(StoxLandingData$Landing$Gear[20], 25))
StoxLandingData$Landing$GG <- StoxLandingData$Landing$Gear

prep <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"))
expect_true("Area" %in% names(prep$Landings$AgeLengthCov))

#context("test-StoxAnalysisFunctions: PrepareRecaEstimate cellEffect")
prepCell <- RstoxFDA:::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area", "GG"), CellEffect = "All")
expect_equal(prepCell$AgeLength$info$interaction[prepCell$AgeLength$info$covariate=="Area"], 1)
expect_equal(prepCell$AgeLength$info$interaction[prepCell$AgeLength$info$covariate=="GG"], 1)

fpath <- RstoxFDA:::makeTempDirReca()
paramOut <- RstoxFDA:::ParameterizeRecaModels(prepCell, 10, 50, 1, fpath, Seed = 451)
expect_true("cell" %in% names(paramOut$FitProportionAtAge))

RstoxFDA:::removeTempDirReca(fpath)

#context("test-StoxAnalysisFunctions: RunRecaEstimate with random effect Area")
expect_warning(est <- RstoxFDA::RunRecaEstimate(prep, 10, 100, 0, Seed = 112))
expect_true("Area" %in% names(est$fit$ProportionAtAge$Intercept$cov))

