context("PrepRecaEstimate: AgerrorMatrix")
ageerorfile <- system.file("testresources","AgeErrorHirstEtAl2012.txt", package="RstoxFDA")
ageerror <- DefineAgeErrorMatrix(FileName = ageerorfile)
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

StoxBioticData$Station$Area <- StoxLandingData$Landing$Area[sample(20,45,T)]
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Area"), RandomEffects = c("Area")), "Some random effects are also specified as fixed effects: Area")
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"), CarEffect = "Area"), "UseCarEffect is False, while the parameter 'CarEffect' is given")
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area"), CarEffect = "Area", UseCarEffect = T, CarNeighbours = list()), "The CAR effect Area is also specified as fixed effect or random effect")

#check CAR value cehcks
carfile <- system.file("testresources","mainarea_neighbour_correct_codes.txt", package="RstoxFDA")
car <- DefineCarNeighbours(NULL, FileName = carfile)
car$Neighbours[9] <- paste(car$Neighbours[9], "30", sep=",")
car$Neighbours[29] <- paste(car$Neighbours[29], "08", sep=",")
prepCar <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), CarEffect = "Area", UseCarEffect = T, CarNeighbours = car)

context("PrepareRecaEstimate: test run with car")
fpath <- makeTempDirReca()
paramOut <- ParameterizeRecaModels(prepCar, 10, 50, 1, fpath)
results <- RunRecaModels(paramOut, StoxLandingData)
removeTempDirReca(fpath)
expect_true(all(!is.na(paramOut$FitLengthGivenAge$Area$car_Intercept)))

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

context("test-StoxAnalysisFunctions: RunRecaModels with AggregationVariables")
results <- RunRecaModels(paramOut, StoxLandingData, AggregationVariables = c("Gear"))

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
paramOut <- ParameterizeRecaModels(prep, 10, 50, 1, fpath)
results <- RunRecaModels(paramOut, StoxLandingData)
expect_true("Gear" %in% names(paramOut$Landings$AgeLengthCov))
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(is.RecaCatchAtAge(results))

paramOut <- ParameterizeRecaModels(prep, 10, 50, 1, fpath)
results <- RunRecaModels(paramOut, StoxLandingData, AggregationVariables = "Gear")
expect_true("Gear" %in% names(paramOut$Landings$AgeLengthCov))
expect_true("Age" %in% names(results$CatchAtAge))
expect_true(is.RecaCatchAtAge(results))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate missing arguments")
expect_error(ParameterizeRecaModels(prep, 10, 50, 1, fpath, Lgamodel = NULL), "Parameter 'Lgamodel' must be provided.")

removeTempDirReca(fpath)

context("test-StoxAnalysisFunctions: PrepareRecaEstimate simple case")
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)

prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c())
expect_equal(length(prep$CovariateMaps$randomEffects$AgeLength$catchSample), length(unique(StoxBioticData$Individual$HaulKey)))


prep <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c(), MinAge=1, MaxAge=30)

context("test-StoxAnalysisFunctions: RunRecaEstimate simple case")
result <- RunRecaEstimate(prep, 10, 50, Thin=1)
expect_true(all(c("input", "fit", "prediction", "covariateMaps") %in% names(result)))
expect_equal(dim(result$prediction$TotalCount)[3], 10)


context("test-StoxAnalysisFunctions: PrepareRecaEstimate, missing sample dates")
StoxBioticData$Station$DateTime[1] <- NA
expect_error(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c()))

context("test-StoxAnalysisFunctions: PrepareRecaEstimate, stratified samples (nFish), missing CatchFractionCount")
StoxBioticDataDelp <- readRDS(system.file("testresources","StoxBioticDelpr.rds", package="RstoxFDA"))
expect_error(PrepareRecaEstimate(StoxBioticDataDelp, StoxLandingData, FixedEffects = c(), RandomEffects = c()))

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
prepCell <- PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Area", "GG"), CellEffect = T)
expect_equal(prepCell$AgeLength$info$interaction[prepCell$AgeLength$info$covariate=="Area"], 1)
expect_equal(prepCell$AgeLength$info$interaction[prepCell$AgeLength$info$covariate=="GG"], 1)

fpath <- makeTempDirReca()
paramOut <- ParameterizeRecaModels(prepCell, 10, 50, 1, fpath)
expect_true("cell" %in% names(paramOut$FitProportionAtAge))
removeTempDirReca(fpath)


context("test-StoxAnalysisFunctions: RunRecaEstimate with random effect Area")
est <- RunRecaEstimate(prep, 10, 100, 0)
expect_true("Area" %in% names(est$fit$ProportionAtAge$Intercept$cov))

context("RunRecaEstimate not providing burnin")
expect_error(RunRecaEstimate(prep, 10))
