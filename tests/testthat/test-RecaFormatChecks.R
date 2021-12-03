StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

ecaPrep <- convertStox2PrepReca(PrepareRecaEstimate(StoxBioticData, StoxLandingData))
checkLandings(ecaPrep$Landings)
check_cov_vs_info(ecaPrep$AgeLength)
check_cov_vs_info(ecaPrep$WeightLength)
check_data_matrix(ecaPrep$AgeLength)
check_data_matrix(ecaPrep$WeightLength)
check_covariates(ecaPrep$AgeLength)
check_covariates(ecaPrep$WeightLength)
checkAgeLength(ecaPrep$AgeLength)
checkWeightLength(ecaPrep$WeightLength)
checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov)
checkCovariateConsistency(ecaPrep$WeightLength, ecaPrep$Landings$WeightLengthCov)
check_landings_cov(ecaPrep$Landings$AgeLengthCov)
check_landings_cov(ecaPrep$Landings$WeightLengthCov)
checkGlobalParameters(ecaPrep$GlobalParameters, ecaPrep$AgeLength, ecaPrep$WeightLength)

#trigger failure
context("Test reca format checks")
errorPrep <- ecaPrep
errorPrep$AgeLength$info["constant","nlev"] <- 2
expect_error(check_cov_vs_info(errorPrep$AgeLength), "Not all values present for fixed covariate")

errorPrep <- ecaPrep
errorPrep$AgeLength$DataMatrix$samplingID <- NULL
expect_error(checkAgeLength(errorPrep$AgeLength), "column samplingID missing.")

errorPrep <- ecaPrep
errorPrep$AgeLength$DataMatrix$samplingID[1] <- NA
expect_error(checkAgeLength(errorPrep$AgeLength), "column samplingID has missing value.")

errorPrep <- ecaPrep
errorPrep$AgeLength$CovariateMatrix$constant <- NULL
expect_error(checkAgeLength(errorPrep$AgeLength), "No constant column provided in covariate matrix or info matrix")

errorPrep <- ecaPrep
errorPrep$WeightLength$info["constant","random"] <- 1
expect_error(checkWeightLength(errorPrep$WeightLength, errorPrep$Landings), "Constant covariate is not configured correctly")

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticData$Haul$Gear <- "11"

ecaPrep <- convertStox2PrepReca(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear")))
ecaPrep$AgeLength$CovariateMatrix <- ecaPrep$AgeLength$CovariateMatrix[,c("Gear", "constant")]
expect_error(checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov), "Covariates are not ordered consistently in model and landing")

ecaPrep <- convertStox2PrepReca(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear")))
ecaPrep$AgeLength$info <- ecaPrep$AgeLength$info[2:1,]
expect_error(checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov), "Covariates are not ordered consistently in info matrix and landing")


StoxBioticData$Station$Area <- "01"
StoxBioticData$Haul$Gear[1] <- "53"
StoxBioticData$Station$Area[1] <- "02"
StoxLandingData$Landing$Area <- "01"
StoxLandingData$Landing$Area[1] <- "02"
ecaPrep <- convertStox2PrepReca(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear", "Area")))
ecaPrep$Landings$AgeLengthCov$Area<-1
expect_error(checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov), "Not all sampled cells exist in landings")

ecaPrep <- convertStox2PrepReca(PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Area"), RandomEffects = c("Gear")))
errorPrep <- ecaPrep
errorPrep$AgeLength$info["Area", "nlev"] <- 3
expect_error(checkCovariateConsistency(errorPrep$AgeLength, errorPrep$Landings$AgeLengthCov), "Fixed effect Area does not have values for all corresponding landings")

errorPrep <- ecaPrep
errorPrep$Landings$AgeLengthCov$midseason[1] <- 0
expect_error(checkLandings(errorPrep$Landings), "midseason must be in")

errorPrep <- ecaPrep
errorPrep$Landings$AgeLengthCov$Area[1] <- NA
expect_error(checkLandings(errorPrep$Landings), "NAs in landings:  Area")

errorPrep <- ecaPrep
errorPrep$Landings$AgeLengthCov <- errorPrep$Landings$AgeLengthCov[1:2,]
expect_error(checkLandings(errorPrep$Landings), "number of rows in landings covariate matrices does not match")

errorPrep <- ecaPrep
errorPrep$Landings$LiveWeightKG <- errorPrep$Landings$LiveWeightKG[1:2]
expect_error(checkLandings(errorPrep$Landings), "length of weight vector does not match number of rows in covariate matrices in landings.")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$lengthresCM <- NULL
expect_error(checkGlobalParameters(errorPrep$GlobalParameters, errorPrep$AgeLength, errorPrep$WeightLength), "Length resolution not set")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$maxage <- 3
expect_error(checkGlobalParameters(errorPrep$GlobalParameters, errorPrep$AgeLength, errorPrep$WeightLength), "Parameter maxage 3 is smaller than maximal age in samples")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$minage <- 3
expect_error(checkGlobalParameters(errorPrep$GlobalParameters, errorPrep$AgeLength, errorPrep$WeightLength), "Parameter minage 3 is larger than minimal age in samples")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$maxlength <- 22
expect_error(checkGlobalParameters(errorPrep$GlobalParameters, errorPrep$AgeLength, errorPrep$WeightLength), "Parameter maxlength")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$age.error <- TRUE
expect_error(checkGlobalParameters(errorPrep$GlobalParameters, errorPrep$AgeLength, errorPrep$WeightLength), "Age error matrix not set, but age.error parameter set to TRUE.")
