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
