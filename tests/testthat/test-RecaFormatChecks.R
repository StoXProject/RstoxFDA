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
ecaPrep$AgeLength$info["constant","nlev"] <- 2
expect_error(check_cov_vs_info(ecaPrep$AgeLength))
