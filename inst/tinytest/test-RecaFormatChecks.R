# ECA tests are not run for platforms where Reca is not available from StoX repositories.
env<-Sys.getenv()
if (!("_R_CHECK_FORCE_SUGGESTS_" %in% names(env)) || as.logical(env[["_R_CHECK_FORCE_SUGGESTS_"]])){

StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

ecaPrep <- RstoxFDA:::convertStox2PrepReca(RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData))
sb <- StoxBioticData
sb$Haul$Gear <- rep(StoxLandingData$Landing$Gear, 3)[1:nrow(sb$Haul)]
ecaPrepWcov <- RstoxFDA:::convertStox2PrepReca(RstoxFDA::PrepareRecaEstimate(sb, StoxLandingData, RandomEffects = c("Gear")))

RstoxFDA:::checkLandings(ecaPrep$Landings)
RstoxFDA:::check_cov_vs_info(ecaPrep$AgeLength)
RstoxFDA:::check_cov_vs_info(ecaPrep$WeightLength)
RstoxFDA:::check_data_matrix(ecaPrep$AgeLength)
RstoxFDA:::check_data_matrix(ecaPrep$WeightLength)
RstoxFDA:::check_covariates(ecaPrep$AgeLength)
RstoxFDA:::check_covariates(ecaPrep$WeightLength)
RstoxFDA:::checkAgeLength(ecaPrep$AgeLength)
RstoxFDA:::checkWeightLength(ecaPrep$WeightLength)
RstoxFDA:::checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov)
RstoxFDA:::checkCovariateConsistency(ecaPrep$WeightLength, ecaPrep$Landings$WeightLengthCov)
RstoxFDA:::check_landings_cov(ecaPrep$Landings$AgeLengthCov)
RstoxFDA:::check_landings_cov(ecaPrep$Landings$WeightLengthCov)
RstoxFDA:::checkGlobalParameters(ecaPrep$GlobalParameters, ecaPrep$AgeLength, ecaPrep$WeightLength)

#trigger failure
#context("Test reca format checks")
errorPrep <- ecaPrep
errorPrep$AgeLength$info["constant","nlev"] <- 2
expect_error(RstoxFDA:::check_cov_vs_info(errorPrep$AgeLength), "Not all values present for fixed covariate")

errorPrep <- ecaPrepWcov
rownames(errorPrep$AgeLength$info) <- c("constant", "Gir")
expect_error(RstoxFDA:::check_cov_vs_info(errorPrep$AgeLength), "Covariate Gear not in info matrix")

errorPrep <- ecaPrepWcov
errorPrep$AgeLength$CovariateMatrix$Gear[1] <- NA
expect_error(RstoxFDA:::check_cov_vs_info(errorPrep$AgeLength), "NAs for covariate Gear")

errorPrep <- ecaPrepWcov
errorPrep$AgeLength$CovariateMatrix$Gear[1] <- max(errorPrep$AgeLength$CovariateMatrix$Gear) +1
expect_error(RstoxFDA:::check_cov_vs_info(errorPrep$AgeLength), "Max value higher than nlev for covariate Gear")

errorPrep <- ecaPrepWcov
errorPrep$AgeLength$CovariateMatrix$Gear[1] <- 0
expect_error(RstoxFDA:::check_cov_vs_info(errorPrep$AgeLength), "Min value lower than 1 for covariate Gear")

errorPrep <- ecaPrep
errorPrep$AgeLength$DataMatrix$samplingID <- NULL
expect_error(RstoxFDA:::sanitizeRecaInput(AgeLength=errorPrep$AgeLength), "column samplingID missing.")

errorPrep <- ecaPrep
errorPrep$AgeLength$DataMatrix$samplingID[1] <- NA
expect_error(RstoxFDA:::sanitizeRecaInput(AgeLength=errorPrep$AgeLength), "column samplingID has missing value.")

errorPrep <- ecaPrep
errorPrep$AgeLength$CovariateMatrix$constant <- NULL
expect_error(RstoxFDA:::sanitizeRecaInput(AgeLength=errorPrep$AgeLength), "No constant column provided in covariate matrix or info matrix")

errorPrep <- ecaPrep
errorPrep$WeightLength$info["constant","random"] <- 1
expect_error(RstoxFDA:::sanitizeRecaInput(WeightLength=errorPrep$WeightLength, Landings=errorPrep$Landings), "Constant covariate is not configured correctly")

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)
StoxLandingFile <- system.file("testresources","StoxLandingData.rds", package="RstoxFDA")
StoxLandingData <- readRDS(StoxLandingFile)
StoxBioticData$Haul$Gear <- "11"

ecaPrep <- RstoxFDA:::convertStox2PrepReca(RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear")))
ecaPrep$AgeLength$CovariateMatrix <- ecaPrep$AgeLength$CovariateMatrix[,c("Gear", "constant")]
expect_error(RstoxFDA:::checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov), "Covariates are not ordered consistently in model and landing")

ecaPrep <- RstoxFDA:::convertStox2PrepReca(RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear")))
ecaPrep$AgeLength$info <- ecaPrep$AgeLength$info[2:1,]
expect_error(RstoxFDA:::checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov), "Covariates are not ordered consistently in info matrix and landing")


StoxBioticData$Station$Area <- "01"
StoxBioticData$Haul$Gear[1] <- "53"
StoxBioticData$Station$Area[1] <- "02"
StoxLandingData$Landing$Area <- "01"
StoxLandingData$Landing$Area[1] <- "02"
ecaPrep <- RstoxFDA:::convertStox2PrepReca(RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c(), RandomEffects = c("Gear", "Area")))
ecaPrep$Landings$AgeLengthCov$Area<-1
expect_error(RstoxFDA:::checkCovariateConsistency(ecaPrep$AgeLength, ecaPrep$Landings$AgeLengthCov), "Not all sampled cells exist in landings")

ecaPrep <- RstoxFDA:::convertStox2PrepReca(RstoxFDA::PrepareRecaEstimate(StoxBioticData, StoxLandingData, FixedEffects = c("Area"), RandomEffects = c("Gear")))
errorPrep <- ecaPrep
errorPrep$AgeLength$info["Area", "nlev"] <- 3
expect_error(RstoxFDA:::checkCovariateConsistency(errorPrep$AgeLength, errorPrep$Landings$AgeLengthCov), "Fixed effect Area does not have values for all corresponding landings")

errorPrep <- ecaPrep
errorPrep$Landings$AgeLengthCov$midseason[1] <- 0
expect_error(RstoxFDA:::sanitizeRecaInput(Landings=errorPrep$Landings), "midseason must be in")

errorPrep <- ecaPrep
errorPrep$Landings$AgeLengthCov$Area[1] <- NA
expect_error(RstoxFDA:::sanitizeRecaInput(Landings=errorPrep$Landings), "NAs in landings:  Area")

errorPrep <- ecaPrep
errorPrep$Landings$AgeLengthCov <- errorPrep$Landings$AgeLengthCov[1:2,]
expect_error(RstoxFDA:::sanitizeRecaInput(Landings=errorPrep$Landings), "number of rows in landings covariate matrices does not match")

errorPrep <- ecaPrep
errorPrep$Landings$LiveWeightKG <- errorPrep$Landings$LiveWeightKG[1:2]
expect_error(RstoxFDA:::sanitizeRecaInput(Landings=errorPrep$Landings), "length of weight vector does not match number of rows in covariate matrices in landings.")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$lengthresCM <- NA
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength), "Some required global parameters are NA: lengthresCM")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$lengthresCM <- NULL
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength), "Some required global parameters are missing: lengthresCM")


errorPrep <- ecaPrep
errorPrep$GlobalParameters$maxage <- 3
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength), "Parameter maxage 3 is smaller than maximal age in samples")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$minage <- 3
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength), "Parameter minage 3 is larger than minimal age in samples")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$maxlength <- 22
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength), "Parameter maxlength")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$age.error <- TRUE
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength), "Age error matrix not set, but age.error parameter set to TRUE.")

errorPrep <- ecaPrep
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength, stage="parameterize"), "Some required global parameters are missing: nSamples,thin,burnin,resultdir,fitfile,delta.age,lgamodel")
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength, stage="predict"), "Some required global parameters are missing: nSamples,thin,burnin,resultdir,fitfile,predictfile,delta.age,lgamodel,caa.burnin")

errorPrep <- ecaPrep
errorPrep$GlobalParameters$nSamples <- 10
errorPrep$GlobalParameters$burnin <- 10
errorPrep$GlobalParameters$thin <- NA
errorPrep$GlobalParameters$resultdir <- "dir"
errorPrep$GlobalParameters$fitfile <- "fit"
errorPrep$GlobalParameters$delta.age <- .01
errorPrep$GlobalParameters$lgamodel <- "non-linear"
expect_error(RstoxFDA:::sanitizeRecaInput(GlobalParameters=errorPrep$GlobalParameters, AgeLength=errorPrep$AgeLength, WeightLength=errorPrep$WeightLength, stage="parameterize"), "Some required global parameters are NA: thin")

}
