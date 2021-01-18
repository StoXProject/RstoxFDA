#' Prepare data for Reca.
#' @description
#'  StoX-function.
#'  Performs data checks and data conversions,
#'  and stores some data-related parameters in preparation for running
#'  \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#'  via \code{\link[RstoxFDA]{RunRecaEstimate}}.
#' @details 
#'  A covariate indentifying haul is always included in Reca. Do not add haul-identifiers as covariates.
#'
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries
#'  and approriate columns appended for identifying corresponding landings.
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns appended for identifying corresponding samples
#' @param fixedEffects
#'  optional, vector identifying column names that should be treated as fixed effects. Defaults to no fixed effects.
#' @param randomEffects
#'  optional, vector identifying column names that should be treated as random effects. Defaults to no random effects.
#' @param UseCarEffect
#'  logical indicating whether CAR-effect (conditional autoregressive effect) should be used.
#' @param carEffect
#'  optional, character identifying the column name that should be treated as CAR-effect.
#'  Mandatory if CAR-effect is TRUE
#'  (conditional autoregressive effect).
#' @param CarNeighbours
#'  \code{\link[RstoxFDA]{CarNeighbours}}, mandatory if 'carEffect' is given.
#'  Identifies which values of the carEffect are to be considered as neighbours.
#' @param UseAgingError 
#'  logical identifying if error-aging parameters should be incorporated in the model
#' @param AgeErrorMatrix
#'  \code{\link[RstoxFDA]{AgeErrorMatrix}}, optional, specifies the probabilities of misreading ages.
#'  mandatory if UseAgingError is TRUE.
#' @param minAge
#'  optional, must match dimensions of any 'AgeErrorMatrix'.
#'  If not provided it will be derived from data.
#' @param maxAge
#'  optional, must match dimensions of any 'AgeErrorMatrix'.
#'  If not provided it will be derived from data.
#' @param maxLength
#'  optional, maximal fish length in data in cm.
#'  If not provided it will be derived from data.
#' @param lengthResolution
#'  optional, resolution for length measurements in cm.
#'  If not provided modal value from data is used.
#' @param temporalResolution
#'  default "Quarter", code for temporal resolution in landings: "Month" or "Quarter".
#'  Regulates temporal resolution for calculating fractional ages of fish.
#'  Not to be confused with any temporal covariate.
#' @param hatchDay
#'  defaults to 1 representing Jan 1st.
#'  encoding the day of the year when fish is consider to transition from one age to the next.
#' @return \code{\link[RstoxFDA]{RecaData}} Data prepared for running Reca.
#' @export
PrepareRecaEstimate <- function(StoxBioticData, StoxLandingData, fixedEffects=NULL, randomEffects=NULL, UseCarEffect=F, carEffect=NULL, CarNeighbours=NULL, UseAgingError=F, AgeErrorMatrix=NULL, minAge=NULL, maxAge=NULL, maxLength=NULL, lengthResolution=NULL, temporalResolution=c("Quarter", "Month"), hatchDay=NULL){
  
  #expose as parameter when implemented
  ClassificationError=NULL
  stockSplitting=FALSE
  continousEffects<-NULL
  warning("Stox splitting, continous effect, and configuration of interaction is not implemented")
  
  temporalResolution <- match.arg(temporalResolution)
  
  if (!UseAgingError){
    AgeErrorMatrix = NULL
  }
  if (UseAgingError){
    if (is.null(AgeErrorMatrix)){
      stop("'AgeErrorMatrix' must be provided when UseAgingError is TRUE.")
    }
  }
  if (!UseCarEffect){
    CarNeighbours = NULL
    carEffect = c()
  }
  if (UseCarEffect){
    if (is.null(CarNeighbours)){
      stop("'CarNeighbours' must be provided when UseCarEffect is TRUE.")
    }
    if (is.null(carEffect) | length(carEffect) == 0){
      stop("'carEffect' must be provided when UseCarEffect is TRUE.")
    }

  }
  
  if (is.null(fixedEffects)){
    fixedEffects <- c()
  }
  if (is.null(randomEffects)){
    randomEffects <- c()
  }
  
  if (!isGiven(hatchDay)){
    hatchDay <- 1
  }
  else{
    hatchDay <- as.integer(hatchDay)
  }
  if (!isGiven(carEffect)){
    carEffect <- NULL
  }
  if (!isGiven(minAge)){
    minAge <- NULL
  }
  else{
    minAge <- as.integer(minAge)
  }
  if (!isGiven(maxAge)){
    maxAge <- NULL
  }
  else{
    maxAge <- as.integer(maxAge)
  }
  if (!isGiven(maxLength)){
    maxLength <- NULL
  }
  else{
    maxLength <- as.double(maxLength)
  }
  if (!isGiven(lengthResolution)){
    lengthResolution <- NULL
  }
  else{
    lengthResolution <- as.double(lengthResolution)
  }
  
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))

  temporalResolution <- match.arg(temporalResolution, temporalResolution)
  if (!(temporalResolution %in% c("Quarter", "Month", "Week"))){
    stop(paste("Temporal resolution", temporalResolution, "not supported"))
  }

  #
  # Set temporal resolution
  #
  quarter=NULL
  month=NULL

  if (temporalResolution == "Quarter"){
    quarter <- quarter(StoxLandingData$landings$CatchDate)
  }
  else if (temporalResolution == "Month"){
    month <- month(StoxLandingData$landings$CatchDate)
  }
  else{
    stop(paste("Temporal resolution", temporalResolution, "not supported"))
  }

  flatlandings <- StoxLandingData$landings
  flatlandings$LiveWeightKG <- flatlandings$RoundWeightKilogram

  flatbiotic <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "Individual")
  flatbiotic <- flatbiotic[!is.na(flatbiotic$IndividualKey),]
  flatbiotic$catchId <- paste(flatbiotic$CruiseKey, flatbiotic$StationKey, flatbiotic$HaulKey, sep="_")
  flatbiotic$sampleId <- paste(flatbiotic$catchId, flatbiotic$SampleKey, sep="_")
  flatbiotic$Age <- flatbiotic$IndividualAge
  flatbiotic$Length <- flatbiotic$IndividualTotalLength
  flatbiotic$Weight <- flatbiotic$IndividualRoundWeight
  flatbiotic$date <- as.Date(flatbiotic$DateTime)
  
  
  nFish = NULL
  #
  # set the nFish table for any catches where several fractions have been sampled (delprøve)
  #
  if (length(unique(flatbiotic$catchId)) != length(unique(flatbiotic$sampleId))){
    allSamples <- unique(flatbiotic[,c("catchId", "sampleId")])
    stratifiedCatchIds <- allSamples$catchId[duplicated(allSamples$catchId)]
    nFish <- flatbiotic[flatbiotic$catchId %in% stratifiedCatchIds,c("sampleId", "CatchFractionCount", "HaulKey", "SampleKey")]
    nFish <- nFish[!duplicated(nFish$sampleId),]
    if (any(is.na(nFish$CatchFractionCount))){
      missing <- nFish[is.na(nFish$CatchFractionCount),]
      for (i in 1:nrow(missing)){
        HaulKey <- missing$HaulKey[i]
        SampleKey <- missing$SampleKey[i]
        message(paste("'CatchFractionCount' missing from Sample", SampleKey, "from Haul", HaulKey, "which have several catch fractions sampled."))
      }
      stop("'StoxBioticData' have 'CatchFractionCount' missing from the 'Sample' table for Hauls with several fractions sampled.")
    }
    nFish <- nFish[,c("sampleId", "CatchFractionCount")]
    names(nFish) <- c("sampleId", "count")
  }
    
  
  recaObject <- prepRECA(flatbiotic, 
                         flatlandings, 
                         fixedEffects, 
                         randomEffects, 
                         carEffect, 
                         neighbours=CarNeighbours, 
                         nFish=nFish, 
                         ageError=AgeErrorMatrix, 
                         minAge=minAge, 
                         maxAge=maxAge, 
                         maxLength=maxLength, 
                         lengthResolution=lengthResolution, 
                         date=NULL, 
                         month=month, 
                         quarter=quarter, 
                         hatchDay=hatchDay)
  return(recaObject)
}


#' Run Reca
#' @description
#'  StoX-function.
#'  Runs \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}.
#' @details
#'  \code{\link[Reca]{eca.estimate}} performs Markov-chain Monte Carlo (MCMC) simulations to determine maximum likelihood of parameters for the given samples.
#'
#'  \code{\link[Reca]{eca.predict}} samples the posterior distributions of parameters estimated in \code{\link[Reca]{eca.estimate}},
#'  in order to obtain proportinos of catches and fish parameters.
#'  Using these parameters and the given total landings, predictions of distribution of catch-parameter distributions will be calculated.
#'
#'  If resultdir is NULL,  atemporary directory will be created for its purpose.
#'  This will be attempted removed after execution.
#'  If removal is not successful a warning will be issued which includes the path to the temporary directory.
#'
#' @param RecaData \code{\link[RstoxFDA]{RecaData}} as returned from \code{\link[RstoxFDA]{PrepareRecaEstimate}}
#' @param nSamples number of MCMC samples that will be made available for \code{\link[Reca]{eca.predict}}. See documentation for \code{\link[Reca]{eca.estimate}},
#' @param burnin number of MCMC samples run and discarded by \code{\link[Reca]{eca.estimate}} before any samples are saved. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param lgamodel The length age relationship to use for length-age fits (options: "log-linear", "non-linear": Schnute-Richards model). See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param resultdir a directory where Reca may store temp-files \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. . If not given a temporary directory will be created. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param thin controls how many iterations are run between each samples saved. This may be set to account for autocorrelation introduced by Metropolis-Hastings simulation. see documentation for \code{\link[Reca]{eca.estimate}}
#' @param delta.age see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to 0.001.
#' @param seed see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to random seed.
#' @param caa.burnin see documentation for \code{\link[Reca]{eca.predict}}. Defaults to 0.
#' @return \code{\link[RstoxFDA]{RecaResult}} results from Reca run.
#' @export
RunRecaEstimate <- function(RecaData, nSamples, burnin, thin, lgamodel=c("log-linear", "non-linear"), resultdir=NULL, delta.age=NULL, seed=NULL, caa.burnin=NULL){

  fitfile="fit"
  predictfile="pred"
  
  nSamples <- as.integer(nSamples)
  burnin <- as.integer(burnin)
  thin <- as.integer(thin)
  
  if (!isGiven(resultdir)){
    resultdir <- NULL
  }
  if (!isGiven(seed)){
    seed <- NULL
  }
  if (!isGiven(delta.age)){
    delta.age <- 0.001
  }
  else{
    as.double(delta.age)
  }
  if (!isGiven(caa.burnin)){
    caa.burnin <- 0
  }
  else{
    as.integer(caa.burnin)
  }
  
  stopifnot(is.RecaData(RecaData))

  lgamodel <- match.arg(lgamodel)

  recaResult <- runRECA(RecaData, nSamples = nSamples, burnin=burnin, lgamodel=lgamodel, fitfile=fitfile, predictfile = predictfile, resultdir = resultdir, thin=thin, delta.age = delta.age, seed = seed, caa.burnin = caa.burnin)

  return(recaResult)
}
