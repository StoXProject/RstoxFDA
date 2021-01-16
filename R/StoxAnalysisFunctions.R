#' Prepare data for Reca.
#' @description
#'  StoX-function.
#'  Performs data checks and data conversions,
#'  and stores some data-related parameters in preparation for running
#'  \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#'  via \code{\link[RstoxFDA]{RunRecaEstimate}}.
#'
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries
#'  and approriate columns appended for identifying corresponding landings.
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns appended for identifying corresponding samples
#' @param fixedEffects
#'  character() vector identifying column names that should be treated as fixed effects
#' @param randomEffects
#'  character() vector identifying column names that should be treated as random effects
#' @param carEffect
#'  character(), optional, identifying the column name that should be treated as CAR-effect
#'  (conditional autoregressive effect)
#' @param CarNeighbours
#'  \code{\link[RstoxFDA]{CarNeighbours}}, mandatory if 'carEffect' is given.
#'  Identifies which values of the carEffect are to be considered as neighbours.
#' @param AgeErrorMatrix
#'  \code{\link[RstoxFDA]{AgeErrorMatrix}}, optional, specifies the probabilities of misreading ages.
#'  If not provided age reading errors will not be modelled.
#' @param stockSplitting
#'  default FALSE, whether to run estimates for separate stocks in the data (coastal cod-analysis)
#' @param ClassificationError
#'  \code{\link[RstoxFDA]{ClassificationError}}, optional,
#'  specifies the probability of misclassifying stock for an individual Used in conjunction with 'stockSplitting'. If not provided classification errors will not be modelled.
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
PrepareRecaEstimate <- function(StoxBioticData, StoxLandingData, fixedEffects, randomEffects, carEffect=NULL, CarNeighbours=NULL, AgeErrorMatrix=NULL, stockSplitting=FALSE, ClassificationError=NULL, minAge=NULL, maxAge=NULL, maxLength=NULL, lengthResolution=NULL, temporalResolution=c("Quarter", "Month"), hatchDay=NULL){
  
  #expose as parameter when implemented
  continousEffects<-NULL
  
  temporalResolution <- match.arg(temporalResolution)
  
  if (!isGiven(hatchDay)){
    hatchDay <- 1
  }
  if (!isGiven(carEffect)){
    carEffect <- NULL
  }
  if (!isGiven(minAge)){
    minAge <- NULL
  }
  if (!isGiven(maxAge)){
    maxAge <- NULL
  }
  if (!isGiven(maxLength)){
    maxLength <- NULL
  }
  if (!isGiven(lengthResolution)){
    lengthResolution <- NULL
  }
  
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))

  temporalResolution <- match.arg(temporalResolution, temporalResolution)
  if (!(temporalResolution %in% c("Quarter", "Month", "Week"))){
    stop(paste("Temporal resolution", temporalResolution, "not supported"))
  }

  if (length(continousEffects) != 0){
    stop("Data preparation for continous effect is not yet implemented.")
  }

  if (stockSplitting){
    stop("Data preparation for stock splitting is not yet implemented.")
  }

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

  warning("Get nFish for each sample with delprove.")
  nFish = NULL
  
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
  
  if (!isGiven(resultdir)){
    resultdir <- NULL
  }
  if (!isGiven(seed)){
    seed <- NULL
  }
  if (!isGiven(delta.age)){
    delta.age <- 0.001
  }
  if (!isGiven(caa.burnin)){
    caa.burnin <- 0
  }
  
  stopifnot(is.RecaData(RecaData))

  lgamodel <- match.arg(lgamodel)

  recaResult <- runRECA(RecaData, nSamples = nSamples, burnin=burnin, lgamodel=lgamodel, fitfile=fitfile, predictfile = predictfile, resultdir = resultdir, thin=thin, delta.age = delta.age, seed = seed, caa.burnin = caa.burnin)

  return(recaResult)
}
