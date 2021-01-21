
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
#' @param FixedEffects
#'  optional, vector identifying column names that should be treated as fixed effects. Defaults to no fixed effects.
#' @param RandomEffects
#'  optional, vector identifying column names that should be treated as random effects. Defaults to no random effects.
#' @param UseCarEffect
#'  logical indicating whether CAR-effect (conditional autoregressive effect) should be used.
#' @param CarEffect
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
#' @param MinAge
#'  optional, must match dimensions of any 'AgeErrorMatrix'.
#'  If not provided it will be derived from data.
#' @param MaxAge
#'  optional, must match dimensions of any 'AgeErrorMatrix'.
#'  If not provided it will be derived from data.
#' @param MaxLength
#'  optional, maximal fish length in data in cm.
#'  If not provided it will be derived from data.
#' @param LengthResolution
#'  optional, resolution for length measurements in cm.
#'  If not provided modal value from data is used.
#' @param TemporalResolution
#'  default "Quarter", code for temporal resolution in landings: "Month" or "Quarter".
#'  Regulates temporal resolution for calculating fractional ages of fish.
#'  Not to be confused with any temporal covariate.
#' @param HatchDay
#'  defaults to 1 representing Jan 1st.
#'  encoding the day of the year when fish is consider to transition from one age to the next.
#' @return \code{\link[RstoxFDA]{RecaData}} Data prepared for running Reca.
#' @export
PrepareRecaEstimate <- function(StoxBioticData, StoxLandingData, FixedEffects=NULL, RandomEffects=NULL, UseCarEffect=F, CarEffect=NULL, CarNeighbours=NULL, UseAgingError=F, AgeErrorMatrix=NULL, MinAge=integer(), MaxAge=integer(), MaxLength=numeric(), LengthResolution=numeric(), TemporalResolution=c("Quarter", "Month"), HatchDay=integer()){
  
  #expose as parameter when implemented
  ClassificationError=NULL
  StockSplitting=FALSE
  ContinousEffect<-NULL
  warning("Stox splitting, continous effect, and configuration of interaction is not implemented. Renaming and translation of catchSample effect and naming/translation of cell-levels")
  
  
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
    CarEffect = c()
  }
  if (UseCarEffect){
    if (is.null(CarNeighbours)){
      stop("'CarNeighbours' must be provided when UseCarEffect is TRUE.")
    }
    if (is.null(CarEffect) | length(CarEffect) == 0){
      stop("'CarEffect' must be provided when UseCarEffect is TRUE.")
    }

  }
  
  if (is.null(FixedEffects)){
    FixedEffects <- c()
  }
  if (is.null(RandomEffects)){
    RandomEffects <- c()
  }
  
  if (!isGiven(HatchDay)){
    HatchDay <- 1
  }
  if (!isGiven(CarEffect)){
    CarEffect <- c()
  }
  if (!isGiven(MinAge)){
    MinAge <- NULL
  }
  if (!isGiven(MaxAge)){
    MaxAge <- NULL
  }
  if (!isGiven(MaxLength)){
    MaxLength <- NULL
  }
  if (!isGiven(LengthResolution)){
    LengthResolution <- NULL
  }

  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))

  TemporalResolution <- match.arg(TemporalResolution, TemporalResolution)
  if (!(TemporalResolution %in% c("Quarter", "Month", "Week"))){
    stop(paste("Temporal resolution", TemporalResolution, "not supported"))
  }

  #
  # Set temporal resolution
  #
  quarter=NULL
  month=NULL

  if (TemporalResolution == "Quarter"){
    quarter <- quarter(StoxLandingData$landings$CatchDate)
  }
  else if (TemporalResolution == "Month"){
    month <- month(StoxLandingData$landings$CatchDate)
  }
  else{
    stop(paste("Temporal resolution", TemporalResolution, "not supported"))
  }

  flatlandings <- StoxLandingData$landings
  flatlandings$LiveWeightKG <- flatlandings$RoundWeight

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
                         FixedEffects, 
                         RandomEffects, 
                         CarEffect, 
                         neighbours=CarNeighbours, 
                         nFish=nFish, 
                         ageError=AgeErrorMatrix, 
                         minAge=MinAge, 
                         maxAge=MaxAge, 
                         maxLength=MaxLength, 
                         lengthResolution=LengthResolution, 
                         date=NULL, 
                         month=month, 
                         quarter=quarter, 
                         hatchDay=HatchDay)
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
#' @param Nsamples number of MCMC samples that will be made available for \code{\link[Reca]{eca.predict}}. See documentation for \code{\link[Reca]{eca.estimate}},
#' @param Burnin number of MCMC samples run and discarded by \code{\link[Reca]{eca.estimate}} before any samples are saved. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param Lgamodel The length age relationship to use for length-age fits (options: "log-linear", "non-linear": Schnute-Richards model). See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param Resultdir a directory where Reca may store temp-files \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. . If not given a temporary directory will be created. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param Thin controls how many iterations are run between each samples saved. Defaults to 0. This may be set to account for autocorrelation introduced by Metropolis-Hastings simulation. see documentation for \code{\link[Reca]{eca.estimate}}
#' @param Delta.age see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to 0.001.
#' @param Seed see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to random seed.
#' @param Caa.burnin see documentation for \code{\link[Reca]{eca.predict}}. Defaults to 0.
#' @return \code{\link[RstoxFDA]{RecaResult}} results from Reca run.
#' @export
RunRecaEstimate <- function(RecaData, Nsamples=integer(), Burnin=integer(), Thin=integer(), Lgamodel=c("log-linear", "non-linear"), Resultdir=NULL, Delta.age=numeric(), Seed=numeric(), Caa.burnin=numeric()){

  fitfile="fit"
  predictfile="pred"
  
  if (!isGiven(Nsamples)){
    stop("Parameter 'Nsamples' must be provided.")
  }
  if (!isGiven(Burnin)){
    stop("Parameter 'Burnin' must be provided.")
  }
  if (!isGiven(Thin)){
    Thin <- 0
  }
  if (!isGiven(Resultdir)){
    Resultdir <- NULL
  }
  if (!isGiven(Seed)){
    Seed <- NULL
  }
  if (!isGiven(Delta.age)){
    Delta.age <- 0.001
  }
  if (!isGiven(Caa.burnin)){
    Caa.burnin <- 0
  }

  stopifnot(is.RecaData(RecaData))

  Lgamodel <- match.arg(Lgamodel, Lgamodel)

  recaResult <- runRECA(RecaData, nSamples = Nsamples, burnin=Burnin, lgamodel=Lgamodel, fitfile=fitfile, predictfile = predictfile, resultdir = Resultdir, thin=Thin, delta.age = Delta.age, seed = Seed, caa.burnin = Caa.burnin)

  return(recaResult)
}
