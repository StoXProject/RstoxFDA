
#' Prepare data for Reca.
#' @description
#'  Performs data checks and data conversions,
#'  and stores some data-related parameters in preparation for running
#'  \code{\link[Reca]{eca.estimate}}
#'  via \code{\link[RstoxFDA]{ParameterizeRecaModels}}.
#' @details 
#'  A covariate indentifying haul or landing is always included in Reca. Do not add haul-identifiers as covariates.
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
#' @param CellEffect
#'  If true, an interaction term will be added with all covariates that exist in landings (whether they are fixed or random effects)
#' @param UseAgingError 
#'  If true error-aging parameters will be incorporated in the model
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
#' @param HatchDay
#'  defaults to 1 representing Jan 1st.
#'  encoding the day of the year when fish is consider to transition from one age to the next.
#' @return \code{\link[RstoxFDA]{RecaData}} Data prepared for running Reca.
#' @export
PrepareRecaEstimate <- function(StoxBioticData, StoxLandingData, FixedEffects=NULL, RandomEffects=NULL, UseCarEffect=F, CarEffect=character(), CarNeighbours=NULL, UseAgingError=F, AgeErrorMatrix=NULL, CellEffect=F, MinAge=integer(), MaxAge=integer(), MaxLength=numeric(), LengthResolution=numeric(), HatchDay=integer()){
  
  #expose as parameter when implemented
  ClassificationError=NULL
  StockSplitting=FALSE
  ContinousEffect<-NULL
  warning("Stox splitting, continous effect.")
  
  
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
  
  interaction <- c()
  if (CellEffect){
    interaction <- c(RandomEffects, FixedEffects, CarEffect)
    interaction <- interaction[interaction %in% names(StoxLandingData$landings)]
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


  #
  # Set temporal resolution.
  # Landings compilation are only done to ensure coding consistency.
  # The temporal resolution doesnt matter.
  #
  quarter <- quarter(StoxLandingData$landings$CatchDate)
  data <- NULL
  month <- NULL

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
                         hatchDay=HatchDay,
                         interaction = interaction)
  return(convertPrepReca2stox(recaObject))
}


#' Run Reca. Being kept for testing purposes. Is replaced by \code{\link[RstoxFDA]{ParameterizeRecaModels}} and reporting functions
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
#'  If resultdir is NULL,  a temporary directory will be created for its purpose.
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
#' @noRd
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
  RecaData <- convertStox2PrepReca(RecaData)
  recaResult <- runRECA(RecaData, nSamples = Nsamples, burnin=Burnin, lgamodel=Lgamodel, fitfile=fitfile, predictfile = predictfile, resultdir = Resultdir, thin=Thin, delta.age = Delta.age, seed = Seed, caa.burnin = Caa.burnin)

  return(recaResult)
}

#' Parameterize Reca.
#' @description
#'  Runs estimation of parameters for Reca models. Invokes \code{\link[Reca]{eca.estimate}}.
#' @details
#'  \code{\link[Reca]{eca.estimate}} performs Markov-chain Monte Carlo (MCMC) simulations to determine maximum likelihood of parameters for the given samples.
#'  This is computationally intensive and run time may be noticable. For a given model configuration running time is mainly determined by the parameters 'Nsample', 'Burnin' and 'Thin'.
#'
#' @section temporary files:
#'  Various report functions may use output of this function with the function \code{\link[Reca]{eca.predict}} which samples the posterior distributions of parameters.
#'  Communication between \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}} is managed by writing and reading files, 
#'  and a directory for storing intermediate calculations must be provided with the parameter 'ResultDirectory'.
#'
#' @param RecaData \code{\link[RstoxFDA]{RecaData}} as returned from \code{\link[RstoxFDA]{PrepareRecaEstimate}}
#' @param Nsamples number of MCMC samples that will be made available for \code{\link[Reca]{eca.predict}}. See documentation for \code{\link[Reca]{eca.estimate}},
#' @param Burnin number of MCMC samples run and discarded by \code{\link[Reca]{eca.estimate}} before any samples are saved. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param Lgamodel The length age relationship to use for length-age fits (options: "log-linear", "non-linear": Schnute-Richards model). See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param Resultdir a directory where Reca may store temp-files \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}.
#' @param Thin controls how many iterations are run between each samples saved. Defaults to 0. This may be set to account for autocorrelation introduced by Metropolis-Hastings simulation. see documentation for \code{\link[Reca]{eca.estimate}}
#' @param Delta.age see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to 0.001.
#' @param Seed see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to random seed.
#' @return \code{\link[RstoxFDA]{RecaParameterData}} results from Reca Model Parameterization.
#' @export
ParameterizeRecaModels <- function(RecaData, Nsamples=integer(), Burnin=integer(), Thin=integer(), ResultDirectory=NULL, Lgamodel=c("log-linear", "non-linear"), Delta.age=numeric(), Seed=numeric()){
  
  RecaData <- convertStox2PrepReca(RecaData)
  
  Lgamodel <- match.arg(Lgamodel, Lgamodel)
  if (!isGiven(Lgamodel)){
    stop("Parameter 'Lgamodel' must be provided.")
  }
  
  if (!isGiven(Seed)){
    Seed <- sample.int(.Machine$integer.max, 1)
  }
  if (!isGiven(ResultDirectory)){
    stop("Parameter 'ResultDirectory' must be provided.")
  }
  
  ResultDirectory <- path.expand(ResultDirectory)
  
  if (!file.exists(ResultDirectory)){
    stop(paste("Could not find the directory", ResultDirectory))
  }
  
  if (grepl(" ", ResultDirectory)) {
    stop(paste(
      "Please make temporary directory for Reca contain no spaces",
      "(current:",
      ResultDirectory,
      ") ."
    ))
  }
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
  if (!isGiven(Delta.age)){
    Delta.age <- 0.001
  }

  stopifnot(is.RecaData(RecaData))
  
  GlobalParameters <- RecaData$GlobalParameters
  GlobalParameters$nSamples <- Nsamples
  GlobalParameters$burnin <- Burnin
  GlobalParameters$lgamodel <- Lgamodel
  GlobalParameters$resultdir <- ResultDirectory
  GlobalParameters$fitfile <- fitfile
  GlobalParameters$predictfile <- predictfile
  GlobalParameters$thin <- Thin
  GlobalParameters$delta.age <- Delta.age
  GlobalParameters$seed <- Seed
  
  RecaData$GlobalParameters <- GlobalParameters
  
  RecaData <- checkEcaObj(RecaData)
  
  fit <- Reca::eca.estimate(RecaData$AgeLength, RecaData$WeightLength, RecaData$Landings, RecaData$GlobalParameters)

  out <- recaFit2Stox(fit, RecaData$CovariateMaps)
  
  RecaData <- convertPrepReca2stox(RecaData)
  
  for (n in names(RecaData)){
    out[[n]] <- RecaData[[n]]
  }
  
  return(out)
}


#' @noRd
getLandingsFromStoxLandings <- function(RecaParameterData, StoxLandingData, TemporalResolution){
  
  StoxLandingData$landings$LiveWeightKG <- StoxLandingData$landings$RoundWeight
  quarter <- NULL
  month <- NULL
  date <- NULL
  if (TemporalResolution == "Quarter"){
    quarter <- quarter(StoxLandingData$landings$CatchDate)
  }
  else if (TemporalResolution == "Month"){
    month <- month(StoxLandingData$landings$CatchDate)
  }
  else if (TemporalResolution == "Day"){
    date <- StoxLandingData$landings$CatchDate
  }
  else{
    stop(paste("Temporal resolution", TemporalResolution, "not supported"))
  }
  
  l <- getLandings(StoxLandingData$landings, covariates = names(RecaParameterData$CovariateMaps$inLandings), covariateMaps = RecaParameterData$CovariateMaps$inLandings, month = month, quarter = quarter, date = date)
  return(l)
}

#' Run Reca Models
#' @description
#'  Runs prediction (catch-at-age estimate) for parameterized Reca models.
#' @details
#'  Parameters may be obtained with \code{\link[RstoxFDA]{ParameterizeRecaModels}}.
#'  If the function-paramter 'AggregationVariables' is provided, predictions will be provided for corresponding partitions of landings.
#'  The parameter 'StoxLandingData' may differ from the landings used in parameterisation, 
#'  as long as all not additional level for the model covariates are introduced.
#' @param RecaParameterData Parameters for Reca models.
#' @param StoxLandingData Landings data (\code{\link[RstoxData]{StoxLandingData}}).
#' @param AggregationVariables character vector identifying columns in 'StoxLandingData' that results should be provided for.
#' @param TemporalResolution
#'  default "Quarter", code for temporal resolution in landings: "Month" or "Quarter".
#'  Regulates temporal resolution for calculating fractional ages of fish.
#'  Not to be confused with any temporal covariate.
#' @param Caa.burnin see documentation for \code{\link[Reca]{eca.predict}}. Defaults to 0.
#' @param Seed see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to seed stored in 'RecaParameterData'.
#' @return \code{\link[RstoxFDA]{RecaCatchAtAge}}
#' @export
RunRecaModels <- function(RecaParameterData, StoxLandingData, AggregationVariables=character(), TemporalResolution=c("Quarter", "Month"), Caa.burnin=numeric(), Seed=numeric()){
  TemporalResolution <- match.arg(TemporalResolution, TemporalResolution)
  if (!isGiven(TemporalResolution)){
    stop("The parameter 'TemporalResolution' must be provided.")
  }
  if (!isGiven(Caa.burnin)){
    Caa.burnin <- 0
  }
  if (!isGiven(Seed)){
    RecaParameterData$GlobalParameters$Seed <- sample.int(.Machine$integer.max, 1)
  }
  
  RecaParameterData$GlobalParameters$caa.burnin <- Caa.burnin
  
  if (!isGiven(AggregationVariables)){
    landings <- getLandingsFromStoxLandings(RecaParameterData, StoxLandingData, TemporalResolution)
    RecaParameterData$Landings <- landings
    results <- Reca::eca.predict(RecaParameterData$AgeLength, RecaParameterData$WeightLength, RecaParameterData$Landings, RecaParameterData$GlobalParameters)
    results <- ecaResult2Stox(results)
    results$AggregationVariables <- data.table::data.table(AggregationVariables=AggregationVariables)
    return(results)
    
  }
  else{
    if (!all(AggregationVariables %in% names(StoxLandingData$landings))){
      missing <- AggregationVariables[!(AggregationVariables %in% names(StoxLandingData$landings))]
      stop(paste("Parameter 'AggregationVariables' contain some variables not found as columns in 'StoxLandingData':", paste(missing, collapse=",")))
    }
    
    result <- NULL
    frame <- unique(StoxLandingData$landings[,AggregationVariables, with=F])
    frame$aggregationId <- 1:nrow(frame)
    
    l <- merge(StoxLandingData$landings, frame, by=names(frame)[names(frame) %in% names(StoxLandingData$landings)])
    for (id in frame$aggregationId){
      partition <- l[l$aggregationId==id,]
      Sl <- StoxLandingData
      Sl$landings <- partition
      landings <- getLandingsFromStoxLandings(RecaParameterData, Sl, TemporalResolution)
      RecaParameterData$Landings <- landings
      partitionresults <- ecaResult2Stox(Reca::eca.predict(RecaParameterData$AgeLength, RecaParameterData$WeightLength, RecaParameterData$Landings, RecaParameterData$GlobalParameters))
      
      for (a in AggregationVariables){
        stopifnot(length(unique(partition[[a]]))==1) # ensured by frame <- unique(StoxLandingData$landings[,AggregationVariables, with=F])
        partitionresults$CatchAtAge[[a]] <- partition[[a]][1]
        partitionresults$MeanLength[[a]] <- partition[[a]][1]
        partitionresults$MeanWeight[[a]] <- partition[[a]][1]
      }
      
      if (is.null(result)){
        result <- partitionresults
      }
      else{
        result$CatchAtAge <- rbind(result$CatchAtAge, partitionresults$CatchAtAge)
        result$MeanLength <- rbind(result$MeanLength, partitionresults$MeanLength)
        result$MeanWeight <- rbind(result$MeanWeight, partitionresults$MeanWeight)
      }
    }
    result$AggregationVariables <- data.table::data.table(AggregationVariables=AggregationVariables)
    return(result)
  }
  
}
