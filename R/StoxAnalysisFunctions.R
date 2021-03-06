

#' Issues stox warnings if any of the varnames has any missing values.
#' Terminate with warning
#' @noRd
warnMissingCovariateStoxBiotic <- function(varnames, StoxBioticData){
  error <- F
  for (e in varnames){
    # check cruise
    if (e %in% names(StoxBioticData$Cruise)){
      missing <- StoxBioticData$Cruise[is.na(StoxBioticData$Cruise[[e]])]
      if (nrow(missing) > 0){
        for (i in 1:nrow(missing)){
          error <- T
          stoxWarning(paste("Variable '",e,"' has missing values on the table 'Cruise' in 'StoxBioticData' for CruiseKey: ", missing$CruiseKey[i], sep=""))
        }
      }
    }
    # check station
    if (e %in% names(StoxBioticData$Station)){
      missing <- StoxBioticData$Station[is.na(StoxBioticData$Station[[e]])]
      if (nrow(missing) > 0){
        for (i in 1:nrow(missing)){
          error <- T
          stoxWarning(paste("Variable '",e,"' has missing values on the table 'Station' in 'StoxBioticData' for CruiseKey: ", missing$CruiseKey[i], " StationKey: ", missing$StationKey[i], sep=""))
        }
      }
    }
    
    # check haul
    if (e %in% names(StoxBioticData$Haul)){
      missing <- StoxBioticData$Haul[is.na(StoxBioticData$Haul[[e]])]
      if (nrow(missing) > 0){
        for (i in 1:nrow(missing)){
          error <- T
          stoxWarning(paste("Variable '",e,"' has missing values on the table 'Haul' in 'StoxBioticData' for CruiseKey: ", missing$CruiseKey[i], " StationKey: ", missing$StationKey[i], " HaulKey: ", missing$HaulKey[i], sep=""))
        }
      }
    }
    
    # check sample
    if (e %in% names(StoxBioticData$Sample)){
      missing <- StoxBioticData$Sample[is.na(StoxBioticData$Sample[[e]])]
      if (nrow(missing) > 0){
        for (i in 1:nrow(missing)){
          error <- T
          stoxWarning(paste("Variable '",e,"' has missing values on the table 'Sample' in 'StoxBioticData' for CruiseKey: ", missing$CruiseKey[i], " StationKey: ", missing$StationKey[i], " HaulKey: ", missing$HaulKey[i], " SampleKey: ", missing$SampleKey[i], sep=""))
        }
      }
    }
    
    # check individual
    if (e %in% names(StoxBioticData$Individual)){
      missing <- StoxBioticData$Individual[is.na(StoxBioticData$Individual[[e]])]
      if (nrow(missing) > 0){
        for (i in 1:nrow(missing)){
          error <- T
          stoxWarning(paste("Variable '",e,"' has missing values on the table 'Individual' in 'StoxBioticData' for CruiseKey: ", missing$CruiseKey[i], " StationKey: ", missing$StationKey[i], " HaulKey: ", missing$HaulKey[i], " SampleKey: ", missing$SampleKey[i], " IndividualKey: ", missing$IndividualKey[i], sep=""))
        }
      }
    }
  }
  
  if (error){
    stop("Cannot proceed with missing values for Reca-effects (covariates) or the variables for fish length or catch date.")
  }
}

#' Prepare data for Reca.
#' @description
#'  Performs data checks and data conversions,
#'  and stores some data-related parameters in preparation for running
#'  \code{\link[RstoxFDA]{ParameterizeRecaModels}}.
#' @details 
#'  Parameters are obtained for 'cells' using \code{\link[RstoxFDA]{ParameterizeRecaModels}} 
#'  and and applied to landings-data when predicting total catch-at-age using \code{\link[RstoxFDA]{RunRecaModels}}.
#'  
#'  'Cells' are defined by all effects / covariates that are specified for both 'StoxLandingData' and 'StoxBioticData' 
#'  (see parameters 'FixedEffects', 'RandomEffects', and 'CarEffect'). That is, any variable that are to be included in
#'  the cell definition must always exist (not containing NAs) 
#'  and be coded coherently in both 'StoxBioticData' and 'StoxLandingData'.
#'  
#'  Only effects that are part of the cell definition may be specified as 'Fixed effect' or 'CAR effect'.
#'  For fixed effect all values / levels of the effect must be sampled.
#'  For CAR effect, cell or neighbouring cells ('CarNeighbours') must be sampled
#'  
#'  Any variable in samples 'StoxBioticData' may be included as a random effect as long as it is always observed
#'  (not containing NAs).
#' 
#'  A random effect identifying haul / landing is always included in Reca. Do not add haul-identifiers as covariates.
#'  
#'  The option UseStockSplitting requires that a Variable 'otolithtype' is added to
#'  the table 'Individual' in StoxBioticData.
#'
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries
#'  and approriate columns appended for specifying cells. See details.
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns appended for specifying cells. See details.
#' @param FixedEffects
#'  optional, vector identifying column names that should be treated as fixed effects. See details
#' @param RandomEffects
#'  optional, vector identifying column names that should be treated as random effects. See details
#' @param UseCarEffect
#'  if TRUE, CAR-effect (conditional autoregressive effect) will be used.
#' @param CarEffect
#'  optional, character identifying the column name that should be treated as CAR-effect.
#'  Mandatory if 'UseCarEffect' is TRUE.
#' @param CarNeighbours
#'  \code{\link[RstoxFDA]{CarNeighbours}}, mandatory if 'carEffect' is given.
#'  Identifies which values of the carEffect are to be considered as neighbours.
#' @param CellEffect
#'  Configures the cell effect. If 'All', an interaction term will be added with all covariates that in the cell (whether they are fixed or random effects).
#'  Any CAR-effect is always included in the cell effect.
#' @param UseAgingError 
#'  If TRUE, aging, error parameters will be incorporated in the models.
#' @param AgeErrorMatrix
#'  \code{\link[RstoxFDA]{AgeErrorMatrix}}, optional, specifies the probabilities of misreading ages.
#'  mandatory if UseAgingError is TRUE.
#' @param UseStockSplitting
#'  If TRUE, models will be condigured to provide estimates for each of two stocks based on otholitt-typing. See \code{\link[RstoxFDA]{StockSplittingParameters}}.
#' @param UseStockSplittingError
#'  If TRUE, the model for error in stock classification (otolithtypes) will be applied.
#' @param StockSplittingParameters
#'  Parameters for stock splitting. Mandatory if 'UseStockSplitting' is TRUE. May be obtained with \code{\link[RstoxFDA]{DefineStockSplittingParameters}}.
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
#' @seealso 
#'  \code{\link[RstoxFDA]{ReportFdaSampling}} for inspecting the data availability for potential
#'  cell definitions.
#'  \code{\link[RstoxFDA]{ParameterizeRecaModels}}, 
#'  and \code{\link[RstoxFDA]{RunRecaModels}} for parameterising and running the Reca-analysis.
#'  \code{\link[RstoxBase]{DefineStratumPolygon}}, 
#'  \code{\link[RstoxFDA]{AddStratumStoxBiotic}},
#'  and \code{\link[RstoxFDA]{AddStratumStoxLanding}} for making 'Stratum' part of the cell definition,
#'  and #'  \code{\link[RstoxFDA]{DefineCarNeighbours}}, for maing 'Stratum' available as a CAR-effect
#'  \code{\link[RstoxFDA]{DefinePeriod}}, 
#'  \code{\link[RstoxFDA]{AddPeriodStoxBiotic}}, 
#'  and \code{\link[RstoxFDA]{AddPeriodStoxLanding}} for making 'Period' part of the cell definition.
#'  #'  \code{\link[RstoxData]{DefineTranslation}}, 
#'  \code{\link[RstoxFDA]{AddGearGroupStoxBiotic}}, 
#'  and \code{\link[RstoxFDA]{AddGearGroupStoxLanding}} for making 'GearGroup' part of the cell definition.
#'  \code{\link[RstoxFDA]{DefineAgeErrorMatrix}} for making an age-error matrix available for the Reca-analysis.
#'  \code{\link[RstoxFDA]{DefineStockSplittingParameters}} for configuring stock-splitting parameters, 
#'  and \code{\link[RstoxData]{AddToStoxBiotic}} for adding otolith-type to samples for stock-splitting analysis.
#' @export
PrepareRecaEstimate <- function(StoxBioticData, StoxLandingData, FixedEffects=character(), RandomEffects=character(), UseCarEffect=F, CarEffect=character(), CarNeighbours, UseAgingError=F, AgeErrorMatrix, UseStockSplitting=F, UseStockSplittingError=F, StockSplittingParameters, CellEffect=c("Off", "All"), MinAge=integer(), MaxAge=integer(), MaxLength=numeric(), LengthResolution=numeric(), HatchDay=integer()){
  #expose as parameter when implemented
  ContinousEffect<-NULL
  
  CellEffect <- match.arg(CellEffect, CellEffect)
  
  if (!UseStockSplitting){
    StockSplittingParameters <- NULL
  }
  
  if (!UseAgingError){
    AgeErrorMatrix <- NULL
  }
  if (UseAgingError){
    if (is.null(AgeErrorMatrix)){
      stop("'AgeErrorMatrix' must be provided when UseAgingError is TRUE.")
    }
    AgeErrorMatrix <- convertAgeErrorMatrixReca(AgeErrorMatrix)
  }
  if (!UseCarEffect){
    CarNeighbours <- NULL
    if (isGiven(CarEffect)){
      stop("UseCarEffect is False, while the parameter 'CarEffect' is given")
    }
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
  
  if (!isGiven(FixedEffects)){
    FixedEffects <- c()
  }
  if (!isGiven(RandomEffects)){
    RandomEffects <- c()
  }
  
  if (isGiven(CarEffect)){
    if (CarEffect %in% c(FixedEffects, RandomEffects)){
      stop(paste("The CAR effect", CarEffect, "is also specified as fixed effect or random effect"))
    }
  }
  if (isGiven(RandomEffects) & any(RandomEffects %in% FixedEffects)){
    dup <- RandomEffects[RandomEffects %in% FixedEffects]
    stop(paste("Some random effects are also specified as fixed effects:", paste(dup, collapse=",")))
  }
  
  if (isGiven(CellEffect) & CellEffect=="All"){
    effects <- c(RandomEffects, FixedEffects, CarEffect)
    interaction <- effects[effects %in% names(StoxLandingData$Landing)]
  }
  else if (isGiven(CellEffect) & CellEffect=="Off"){
    interaction <- c()
  }
  else{
    stop(paste("Opion", CellEffect, "is not supported for parameter 'CellEffect'"))
  }
  
  if (!isGiven(HatchDay)){
    HatchDay <- 1
  }
  
  if (!isGiven(CarEffect)){
    CarEffect <- c()
    convertedNeighbours <- NULL
    
  }
  else{
    if (!(CarEffect %in% names(StoxLandingData$Landing))){
      stop(paste("CarEffect", CarEffect, "must be found in 'StoxLandings'"))
    }
    convertedNeighbours <- convertCarNeighbours2recaWrap(CarNeighbours, unique(StoxLandingData$Landing[[CarEffect]]))
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
  # checks for NAs in covariates
  #
  effects <- c(FixedEffects, RandomEffects, CarEffect, "IndividualTotalLength", "DateTime")
  warnMissingCovariateStoxBiotic(effects, StoxBioticData)
  
  
  #
  # Set temporal resolution.
  # Landings compilation are only done to ensure coding consistency.
  # The temporal resolution doesnt matter.
  #
  quarter <- quarter(StoxLandingData$Landing$CatchDate)
  date <- NULL
  month <- NULL

  flatlandings <- StoxLandingData$Landing
  flatlandings$LiveWeightKG <- flatlandings$RoundWeight

  flatbiotic <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "Individual")
  flatbiotic <- flatbiotic[!is.na(flatbiotic$IndividualKey),]
  flatbiotic$catchId <- paste(flatbiotic$CruiseKey, flatbiotic$StationKey, flatbiotic$HaulKey, sep="_")
  flatbiotic$sampleId <- paste(flatbiotic$catchId, flatbiotic$SampleKey, sep="_")
  flatbiotic$Age <- flatbiotic$IndividualAge
  flatbiotic$Length <- flatbiotic$IndividualTotalLength
  flatbiotic$Weight <- flatbiotic$IndividualRoundWeight/1000
  flatbiotic$date <- as.Date(flatbiotic$DateTime)
  
  if (UseStockSplitting){
    if (!("otolithtype" %in% names(flatbiotic))){
      stop("The column 'otolithtype' must exist on the table 'Individual' of 'StoxBioticData' when the option 'UseStockSplitting' is used.")
    }
    flatbiotic$Otolithtype <- flatbiotic$otolithtype
  }
  
  nFish = NULL
  #
  # set the nFish table for any catches where several fractions have been sampled (delprøve)
  #
  if (length(unique(flatbiotic$catchId)) != length(unique(flatbiotic$sampleId))){
    allSamples <- unique(flatbiotic[,c("catchId", "sampleId")])
    stratifiedCatchIds <- allSamples$catchId[duplicated(allSamples$catchId)]
    nFish <- flatbiotic[flatbiotic$catchId %in% stratifiedCatchIds,c("sampleId", "CatchFractionCount", "HaulKey", "SampleKey", "CruiseKey", "StationKey")]
    nFish <- nFish[!duplicated(nFish$sampleId),]
    if (any(is.na(nFish$CatchFractionCount))){
      missing <- nFish[is.na(nFish$CatchFractionCount),]
      for (i in 1:nrow(missing)){
        HaulKey <- missing$HaulKey[i]
        SampleKey <- missing$SampleKey[i]
        CruiseKey<- missing$CruiseKey[i]
        StationKey<- missing$StationKey[i]
        stoxWarning(paste("'CatchFractionCount' missing from Sample", SampleKey, "from Haul", HaulKey, "which have several catch fractions sampled. Cruise:", CruiseKey, "Station:", StationKey))
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
                         neighbours=convertedNeighbours, 
                         nFish=nFish, 
                         ageError=AgeErrorMatrix, 
                         minAge=MinAge, 
                         maxAge=MaxAge, 
                         maxLength=MaxLength, 
                         lengthResolution=LengthResolution, 
                         date=date, 
                         month=month, 
                         quarter=quarter, 
                         hatchDay=HatchDay,
                         interaction = interaction)

  # stock splitting not handled by recawrap
  if (!is.null(StockSplittingParameters)){
    recaObject$CovariateMaps$StockSplitting <- list()
    recaObject$CovariateMaps$StockSplitting$StockNameCC <- StockSplittingParameters$StockNameCC
    recaObject$CovariateMaps$StockSplitting$StockNameS <- StockSplittingParameters$StockNameS
    recaObject$AgeLength$CCerrorList <- convertStockSplittingParameters2reca(StockSplittingParameters)
  }
  
  if (UseStockSplitting){
    recaObject$GlobalParameters$CC = TRUE
    recaObject$GlobalParameters$CCerror = UseStockSplittingError
  }
  
  res <- convertPrepReca2stox(recaObject)
  
  return(res)
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
#' @export
RunRecaEstimate <- function(RecaData, Nsamples=integer(), Burnin=integer(), Thin=integer(), Lgamodel=c("log-linear", "non-linear"), Resultdir=character(), Delta.age=numeric(), Seed=numeric(), Caa.burnin=numeric()){

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
#' @param ResultDirectory a directory where Reca may store temp-files \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}.
#' @param Thin controls how many iterations are run between each samples saved. Defaults to 0. This may be set to account for autocorrelation introduced by Metropolis-Hastings simulation. see documentation for \code{\link[Reca]{eca.estimate}}
#' @param Delta.age see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to 0.001.
#' @param Seed see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to random seed.
#' @return \code{\link[RstoxFDA]{RecaParameterData}} results from Reca Model Parameterization.
#' @seealso \code{\link[RstoxFDA]{PrepareRecaEstimate}} for model configuration, and data preparation for this function, and
#'  \code{\link[RstoxFDA]{RunRecaModels}} for obtaining predictions / estimates from the Reca-models.
#' @export
ParameterizeRecaModels <- function(RecaData, Nsamples=integer(), Burnin=integer(), Thin=integer(), ResultDirectory=character(), Lgamodel=c("log-linear", "non-linear"), Delta.age=numeric(), Seed=numeric()){
  
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
  
  StoxLandingData$Landing$LiveWeightKG <- StoxLandingData$Landing$RoundWeight
  quarter <- NULL
  month <- NULL
  date <- NULL
  if (TemporalResolution == "Quarter"){
    quarter <- quarter(StoxLandingData$Landing$CatchDate)
  }
  else if (TemporalResolution == "Month"){
    month <- month(StoxLandingData$Landing$CatchDate)
  }
  else if (TemporalResolution == "Day"){
    date <- StoxLandingData$Landing$CatchDate
  }
  else{
    stop(paste("Temporal resolution", TemporalResolution, "not supported"))
  }
  
  l <- getLandings(StoxLandingData$Landing, covariates = names(RecaParameterData$CovariateMaps$inLandings), covariateMaps = RecaParameterData$CovariateMaps$inLandings, month = month, quarter = quarter, date = date)
  return(l)
}

#' Run Reca Models
#' @description
#'  Runs prediction (catch-at-age estimate) for parameterized Reca models.
#' @details
#'  Parameters may be obtained with \code{\link[RstoxFDA]{ParameterizeRecaModels}}.
#'  If the function-paramter 'AggregationVariables' is provided, predictions will be provided for corresponding partitions of landings.
#'  The parameter 'StoxLandingData' may differ from the landings used in parameterisation (passed to \code{\link[RstoxFDA]{ParameterizeRecaModels}}), 
#'  as long as all not additional values / levels for the model covariates / effects are introduced.
#'  
#'  If The models are configured for stock-splitting analysis. The variable 'Stock' will be added to 'AggregationVariables' in the return value (\code{\link[RstoxFDA]{RecaCatchAtAge}})
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
#' @seealso \code{\link[RstoxFDA]{ParameterizeRecaModels}} for model parameterisation,
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtAge}}, 
#'  \code{\link[RstoxFDA]{ReportRecaLengthAtAge}}, 
#'  \code{\link[RstoxFDA]{ReportRecaWeightAtAge}} for compiling reports of predictions / estimates.
#' @export
RunRecaModels <- function(RecaParameterData, StoxLandingData, AggregationVariables=character(), TemporalResolution=c("Quarter", "Month"), Caa.burnin=numeric(), Seed=numeric()){
  
  #discard fit info and convert
  RecaParameterData$FitProportionAtAge <- NULL
  RecaParameterData$FitLengthGivenAge <- NULL
  RecaParameterData$FitWeightGivenLength <- NULL
  RecaParameterData <- convertStox2PrepReca(RecaParameterData)
  
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
    results <- ecaResult2Stox(results, RecaParameterData$CovariateMaps$StockSplitting)
    results$AggregationVariables <- data.table::data.table(AggregationVariables=AggregationVariables)
    if (RecaParameterData$GlobalParameters$CC){
      stopifnot("Stock" %in% names(results$CatchAtAge))
      if ("Stock" %in% names(results$AggregationVariables)){
        stop("Cannot add 'Stock' to aggregation variables, when it already exists as an aggregation variable")
      }
      results$AggregationVariables <- data.table::data.table(AggregationVariables=c(results$AggregationVariables$AggregationVariables, "Stock"))
    }
    return(results)
    
  }
  else{
    if (!all(AggregationVariables %in% names(StoxLandingData$Landing))){
      missing <- AggregationVariables[!(AggregationVariables %in% names(StoxLandingData$Landing))]
      stop(paste("Parameter 'AggregationVariables' contain some variables not found as columns in 'StoxLandingData':", paste(missing, collapse=",")))
    }
    
    result <- NULL
    frame <- unique(StoxLandingData$Landing[,AggregationVariables, with=F])
    frame$aggregationId <- 1:nrow(frame)
    
    l <- merge(StoxLandingData$Landing, frame, by=names(frame)[names(frame) %in% names(StoxLandingData$Landing)])
    for (id in frame$aggregationId){
      partition <- l[l$aggregationId==id,]
      Sl <- StoxLandingData
      Sl$Landing <- partition
      Landing <- getLandingsFromStoxLandings(RecaParameterData, Sl, TemporalResolution)
      RecaParameterData$Landings <- Landing
      partitionresults <- ecaResult2Stox(Reca::eca.predict(RecaParameterData$AgeLength, RecaParameterData$WeightLength, RecaParameterData$Landings, RecaParameterData$GlobalParameters), RecaParameterData$CovariateMaps$StockSplitting)
      
      for (a in AggregationVariables){
        stopifnot(length(unique(partition[[a]]))==1) # ensured by frame <- unique(StoxLandingData$Landing[,AggregationVariables, with=F])
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
    
    if (RecaParameterData$GlobalParameters$CC){
      
      stopifnot("Stock" %in% names(result$CatchAtAge))
      if ("Stock" %in% names(result$AggregationVariables)){
        stop("Cannot add 'Stock' to aggregation variables, when it already exists as an aggregation variable")
      }
      result$AggregationVariables <- data.table::data.table(AggregationVariables=c(result$AggregationVariables$AggregationVariables, "Stock"))
    }
    
    return(result)
  }
  
}
