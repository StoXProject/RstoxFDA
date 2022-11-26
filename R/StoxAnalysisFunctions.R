

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

#' Issues stox warnings if any of the samples are taken form cells not in landings.
#' Terminate with warning
#' @noRd
warnMissingLandings <- function(StoxBiotic, StoxLanding, effects){
  
  if (!isGiven(effects)){
    return()
  }
  flatbiotic <- RstoxData::MergeStoxBiotic(StoxBiotic, TargetTable = "Individual")
  effects <- effects[effects %in% names(StoxLanding$Landing) & effects %in% names(flatbiotic)]
  if (!isGiven(effects)){
    return()
  }
  
  #reconstruct flatbiotic as deep as necessary to cover all effects
  levelEff <- effects
  flatbiotic <- StoxBiotic$Cruise
  levelEff <- levelEff[!(levelEff %in% names(StoxBiotic$Cruise))]
  if (length(levelEff) > 0){
    flatbiotic <- merge(flatbiotic, StoxBiotic$Station, by=names(flatbiotic)[names(flatbiotic) %in% names(StoxBiotic$Station)])
    levelEff <- levelEff[!(levelEff %in% names(StoxBiotic$Station))]
  }
  
  if (length(levelEff) > 0){
    flatbiotic <- merge(flatbiotic, StoxBiotic$Haul, by=names(flatbiotic)[names(flatbiotic) %in% names(StoxBiotic$Haul)])
    levelEff <- levelEff[!(levelEff %in% names(StoxBiotic$Haul))]
  }
  
  if (length(levelEff) > 0){
    flatbiotic <- merge(flatbiotic, StoxBiotic$SpeciesCategory, by=names(flatbiotic)[names(flatbiotic) %in% names(StoxBiotic$SpeciesCategory)])
    levelEff <- levelEff[!(levelEff %in% names(StoxBiotic$SpeciesCategory))]
  }
  
  if (length(levelEff) > 0){
    flatbiotic <- merge(flatbiotic, StoxBiotic$Sample, by=names(flatbiotic)[names(flatbiotic) %in% names(StoxBiotic$Sample)])
    levelEff <- levelEff[!(levelEff %in% names(StoxBiotic$Sample))]
  }
  
  if (length(levelEff) > 0){
    flatbiotic <- merge(flatbiotic, StoxBiotic$Individual, by=names(flatbiotic)[names(flatbiotic) %in% names(StoxBiotic$Individual)])
  }

  flatbiotic$effectId <- ""
  StoxLanding$Landing$effectId <- ""
  
  for (e in effects){
    flatbiotic$effectId <- paste(flatbiotic$effectId, flatbiotic[[e]])  
    StoxLanding$Landing$effectId <- paste(StoxLanding$Landing$effectId, StoxLanding$Landing[[e]])  
  }
  LandingEffectIds <- StoxLanding$Landing$effectId

  flatbiotic <- flatbiotic[!(flatbiotic$effectId %in% LandingEffectIds),]
  
  if (nrow(flatbiotic)==0){
    return()
  }
  
  for (i in 1:nrow(flatbiotic)){
    warningString <- "Sample taken from cell not in landings "
    effectDescr <- c()
    for (e in effects){
      effectDescr <- c(effectDescr, paste(e,flatbiotic[[e]][[i]],sep="="))
    }
    
    warningString <- paste0(warningString, "(", paste(effectDescr, collapse=", "), "):")
    keyDescr <- c()
    if ("CruiseKey" %in% names(flatbiotic)){
      keyDescr <- c(keyDescr, paste("CruiseKey:",flatbiotic$CruiseKey[[i]]))
    }
    if ("StationKey" %in% names(flatbiotic)){
      keyDescr <- c(keyDescr, paste("StationKey:",flatbiotic$StationKey[[i]]))
    }
    if ("HaulKey" %in% names(flatbiotic)){
      keyDescr <- c(keyDescr, paste("HaulKey:",flatbiotic$HaulKey[[i]]))
    }
    if ("SpeciesCategoryKey" %in% names(flatbiotic)){
      keyDescr <- c(keyDescr, paste("SpeciesCategoryKey:",flatbiotic$SpeciesCategoryKey[[i]]))
    }
    if ("SampleKey" %in% names(flatbiotic)){
      keyDescr <- c(keyDescr, paste("SampleKey:",flatbiotic$Sample[[i]]))
    }
    if ("IndividualKey" %in% names(flatbiotic)){
      keyDescr <- c(keyDescr, paste("IndividualKey:",flatbiotic$IndividualKey[[i]]))
    }
    warningString <- paste(warningString, paste(keyDescr, collapse=", "))
    stoxWarning(warningString)
  }
  
  stop("Cannot proceed with samples taken from cells that are not in landings,")
  
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
#'  #### Cells
#'  Reca conceptually decomposes a fishery into cells. Parameters are estimated for each cell,
#'  and later total catch at age is predicted for each cell. Cells must therefore be defined for
#'  both the census of landings, and for the samples.
#'  
#'  'Cells' are defined by all effects / covariates that are specified for both 'StoxLandingData' and 'StoxBioticData' 
#'  (all effects are specified by one of the arguments: 'FixedEffects', 'RandomEffects', and 'CarEffect'). 
#'  Any variable that are to be included in
#'  the cell definition must always be observed (not containing NAs) 
#'  and be coded coherently (the same coding system) in both 'StoxBioticData' and 'StoxLandingData'.
#'  
#'  #### Covariates
#'  Only effects that are part of the cell definition may be specified as 'Fixed effect' or 'CAR effect'.
#'  For fixed effect all values / levels of the effect must be sampled.
#'  For CAR effect, the cell or neighbouring cells (as defined by 'CarNeighbours') must be sampled
#'  
#'  Any variable in samples 'StoxBioticData' may be included as a random effect as long as it is always observed
#'  (not containing NAs).
#' 
#'  A random effect identifying haul / landing is always included in Reca. Do not add haul-identifiers as covariates.
#'  
#'  #### Stock splitting
#'  The option UseStockSplitting requires that a Variable 'otolithtype' is added to
#'  the table 'Individual' in StoxBioticData.
#'  This is developed for splitting coastal cod from atlantic cod, and that is reflected in the Reca documentation,
#'  but this is abstracted in StoX, so that other stocks may be encoded to fit the analysis.
#'  For some cases it may even be extended to other domains defined for individual fish.
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
#'  Configures the cell effect. Defaults to `r RstoxFDA:::stoxFunctionAttributes$PrepareRecaEstimate$functionParameterDefaults$CellEffect`. If 'All', an interaction term will be added with all covariates that in the cell (whether they are fixed or random effects).
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
#'  defaults to Defaults to `r RstoxFDA:::stoxFunctionAttributes$PrepareRecaEstimate$functionParameterDefaults$HatchDay`.
#'  encoding the day of the year when fish is consider to transition from one age to the next. 1 represents Jan 1st.
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
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
#' @md
PrepareRecaEstimate <- function(StoxBioticData, StoxLandingData, FixedEffects=character(), RandomEffects=character(), UseCarEffect=FALSE, CarEffect=character(), CarNeighbours, UseAgingError=FALSE, AgeErrorMatrix, UseStockSplitting=FALSE, UseStockSplittingError=FALSE, StockSplittingParameters, CellEffect=c("Off", "All"), MinAge=integer(), MaxAge=integer(), MaxLength=numeric(), LengthResolution=numeric(), HatchDay=integer()){
  #expose as parameter when implemented
  ContinousEffect<-NULL
  
  CellEffect <- getDefault(CellEffect, "CellEffect", F, RstoxFDA::stoxFunctionAttributes$PrepareRecaEstimate$functionParameterDefaults$CellEffect)
  CellEffect <- checkOptions(CellEffect, "CellEffect", c("Off", "All"))
  
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
    stop(paste("Option", CellEffect, "is not supported for parameter 'CellEffect'"))
  }
  
  HatchDay <- getDefault(HatchDay, "HatchDay", F, RstoxFDA::stoxFunctionAttributes$PrepareRecaEstimate$functionParameterDefaults$HatchDay)
  checkMandatory(HatchDay, "HatchDay")
  
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

  if (!is.StoxBioticData(StoxBioticData)){
    stop("Malformed StoxBioticData.")
  }
  if (!RstoxData::is.StoxLandingData(StoxLandingData)){
    stop("Malformed StoxLandingData")
  }

  #
  # checks for NAs in covariates
  #
  effects <- c(FixedEffects, RandomEffects, CarEffect, "IndividualTotalLength", "DateTime")
  warnMissingCovariateStoxBiotic(effects, StoxBioticData)
  effects <- c(FixedEffects, RandomEffects, CarEffect)
  warnMissingLandings(StoxBioticData, StoxLandingData, effects)
  
  
  #
  # Set temporal resolution.
  # Landings compilation are only done to ensure coding consistency.
  # The temporal resolution doesnt matter.
  #
  quarter <- data.table::quarter(StoxLandingData$Landing$CatchDate)
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
    if(!all(is.na(flatbiotic$Age) | flatbiotic$Otolithtype %in% c(1,2,4,5))){
      stoxWarning("Some aged fish does not have Otolithtype set, or have it set to an unrecognized value. This may slow down Stox processing of Reca results.")
    }
  }
  
  nFish = NULL
  #
  # set the nFish table for any catches where several fractions have been sampled (delprøve)
  #
  if (length(unique(flatbiotic$catchId)) != length(unique(flatbiotic$sampleId))){
    allSamples <- unique(flatbiotic[,c("catchId", "sampleId")])
    stratifiedCatchIds <- allSamples$catchId[duplicated(allSamples$catchId)]
    nFish <- flatbiotic[flatbiotic$catchId %in% stratifiedCatchIds,c("sampleId", "CatchFractionNumber", "HaulKey", "SampleKey", "CruiseKey", "StationKey")]
    nFish <- nFish[!duplicated(nFish$sampleId),]
    if (any(is.na(nFish$CatchFractionNumber))){
      missing <- nFish[is.na(nFish$CatchFractionNumber),]
      for (i in 1:nrow(missing)){
        HaulKey <- missing$HaulKey[i]
        SampleKey <- missing$SampleKey[i]
        CruiseKey<- missing$CruiseKey[i]
        StationKey<- missing$StationKey[i]
        stoxWarning(paste("'CatchFractionNumber' missing from Sample", SampleKey, "from Haul", HaulKey, "which have several catch fractions sampled. Cruise:", CruiseKey, "Station:", StationKey))
      }
      stop("'StoxBioticData' have 'CatchFractionNumber' missing from the 'Sample' table for Hauls with several fractions sampled.")
    }
    nFish <- nFish[,c("sampleId", "CatchFractionNumber")]
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


#' Run Reca.
#' @description
#'  This function is deprecated and are being kept for testing purposes. 
#'  It is replaced by \code{\link[RstoxFDA]{ParameterizeRecaModels}} and \code{\link[RstoxFDA]{RunRecaModels}}
#' @details
#'  \code{\link[Reca]{eca.estimate}} performs Markov-chain Monte Carlo (MCMC) simulations to determine maximum likelihood of parameters for the given samples.
#'
#'  \code{\link[Reca]{eca.predict}} samples the posterior distributions of parameters estimated in \code{\link[Reca]{eca.estimate}},
#'  in order to obtain proportions of catches and fish parameters.
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
#' @concept deprecated
#' @md
#' @export
RunRecaEstimate <- function(RecaData, Nsamples=integer(), Burnin=integer(), Thin=integer(), Lgamodel=c("log-linear", "non-linear"), Resultdir=character(), Delta.age=numeric(), Seed=numeric(), Caa.burnin=numeric()){

  deprecationWarning("RunRecaEstimate", "August 2022")
  
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
#'  #### Seed
#'  If 'Seed' is not provided a random seed is chosen. This is stored in the returned data (RecaParameterData$GlobalVariables$Seed).
#'  This seed is passed to Reca, but not all versions of Reca has provided exact reproducability for a given seed,
#'  so the behaviour is dependent on the installed Reca-version.
#'  
#'  #### Caching
#'  The argument 'UseCachedData' allows previously computed parameterization to be returned in stead of parameterizing again.
#'  If no previous run is located in the 'ResultDirectory', or the arguments or data that are passed to Reca differs from the previous run, 
#'  execution will halt with an error when 'UseCachedData'. In this respect it may also be useful to note a counter intuitive
#'  aspect of the argument 'Seed'. If 'Seed' was not provided for the previous run, the arguments will be considered equal if
#'  the seed is set to the value returned on the previous run (RecaParameterData$GlobalVariables$Seed).
#'
#'  #### ResultDirectory files:
#'  Various report functions may use output of this function with the function \code{\link[Reca]{eca.predict}} which samples the posterior distributions of parameters.
#'  Communication between \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}} is managed by writing and reading files, 
#'  and a directory for storing intermediate calculations must be provided with the parameter 'ResultDirectory'.
#'  For multi-chain analysis, a different directory should be provided for each chain. 
#'  
#'  Be aware that this breaks with the general design of StoX and somewhat limits the transferrability of
#'  StoX projects between computers. 
#'
#' @param RecaData \code{\link[RstoxFDA]{RecaData}} as returned from \code{\link[RstoxFDA]{PrepareRecaEstimate}}
#' @param Nsamples number of MCMC samples that will be made available for \code{\link[Reca]{eca.predict}}. See documentation for \code{\link[Reca]{eca.estimate}},
#' @param Burnin number of MCMC samples run and discarded by \code{\link[Reca]{eca.estimate}} before any samples are saved. See documentation for \code{\link[Reca]{eca.estimate}}.
#' @param Lgamodel The length age relationship to use for length-age fits (options: "log-linear", "non-linear": Schnute-Richards model). See documentation for \code{\link[Reca]{eca.estimate}}. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ParameterizeRecaModels$functionParameterDefaults$Lgamodel`
#' @param ResultDirectory a directory where Reca may store temp-files \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}.
#' @param Thin controls how many iterations are run between each samples saved. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ParameterizeRecaModels$functionParameterDefaults$Thin`. This may be set to account for autocorrelation introduced by Metropolis-Hastings simulation. see documentation for \code{\link[Reca]{eca.estimate}}
#' @param Delta.age see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ParameterizeRecaModels$functionParameterDefaults$Delta.age`.
#' @param Seed see documentation for \code{\link[Reca]{eca.estimate}}. Defaults to random seed.
#' @param UseCachedData if TRUE Parameterization is not run, but any previous runs for exactly the same arguments are returned.
#' @return \code{\link[RstoxFDA]{RecaParameterData}} results from Reca Model Parameterization.
#' @seealso \code{\link[RstoxFDA]{PrepareRecaEstimate}} for model configuration, and data preparation for this function, and
#'  \code{\link[RstoxFDA]{RunRecaModels}} for obtaining predictions / estimates from the Reca-models.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
#' @md
ParameterizeRecaModels <- function(RecaData, Nsamples=integer(), Burnin=integer(), Thin=integer(), ResultDirectory=character(), Lgamodel=c("log-linear", "non-linear"), Delta.age=numeric(), Seed=numeric(), UseCachedData=FALSE){
  
  if (!isGiven(RecaData)){
    stop("Parameter 'RecaData' must be provided.")
  }
  
  RecaData <- convertStox2PrepReca(RecaData)
  Lgamodel <- getDefault(Lgamodel, "Lgamodel", F, RstoxFDA::stoxFunctionAttributes$ParameterizeRecaModels$functionParameterDefaults$Lgamodel)
  Lgamodel <- checkOptions(Lgamodel, "Lgamodel", c("log-linear", "non-linear"))

  if (!isGiven(Seed)){
    Seed <- sample.int(.Machine$integer.max, 1)
  }
  
  checkMandatory(ResultDirectory, "ResultDirectory")
  ResultDirectory <- path.expand(ResultDirectory)
  
  if (!file.exists(ResultDirectory)){
    stop(paste("Could not find the 'ResultDirectory'", ResultDirectory, ". See ?ParameterizeRecaModels for details about the 'ResultDirectory'."))
  }
  
  if (grepl(" ", ResultDirectory)) {
    stop(paste(
      "path to 'ResultDirectory' may not contain spaces",
      "(current:",
      ResultDirectory,
      ") ."
    ))
  }
  
  timestring <- gsub("-", "", gsub(":", "-", gsub(" ", ".", Sys.time())))
  
  fitfile=paste("fit", timestring, sep=".")
  predictfile=paste("pred", timestring, sep=".")
  
  if (!isGiven(Nsamples)){
    stop("Parameter 'Nsamples' must be provided.")
  }
  if (!isGiven(Burnin)){
    stop("Parameter 'Burnin' must be provided.")
  }
  
  Thin <- getDefault(Thin, "Thin", F, RstoxFDA::stoxFunctionAttributes$ParameterizeRecaModels$functionParameterDefaults$Thin)
  checkMandatory(Thin, "Thin")
  
  Delta.age <- getDefault(Delta.age, "Delta.age", F, RstoxFDA::stoxFunctionAttributes$ParameterizeRecaModels$functionParameterDefaults$Delta.age)
  checkMandatory(Delta.age, "Delta.age")
  
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
  RecaData <- checkEcaObj(RecaData, stage="parameterize")
  
  inputcache <- file.path(path.expand(ResultDirectory), "RecaDataCache.rds")
  outputcahce <- file.path(path.expand(ResultDirectory), "RecaFitCache.rds")
  if (!UseCachedData){
    saveRDS(RecaData, file=inputcache)
    fit <- Reca::eca.estimate(RecaData$AgeLength, RecaData$WeightLength, RecaData$Landings, RecaData$GlobalParameters)
    saveRDS(fit, file=outputcahce)
  }
  else{
    
    if (!file.exists(inputcache)){
      stop("No cached input found. Re-run with UseCache=FALSE.")
    }
    if (!file.exists(outputcahce)){
      stop("No cached output found. Re-run with UseCache=FALSE.")
    }
    
    cachedRecaData <- readRDS(inputcache)
    RecaData$GlobalParameters$fitfile <- cachedRecaData$GlobalParameters$fitfile
    RecaData$GlobalParameters$predictfile <- cachedRecaData$GlobalParameters$predictfile
    
    if (!identical(cachedRecaData, RecaData)){
      diffs <- all.equal(cachedRecaData, RecaData)
      for (d in diffs){
        message(d)
      }
      stop("Arguments or data are not identical to cached run. Re-run with UseCache=FALSE.")
    }
    else{
      warning("Using cached data for ParameterizeRecaModels.")
      RecaData <- cachedRecaData
      fit <- readRDS(outputcahce)  
    }
  }
  
  
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
    quarter <- data.table::quarter(StoxLandingData$Landing$CatchDate)
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
#'  If the function-parameter 'GroupingVariables' is provided, predictions will be provided for corresponding partitions of landings.
#'  The parameter 'StoxLandingData' may differ from the landings used in parameterisation (passed to \code{\link[RstoxFDA]{ParameterizeRecaModels}}), 
#'  as long as all not additional values / levels for the model covariates / effects are introduced.
#'  
#'  If The models are configured for stock-splitting analysis. The variable 'Stock' will be added to 'GroupingVariables' in the return value (\code{\link[RstoxFDA]{RecaCatchAtAge}})
#'  
#'  If the 'GroupingVariables' specify a very large number of partitions of the landings, this function may
#'  exhaust available computer memory.
#'  
#'  #### CollapseLength
#'  By default length groups are collapsed into one length group in the result. This does not facilitate reporting
#'  length resolved data, such as length distributions.
#'  The full age-length prediction may be extracted by setting the parameter 'CollapseLength' to FALSE.
#'  If this is used in combination with several grouping variables, there is some risk of exhausting available computer memory.
#' @param RecaParameterData Parameters for Reca models.
#' @param StoxLandingData Landings data (\code{\link[RstoxData]{StoxLandingData}}).
#' @param GroupingVariables character vector identifying columns in 'StoxLandingData' that results should be provided for.
#' @param TemporalResolution
#'  Code for temporal resolution in landings: "Month" or "Quarter". Defaults to `r RstoxFDA:::stoxFunctionAttributes$RunRecaModels$functionParameterDefaults$TemporalResolution`.
#'  Regulates temporal resolution for calculating fractional ages of fish.
#'  Not to be confused with any temporal covariate.
#' @param Caa.burnin see documentation for \code{\link[Reca]{eca.predict}}. Defaults to `r RstoxFDA:::stoxFunctionAttributes$RunRecaModels$functionParameterDefaults$Caa.burnin`.
#' @param CollapseLength indicates whether length groups should be collapsed in result. Defaults to TRUE. See details.
#' @return \code{\link[RstoxFDA]{RecaCatchAtAge}}
#' @seealso \code{\link[RstoxFDA]{ParameterizeRecaModels}} for model parameterisation,
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtAge}}, 
#'  \code{\link[RstoxFDA]{ReportRecaLengthAtAge}}, 
#'  \code{\link[RstoxFDA]{ReportRecaWeightAtAge}} for compiling reports of predictions / estimates.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
#' @md
RunRecaModels <- function(RecaParameterData, StoxLandingData, GroupingVariables=character(), TemporalResolution=c("Quarter", "Month"), Caa.burnin=numeric(), CollapseLength=TRUE){
  
  checkMandatory(RecaParameterData, "RecaParameterData")
  checkMandatory(StoxLandingData, "StoxLandingData")

  TemporalResolution <- getDefault(TemporalResolution, "TemporalResolution", F, RstoxFDA::stoxFunctionAttributes$RunRecaModels$functionParameterDefaults$TemporalResolution)
  TemporalResolution <- checkOptions(TemporalResolution, "TemporalResolution", c("Quarter", "Month"))

  Caa.burnin <- getDefault(Caa.burnin, "Caa.burnin", F, RstoxFDA::stoxFunctionAttributes$RunRecaModels$functionParameterDefaults$Caa.burnin)
  checkMandatory(Caa.burnin, "Caa.burnin")
  
  if (length(GroupingVariables)>1 && !CollapseLength){
    stoxWarning("Producing estimates for all length groups in combination with age and several 'GroupingVariables'. This may exhaust memory, consider the option 'CollapseLength'.")
  }
  
  #discard fit info and convert
  RecaParameterData$FitProportionAtAge <- NULL
  RecaParameterData$FitLengthGivenAge <- NULL
  RecaParameterData$FitWeightGivenLength <- NULL
  RecaParameterData <- convertStox2PrepReca(RecaParameterData)
  
  RecaParameterData$GlobalParameters$caa.burnin <- Caa.burnin
  
  if (!isGiven(GroupingVariables)){
    landings <- getLandingsFromStoxLandings(RecaParameterData, StoxLandingData, TemporalResolution)
    RecaParameterData$Landings <- landings
    results <- Reca::eca.predict(RecaParameterData$AgeLength, RecaParameterData$WeightLength, RecaParameterData$Landings, RecaParameterData$GlobalParameters)
    results <- ecaResult2Stox(results, RecaParameterData$CovariateMaps$StockSplitting)
    results$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
    if (RecaParameterData$GlobalParameters$CC){
      stopifnot("Stock" %in% names(results$CatchAtAge))
      if ("Stock" %in% names(results$GroupingVariables)){
        stop("Cannot add 'Stock' to aggregation variables, when it already exists as an aggregation variable")
      }
      results$GroupingVariables <- data.table::data.table(GroupingVariables=c(results$GroupingVariables$GroupingVariables, "Stock"))
    }
    
    if (CollapseLength){
      colNames <- names(results$CatchAtAge)
      colNames <- colNames[!(colNames %in% c("Length", "CatchAtAge"))]
      
      results$CatchAtAge <- results$CatchAtAge[, list(CatchAtAge=sum(get("CatchAtAge"))), by=colNames]
      results$CatchAtAge$Length <- RecaParameterData$GlobalParameters$maxlength
    }
    
    
    return(results)
    
  }
  else{
    if (!all(GroupingVariables %in% names(StoxLandingData$Landing))){
      missing <- GroupingVariables[!(GroupingVariables %in% names(StoxLandingData$Landing))]
      stop(paste("Parameter 'GroupingVariables' contain some variables not found as columns in 'StoxLandingData':", paste(missing, collapse=",")))
    }
    
    result <- NULL
    frame <- unique(StoxLandingData$Landing[,.SD, .SDcols=GroupingVariables])
    frame$aggregationId <- 1:nrow(frame)
    
    #
    # have absolutely no clue what is going on here, but the data.table merge very occationally does result in an incomplete merge.
    # It seems to have to do with columns that are keys in StoxLandingData$Landing.
    #
    l <- data.table::as.data.table(merge(as.data.frame(StoxLandingData$Landing), as.data.frame(frame), by=names(frame)[names(frame) %in% names(StoxLandingData$Landing)]))
    for (id in frame$aggregationId){
      partition <- l[l$aggregationId==id,]
      Sl <- StoxLandingData
      Sl$Landing <- partition
      Landing <- getLandingsFromStoxLandings(RecaParameterData, Sl, TemporalResolution)
      RecaParameterData$Landings <- Landing
      partitionresults <- ecaResult2Stox(Reca::eca.predict(RecaParameterData$AgeLength, RecaParameterData$WeightLength, RecaParameterData$Landings, RecaParameterData$GlobalParameters), RecaParameterData$CovariateMaps$StockSplitting)
      
      if (CollapseLength){
        colNames <- names(partitionresults$CatchAtAge)
        colNames <- colNames[!(colNames %in% c("Length", "CatchAtAge"))]
        
        partitionresults$CatchAtAge <- partitionresults$CatchAtAge[, list(CatchAtAge=sum(get("CatchAtAge"))), by=colNames]
        partitionresults$CatchAtAge$Length <- RecaParameterData$GlobalParameters$maxlength
      }
      
      for (a in GroupingVariables){
        stopifnot(length(unique(partition[[a]]))==1) # ensured by frame <- unique(StoxLandingData$Landing[,GroupingVariables, with=F])
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
    result$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
    
    if (RecaParameterData$GlobalParameters$CC){
      
      stopifnot("Stock" %in% names(result$CatchAtAge))
      if ("Stock" %in% names(result$GroupingVariables)){
        stop("Cannot add 'Stock' to aggregation variables, when it already exists as an aggregation variable")
      }
      result$GroupingVariables <- data.table::data.table(GroupingVariables=c(result$GroupingVariables$GroupingVariables, "Stock"))
    }
    
    return(result)
  }
  
}
