#' Construct design parameters assuming FSWOR, non-finite, equal prob, potentially stratified
#' @noRd
assumeDesignParametersStoxBiotic <- function(StoxBioticData, SamplingUnitId, StratificationColumns=c()){
  targetTable <- NULL
  for (n in names(StoxBioticData)){
    if (SamplingUnitId %in% names(StoxBioticData[[n]])){
      targetTable=n
    }
  }
  
  if (is.null(targetTable)){
    stop(paste("The SamplingUnitId", SamplingUnitId, "was not found in StoxBioticData"))
  }
  
  flatStox <- StoxBioticData[[targetTable]]
  if (isGiven(StratificationColumns) & length(StratificationColumns)>0 & !all(StratificationColumns %in% names(flatStox))){
    stop("Not all stratification columns were found at ", targetTable, ", where the SamplingUnitId ", SamplingUnitId, " is found.")
  }

  if (any(is.na(flatStox[[SamplingUnitId]]))){
    stop(paste("Cannot construct design parameters for missing SamplingUnitIds. Missing values (NA) found for", SamplingUnitId))
  }
  for (n in StratificationColumns){
    if (any(is.na(flatStox[[n]]))){
      stop(paste("Cannot construct design parameters with missing strata information. Missing values (NA) found for stratification column", n))
    } 
  }

  flatStox$Stratum <- "All"
  if (length(StratificationColumns)>0){
    flatStox$Stratum <- apply(flatStox[,.SD, .SDcol=StratificationColumns], 1, paste, collapse="/")    
  }
  flatStox$SamplingUnitId <- flatStox[[SamplingUnitId]]
  flatStox$Order <- as.numeric(NA)
  
  CommonSelectionData <- flatStox[,list(InclusionProbability=as.numeric(NA), HTsamplingWeight=as.numeric(NA), SelectionProbability=as.numeric(NA), HHsamplingWeight=1/length(unique(SamplingUnitId)), SelectionDescription=as.character(NA)), by=c("Stratum")]
  selectionUnits <- flatStox[,.SD, .SDcol=c("Stratum", "Order", "SamplingUnitId")]
  selectionUnits <- selectionUnits[!duplicated(selectionUnits$SamplingUnitId),]
  selectionTable <- merge(flatStox[,.SD, .SDcol=c("Stratum", "Order", "SamplingUnitId")], CommonSelectionData)
  sampleTable <- flatStox[,list(N=as.numeric(NA), n=length(unique(SamplingUnitId)), SelectionMethod="FSWR", FrameDescription=as.character(NA)), by=c("Stratum")]
  sampleTable <- sampleTable[,.SD,.SDcol=c("Stratum", "N", "n", "SelectionMethod", "FrameDescription")]
  stratificationTable <- flatStox[,.SD,.SDcol=c("Stratum", StratificationColumns)]
  stratificationTable <- stratificationTable[!duplicated(stratificationTable$Stratum),]

  designParameters <- list()
  designParameters$SampleTable <- sampleTable
  designParameters$SelectionTable <- selectionTable
  designParameters$StratificationVariables <- stratificationTable

  return(designParameters)
  
}

#' parse design parameters from tab delimited file
#' @noRd
parseDesignParameters <- function(filename){
  
  colClasses <- c(Stratum="character", N="numeric", n="numeric", SelectionMethod="character", FrameDescription="character", Order="numeric", SamplingUnitId="character", InclusionProbability="numeric", HTsamplingWeight="numeric", SelectionProbability="numeric", HHsamplingWeight="numeric", SelectionDescription="character")
  headers <- data.table::fread(filename, sep="\t", dec=".", header = T, nrows = 1)
  if (!all(names(colClasses) %in% names(headers))){
    missing <- names(colClasses)[!(names(colClasses) %in% names(headers)),]
    stop(paste("Invalid format. Missing columns:", paste(missing, collapse=",")))
  }
  stratificationColumns <- c()
  for (n in names(headers)){
    if (!n %in% names(colClasses)){
      stratificationColumns <- c(stratificationColumns, n)
      newnames <- c(names(colClasses), n)
      colClasses <- c(colClasses, "character")
      names(colClasses) <- newnames
    }
  }  
  
  designParameters <- data.table::fread(filename, sep="\t", dec=".", header = T, colClasses = colClasses, na.strings = c(""))

  selectionTable <- designParameters[,.SD,.SDcol=c("Stratum", "Order", "SamplingUnitId", "InclusionProbability", "HTsamplingWeight", "SelectionProbability", "HHsamplingWeight", "SelectionDescription")]
  sampleTable <- designParameters[,.SD,.SDcol=c("Stratum", "N", "n", "SelectionMethod", "FrameDescription")]
  stratificationTable <- designParameters[,.SD,.SDcol=c("Stratum", names(designParameters)[!(names(designParameters) %in% names(selectionTable)) & !(names(designParameters) %in% names(sampleTable))])]
  
  if (any(is.na(sampleTable$Stratum)) | any(is.na(selectionTable$Stratum))){
    stop("Invalid design specification. The mandatory column 'Stratum' may not contain missing values (NA).")
  }
  if (any(is.na(sampleTable$SelectionMethod))){
    stop("Invalid design specification. The mandatory column 'SelectionMethod' may not contain missing values (NA).")
  }
  
  for (n in stratificationColumns){
    stopifnot(n %in% names(sampleTable))
    if (any(is.na(sampleTable[[n]]))){
      stop(paste("Invalid design specification. The stratification column", n, "may not contain missing values (NA)."))
    }
  }
  
  sampleTableStrings <- apply(sampleTable, 1, paste, collapse="/")
  sampleTable <- sampleTable[!duplicated(sampleTableStrings),]
  duplicatedStrata <- sampleTable$Stratum[duplicated(sampleTable$Stratum)]
  if (length(duplicatedStrata)>0){
    stop(paste("Invalid design specification. The column stratum must uniquely identify all sample table variables. Duplicates found for:", paste(duplicatedStrata, collapse=",")))
  }

  if (length(stratificationColumns) > 0){
    stratificationVariableStrings <- apply(stratificationTable[,.SD, .SDcol=stratificationColumns], 1, paste, collapse="/")
    duplicatedStrata <- stratificationTable$Stratum[duplicated(stratificationVariableStrings)]
    
    if (length(duplicatedStrata)>0){
      stop(paste("Invalid design specification. The stratification variables must uniquely identify a stratum. Duplicates found for:", paste(duplicatedStrata, collapse=",")))
    }
  }
  
  if (any(!is.na(selectionTable$SampleUnitId) & duplicated(paste(selectionTable$Stratum, selectionTable$SampleUnitId)))){
    stop("Invalid design specification. Some strata contain duplicated SampleUnitIds.")
  }
  
  stratificationTable <- stratificationTable[!duplicated(stratificationTable$Stratum),]

  validSelectionMethod <- c("Poisson", "FSWR", "FSWOR")
  if (!all(sampleTable$SelectionMethod %in% validSelectionMethod)){
    invalid <- sampleTable$SelectionMethod[!(sampleTable$SelectionMethod %in% validSelectionMethod)]
    stop(paste("Invalid design specification. Unkown selection method:", paste(invalid, collapse=",")))
  }
  
  designParameters <- list()
  designParameters$SampleTable <- sampleTable
  designParameters$SelectionTable <- selectionTable
  designParameters$StratificationVariables <- stratificationTable

  return(designParameters)
}

#' Define PSU Sampling Design Parameters
#' @description 
#'  Define sampling parameters for Primary Sampling Units in multi-stage sampling.
#' @details 
#'  The DefintionMethod 'ResourceFile' reads sampling parameters from a tab delimited file with headers corresponding to those listed in 
#'  \code{\link[RstoxFDA]{PSUSamplingParametersData}}. The data is provided as one table, so that the information in 'sampleTable' is repeated for each entry in 'selectionTable'.
#'  Any columns not named in \code{\link[RstoxFDA]{PSUSamplingParametersData}} are assumed to be stratification variables.
#'  The conditions listed for the variables in \code{\link[RstoxFDA]{PSUSamplingParametersData}} are checked upon reading the data, and
#'  execution halts with error if any are violated.
#'  
#'  The DefinitionMethod 'AdHocStoxBiotic' constructs Sampling Design Parameters from data, 
#'  assuming equal probability sampling with fixed sample size, selection with replacement and complete response.
#'  These is a reasonable approximation if within-strata sampling is approximately simple random selections,
#'  the sample intensitiy is low (only a small fraction of the population is sampled),
#'  and non-response is believed to be at random.
#' @param processData \code{\link[RstoxFDA]{PSUSamplingParametersData}} as returned from this function.
#' @param DefinitionMethod 'ResourceFile' or 'AdHocStoxBiotic'
#' @param FileName path to resource file
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} Sample data to construct design parameters from
#' @param SamplingUnitId name of column in 'StoxBioticData' that identifies the Primary Sampling Unit the design is constructed for.
#' @param StratificationColumns name of any column (at the same table as 'SamplingUnitId') that are to be used to define Strata for sampling.
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{PSUSamplingParametersData}}
#' @export
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
DefinePSUSamplingParameters <- function(processData, DefinitionMethod=c("ResourceFile", "AdHocStoxBiotic"), FileName=character(), StoxBioticData, SamplingUnitId=character(), StratificationColumns=character(), UseProcessData=F){

  if (UseProcessData){
    return(processData)
  }
  
  DefinitionMethod <- checkOptions(DefinitionMethod, "DefinitionMethod", c("ResourceFile", "AdHocStoxBiotic"))
  
  if (DefinitionMethod == "ResourceFile"){
    return(parseDesignParameters(FileName))
  }
  if (DefinitionMethod == "AdHocStoxBiotic"){
    return(assumeDesignParametersStoxBiotic(StoxBioticData, SamplingUnitId, StratificationColumns))
  }
}

#' collapse strata, recalulate n/N and sampling weights
#' For strata with unknown inclusion and selection probability, sampling weights will be NA.
#' @noRd
collapseStrataIndividualDesignParamaters <- function(designParam, collapseVariables=c()){

  sv <- names(designParam$StratificationVariables)[!names(designParam$StratificationVariables) %in% c("SampleId", "Stratum")]
  if (!all(collapseVariables %in% sv)){
    missing <- collapseVariables[!(collapseVariables %in% sv)]
    stop("The following are specified as strata to collapse, but are not StratificationVariables:", paste(missing, collapse=","))
  }

  retain <- sv[!(sv %in% collapseVariables)]
    
  #change strata definition
  if (length(retain)==0){
      designParam$StratificationVariables$newStratum <- "All" 
  }
  else{
      designParam$StratificationVariables$newStratum <- apply(designParam$StratificationVariables[,.SD,.SDcol=c(retain)], FUN=paste, 1, collapse="/")
  }
  designParam$StratificationVariables <- designParam$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum", retain, "newStratum")]

  designParam$SelectionTable <- merge(designParam$SelectionTable, designParam$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum", "newStratum")], by=c("SampleId", "Stratum"))
  designParam$SampleTable <- merge(designParam$SampleTable, designParam$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum", "newStratum")], by=c("SampleId", "Stratum"))
  
  NselectionMethods <- designParam$SampleTable[,list(NselMet=length(unique(SelectionMethod))), by=c("SampleId", "newStratum")]
  if (any(NselectionMethods$NselMet>1)){
    stop("Cannot collapse strata with heterogenous selection methods.")
  }

  totalN <- designParam$SampleTable[,list(totalN=sum(N), allSampled=all(n>0), totalStrata=length(unique(Stratum))), by=c("SampleId", "newStratum")]
  weights <- merge(designParam$SampleTable, totalN, by=c("SampleId", "newStratum"), all.x=T)
  weights$strataweight <- 1
  
  unSampled <- weights$SampleId[weights$totalStrata>1 & !weights$allSampled]
  if (length(unSampled)>0){
    stop("Cannot collapse unsampled strata. Unsampled strata exists for samples: ", truncateStringVector(unSampled))
  }
  if (any(weights$totalStrata>1)){
    weights$strataweight[weights$totalStrata>1] <- weights$N[weights$totalStrata>1] / weights$totalN[weights$totalStrata>1]    
  }

  missing <- weights[is.na(weights$totalN) & weights$totalStrata>1,]
  if (nrow(missing)>0){
    samples <- unique(missing$SampleId)
    stop("Cannot collapse strata with unkown strata sizes. Sample size missing for some strata in the samples:", truncateStringVector(samples))
  }

  weights <- weights[,.SD, .SDcol=c("SampleId", "Stratum", "newStratum", "strataweight")]

  designParam$SelectionTable <- merge(designParam$SelectionTable, weights, by=c("SampleId", "Stratum", "newStratum"))
  designParam$SelectionTable$HTsamplingWeight <- designParam$SelectionTable$HTsamplingWeight * designParam$SelectionTable$strataweight
  designParam$SelectionTable$HHsamplingWeight <- designParam$SelectionTable$HHsamplingWeight * designParam$SelectionTable$strataweight
  designParam$SelectionTable$strataweight <- NULL
  
  designParam$SelectionTable$Stratum <- designParam$SelectionTable$newStratum
  designParam$SelectionTable$newStratum <- NULL
  
  designParam$SampleTable$Stratum <- designParam$SampleTable$newStratum
  designParam$SampleTable$newStratum <- NULL
  
  designParam$StratificationVariables$Stratum <- designParam$StratificationVariables$newStratum
  designParam$StratificationVariables$newStratum <- NULL
  
  designParam$SampleTable <- designParam$SampleTable[,list(N=sum(N), n=sum(n), SelectionMethod=SelectionMethod[1], SampleDescription=as.character(NA)), by=c("SampleId", "Stratum")]
  designParam$StratificationVariables <- designParam$StratificationVariables[!duplicated(paste(designParam$StratificationVariables$SampleId, designParam$StratificationVariables$Stratum)),.SD, .SDcol=c("SampleId", "Stratum", retain)]
  return(designParam)
}

#' make IndividualDesignParameters for stratified selection of Individuals
#' @noRd
extractIndividualDesignParametersStoxBiotic <- function(StoxBioticData, StratificationColumns, Parameters){
  
  StratificationColumns <- c("SpeciesCategory", StratificationColumns)
  
  individuals <- RstoxData::mergeByIntersect(StoxBioticData$Individual, StoxBioticData$Sample)
  individuals <- RstoxData::mergeByIntersect(individuals, StoxBioticData$SpeciesCategory)
  individuals <- RstoxData::mergeByIntersect(individuals, StoxBioticData$Haul)
    
  if (any(is.na(individuals$CatchFractionNumber))){
    missing <- unique(individuals$Sample[is.na(individuals$CatchFractionNumber)])
    stop(paste("Cannot infer sampling parameters for individuals from Samples with missing total number. CatchFractionNumber missing for Sample:", truncateStringVector(missing)))
  }
  
  #check first, so no restrictions need to be put on names of Parameters.
  hasParam <- rep(FALSE, nrow(individuals))
  for (p in Parameters){
    hasParam <- hasParam | !is.na(individuals[[p]])
  }
  
  if (length(StratificationColumns)>0){
    individuals$Stratum <- apply(individuals[,.SD, .SDcol=StratificationColumns], 1, paste, collapse="/")
  }
  else{
    individuals$Stratum <- "All"
  }

  individuals$SampleId <- individuals$Haul
  
  stratificationTable <- individuals[!duplicated(paste(individuals$SampleId, individuals$Stratum)), .SD,.SDcol=c("SampleId", "Stratum", StratificationColumns)]
  
  individuals$Sampled <- hasParam
  stratumTotals <- individuals[,list(totalInStratum=.N, sampledInStratum=sum(Sampled)), by=c("Stratum", "SampleId")]
  sampleTotals <- individuals[,list(totalInSample=.N), by=c("SampleId")]
  stratumFraction <- merge(stratumTotals, sampleTotals, by="SampleId")
  stratumFraction$StratumFraction <- stratumFraction$totalInStratum / stratumFraction$totalInSample
  individuals <- merge(individuals, stratumFraction, by=c("SampleId", "Stratum"))
  individuals$N <- individuals$CatchFractionNumber*individuals$StratumFraction
  individuals$n <- individuals$sampledInStratum

  sampleTable <- individuals[!duplicated(paste(individuals$Stratum, individuals$SampleId)), .SD, .SDcol=c("SampleId", "Stratum", "N", "n")]  
  sampleTable$SelectionMethod <- "FSWOR"
  sampleTable$SampleDescription <- as.character(NA)
  
  selectedIndividuals <- individuals[individuals$Sampled,]
  selectedIndividuals$IndividualId <- selectedIndividuals$Individual
  selectedIndividuals$Order <- as.numeric(NA)
  selectedIndividuals$InclusionProbability <- selectedIndividuals$n/selectedIndividuals$N
  selectedIndividuals$HTsamplingWeight <- 1/selectedIndividuals$n
  selectedIndividuals$SelectionProbability <- as.numeric(NA) #Need order. Could possibly be obtained by convention from StoxBioticiIndividual$IndividualKey, would have to be user choice.
  selectedIndividuals$HHsamplingWeight <- as.numeric(NA)
  selectedIndividuals$SelectionDescription <- as.character(NA)
  
  selectionTable <- selectedIndividuals[,.SD,.SDcol=c("SampleId", "Stratum", "Order", "IndividualId", "InclusionProbability", "HTsamplingWeight", "SelectionProbability", "HHsamplingWeight", "SelectionDescription")]
  
  designParams <- list()
  designParams$SampleTable <- sampleTable
  designParams$SelectionTable <- selectionTable
  designParams$StratificationVariables <- stratificationTable
  
  return(designParams)
}

#' Define Sampling Parameters for Individuals
#' @description 
#'  Define approximate sampling parameters for the selection of individuals from a haul. Design parameters are inferred from data provided in ~\code{\link[RstoxData]{StoxBioticData}},
#'  and specify how a set of individuals recorded on the Individual table were selected for observation/measurement from a Haul (the table Haul in StoxBioticData).
#' @details 
#'  StoxBioticData represents sorting of species as a separate level in the hierarchy (SpeciesCategory) and Samples are selected in Stratified from the species categories.
#'  This represent sampling stratified on taxons in addition to some additional stratification criteria in the cases where more than one sample is present for
#'  a species-category in a Haul. The exact criteria for stratification is not important for the calculation of sampling parameters, but only clearly encoded criteria can be used
#'  in subsequent analysis, so sampling parameters are reported stratified only on SpeciesCategory. Any other stratification has been incorporated into selection or inclusion probabilities.
#'  
#'  Sampling parameters are approximately inferred, assuming that all selected individuals are recorded, and based on some user-controllable assumptions about the selection process,
#'  specified by the appropriate 'DefinitionMethod'. 
#'  
#'  Individuals with a non-missing value for any of the parameters in 'Parameters' are treated as selected for observation.
#'  In this way selection of individuals may be specified differently for different parameters.
#'  For instance one may define one design for length-measurements and another for length-stratified age, weight and sex observations.
#'  
#'  The available DefinitionMethods specifies how Individuals are selected from a Sample, and are:
#'  \describe{
#'   \item{SRS}{Simple Random Selection. Individuals are selected for measurment by simple random selection without replacement from each Sample.}
#'   \item{Stratified}{Stratified Selection. Individuals are selected for measurement by stratified random selection without replacement from each Sample. Strata are specified as the combination of columns provided in 'StratificationColumns'. The number of fish in each stratum is estimated by the total in sample and the proportion of measured fish in each stratum.}
#'   \item{LengthStratified}{Length stratified selection. Individuals are selected for measurement by stratified random selection without replacement from each Sample. Strata are length groups, specified by the left closed intervals starting with [0,'LengthInterval'>.}
#'  }
#'  
#'  
#' @param processData \code{\link[RstoxFDA]{IndividualSamplingParametersData}} as returned from this function.
#' @param StoxBioticData Data to define individual sampling parameters for
#' @param DefinitionMethod Method to infer sampling parameters, 'SRS', 'Stratified' or 'LengthStratified'. See details.
#' @param Parameters Measurements / observations of individuals included in the design specification. Must all be columns on the Individual-table of StoxBioticData. 
#' @param LengthInterval width of length strata in cm. Specifies left closed intervals used for Length Stratified selection (DefinitionMethod 'Stratified'). A value of 5 indicates that observation are selected stratified on length groups [0 cm,5 cm>, [5 cm, 10 cm>, and so on.
#' @param StratificationColumns names of columns in the Individual table of StoxBioticData that identify strata for Stratified selection (DefinitionMethod 'Stratified').
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{IndividualSamplingParametersData}} where SampleId refers to the variable 'Haul' on the 'Haul' table in StoxBioticData, and IndividualId refers to the variable 'Individual' on the 'Individual' table of StoxBioticData.
#' @export
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
DefineIndividualSamplingParameters <- function(processData, StoxBioticData, DefinitionMethod=c("SRS", "Stratified", "LengthStratified"), Parameters=c(), LengthInterval=numeric(), StratificationColumns=character(), UseProcessData=FALSE){

  #May want to expose this option if DefinitionMethods are added that only provides relative selection probabilities.
  CollapseStrata=FALSE
  
  if (UseProcessData){
    return(processData)
  }
  DefinitionMethod <- checkOptions(DefinitionMethod, "DefinitionMethod", c("SRS", "Stratified", "LengthStratified"))
  checkMandatory(StoxBioticData, "StoxBioticData")
  checkMandatory(Parameters, "Parameters")
  
  if (!all(Parameters %in% names(StoxBioticData$Individual))){
    stop("All values for the argument 'Parameters' must be names columns of the Individual table in 'StoxBioticData'")
  }
  
  #
  # Perform checks and
  # Set stratification columns in accordance with DefinitionMethod
  #
  
  if (DefinitionMethod == "SRS"){
    if (isGiven(LengthInterval) | isGiven(StratificationColumns)){
      stop("The arguments 'LengthInterval' and 'StratificationColumns' should not be provided in combination with DefinitionMethod SRS.")
    }
  }
  
  if (DefinitionMethod == "LengthStratified"){
    if (isGiven(StratificationColumns)){
      stop("The argument 'StratificationColumns' should not be provided in combination with DefinitionMethod 'LengthStratified'.")
    }
    if (!isGiven(LengthInterval)){
      stop("The argument 'LengthInterval' must be provided when DefinitionMethod is 'LengthStratified")
    }
    if (LengthInterval <=0){
      stop("LengthInterval must be a positive value.")
    }
    if ("IndividualTotalLength" %in% Parameters){
      stop("'IndividualTotalLength' may not be among the variables in 'Parameters' for length-stratified sampling.")
    }
    if ("LengthStratum" %in% Parameters){
      stop("'LengthStratum' may not be used as a 'Parameter' with DefinitionMethod 'LengthStratified'. Consider renaming or using the DefinitionMethod 'Stratified'")
    }
    if (any(is.na(StoxBioticData$Individual$IndividualTotalLength))){
      missing <- StoxBioticData$Individual$Individual[is.na(StoxBioticData$Individual$IndividualTotalLength)]
      stop(paste("Cannot specify length stratified selection when some individuals are not measured. Missing IndividualTotalLength for:", truncateStringVector(missing)))
    }
    
    lengthGroups <- seq(0,max(StoxBioticData$Individual$IndividualTotalLength)+LengthInterval,LengthInterval)
    StoxBioticData$Individual$LengthStratum <- paste(as.character(cut(StoxBioticData$Individual$IndividualTotalLength, lengthGroups, right=F)), "cm")
    StratificationColumns <- c("LengthStratum")
  }
  
  if (DefinitionMethod == "Stratified"){
    if (!isGiven(StratificationColumns)){
      stop("The argument 'StratificationColumns' must be provided when DefinitionMethod is 'Stratified")
    }
    if (!all(StratificationColumns %in% names(StoxBioticData$Individual))){
      stop("All values for the argument 'StratificationColumns' must be names of columns of the Individual table in 'StoxBioticData'")
    }
    for (st in StratificationColumns){
      if (any(is.na(StoxBioticData$Individual[[st]]))){
        stop("Cannot specify stratified selection when some individuals are not assigned a stratum. Missing values for:", st)
      }
    }
    reserved_names <- c("Stratum", "SampleId")
    if (any(reserved_names %in% StratificationColumns)){
      stop(paste(paste(reserved_names, collapse=","), "are reserved names in IndividualSamplingParametersData and cannot be specified as StratificationColumns"))
    }
  }
  
  params <- extractIndividualDesignParametersStoxBiotic(StoxBioticData, StratificationColumns, Parameters)
  if (CollapseStrata){
    #SpeciesCategory is added to Stratification in extractIndividualDesignParametersStoxBiotic, and is not retained (not in 'StratificationColumns')
    params <- collapseStrataIndividualDesignParamaters(params, StratificationColumns)
  }

  return(params)
  
}

#' Extend IndividualSamplingParametersData to reflect selection from PSU by specifying intermediate selection.
#' Not yet implemented.
#' @param StoxBioticData data records
#' @param IndividualSamplingParametersData sampling parameters for lower level selection
#' @param Hierarchy character vector describing the hierarchy
#' @param Stratification character vector describing the stratification at each level in the hierarchy
#' @param StrataSizes identifies column where strata sizes are provided
#' @param SelectionMethod sampling method for each level in the hierarchy
#' @return \code{\link[RstoxFDA]{IndividualSamplingParametersData}}
#' @concept Analytical estimation
#' @md
#' @export
DefineSamplingHierarchy <- function(StoxBioticData, IndividualSamplingParametersData, Hierarchy=character(), Stratification=character(), StrataSizes=character(), SelectionMetod=character()){
  stop("Not Implemented")
}

#' Assign PSU Sampling Parameters
#' @description 
#'  Assigns data records to PSU Sampling Parameters, provides non-response adjustments for
#'  selected PSUs that was not sampled, and changes SamplingUnitId to that used to identify data records.
#' @details 
#'  Some sampling parameters provided in \code{\link[RstoxFDA]{PSUSamplingParametersData}} are only
#'  interpretable for sampling with complete response. This function adjusts these parameters, removes non-respondents from the 
#'  \code{\link[RstoxFDA]{PSUSamplingParametersData}}, and checks that all responding PSUs are present in data records.
#'  
#'  After correcting for non-response, the SamplingUnitId in \code{\link[RstoxFDA]{PSUSamplingParametersData}} will be replaced
#'  by an ID (argument 'DataRecordId') so that sampling units can be brought into correspondance with how they are identified in lower
#'  level sampling (\code{\link[RstoxFDA]{IndividualSamplingParametersData}})
#'  
#'  If any respondants (rows of the SelectionTable of PSUSamplingParametersData that does not have NA for SamplingUnitId) are not
#'  found in 'SamplingUnitId', execution halts with an error.
#'  
#'  Response after selection can generally be considered a process that modifies the sampling parameters that are set by design.
#'  Typically sample size, InclusionProbabilities and normalized SamplingWeights need to be adjusted as non-respondents are removed, 
#'  since these are depend of the entire sample, not just the sampling unit they are assigned to. 
#'  SelectionProbabilites are by definition set for a single draw of a single sampling unit from the population and are valid even
#'  when response is not complete.
#'  
#'  Treatment of non-response requires some assumption about systematic differences between respondents and non-respondents. 
#'  These assumptions are specified via the argument 'DefinitionMethod' and the following options are available:
#'  \describe{
#'   \item{MissingAtRandom}{A response propensity is estimated for each stratum as the fraction of the sample resonding, and sample size (n) and InclusionProbability are adjusted by multiplying with this propensity. Sampling weights are adjusted by dividing them with their sum over repsondents in a stratum.}
#'  }
#'  
#' @param PSUSamplingParametersData \code{\link[RstoxFDA]{PSUSamplingParametersData}} with sampling parameters for PSU selection
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} with data records for responding PSUs.
#' @param DataRecordId name of Variable in \code{\link[RstoxData]{StoxBioticData}} that represent records of sampled PSUs
#' @param SamplingUnitId name of Variable in \code{\link[RstoxData]{StoxBioticData}} that represent the SamplingUnitId of PSUs selected for sampling
#' @param DefinitionMethod The method for dealing with non-response, e.g. 'MissingAtRandon'
#' @return \code{\link[RstoxFDA]{PSUSamplingParametersData}} without non-respondent and with 'SamplingUnitId' changed
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
#' @export
AssignPSUSamplingParameters <- function(PSUSamplingParametersData, StoxBioticData, SamplingUnitId, DataRecordId, DefinitionMethod=c("MissingAtRandom")){
  checkMandatory(PSUSamplingParametersData, "PSUSamplingParametersData")
  checkMandatory(StoxBioticData, "StoxBioticData")
  checkMandatory(DataRecordId, "DataRecordId")
  checkMandatory(SamplingUnitId, "SamplingUnitId")
  checkOptions(DefinitionMethod, "DefinitionMethod", c("MissingAtRandom"))
  
  level <- NULL
  for (l in names(StoxBioticData)){
    if (SamplingUnitId %in% names(StoxBioticData[[l]])){
      if (!is.null(level)){
        stop("The 'SamplingUnitId' (", SamplingUnitId ,") exists on several levels in StoxBioticData")
      }
      level <- l
    }
  }

  if (is.null(level)){
    stop(paste("The variable provided for SamplingUnitId (", SamplingUnitId,") is not a variable in 'StoxBioticData'"), sep="")
  }
    
  if (!(DataRecordId %in% names(StoxBioticData[[level]]))){
    stop(paste("The column provided for 'DataRecordId' (", DataRecordId,") is not found on level '", level, "', where 'SamplingUnitId' (",SamplingUnitId,") is provided", sep=""))
  }

  if (!all(duplicated(StoxBioticData[[level]][[DataRecordId]])==duplicated(StoxBioticData[[level]][[SamplingUnitId]]))){
    stop("DataRecordIds and SamplingUnitIds are not in correspondance with each other in StoxBioticData")    
  }

  records <- PSUSamplingParametersData$SelectionTable$SamplingUnitId[!is.na(PSUSamplingParametersData$SelectionTable$SamplingUnitId)]
  if (!all(records %in% StoxBioticData[[level]][[SamplingUnitId]])){
    missing <- records[!(records %in% StoxBioticData[[l]][[SamplingUnitId]])]
    stop(paste("Records are not found for all sampled PSUs. Missing for the following SamplingUnitIds (", SamplingUnitId,"): ", truncateStringVector(missing), sep=""))
  }
  
  if (DefinitionMethod == "MissingAtRandom"){
    responsePropensity <- PSUSamplingParametersData$SelectionTable[,list(ResponsePropensity=sum(!is.na(SamplingUnitId))/.N), by=c("Stratum")]
    
    PSUSamplingParametersData$SampleTable$n <- PSUSamplingParametersData$SampleTable$n * responsePropensity$ResponsePropensity[match(PSUSamplingParametersData$SampleTable$Stratum, responsePropensity$Stratum)]
    
    # correct sampling probabilities
    PSUSamplingParametersData$SelectionTable$InclusionProbability <- PSUSamplingParametersData$SelectionTable$InclusionProbability * responsePropensity$ResponsePropensity[match(PSUSamplingParametersData$SelectionTable$Stratum, responsePropensity$Stratum)]
    
    #remove non respondants
    PSUSamplingParametersData$SelectionTable <- PSUSamplingParametersData$SelectionTable[!is.na(PSUSamplingParametersData$SelectionTable$SamplingUnitId)]
    
    #correct normalized sampling weights
    weights <- PSUSamplingParametersData$SelectionTable[,list(HHsum=sum(HHsamplingWeight), HTsum=sum(HTsamplingWeight)), by=c("Stratum")]
    PSUSamplingParametersData$SelectionTable$HTsamplingWeight <- PSUSamplingParametersData$SelectionTable$HTsamplingWeight / weights$HTsum[match(PSUSamplingParametersData$SelectionTable$Stratum, weights$Stratum)]
    PSUSamplingParametersData$SelectionTable$HHsamplingWeight <- PSUSamplingParametersData$SelectionTable$HHsamplingWeight / weights$HHsum[match(PSUSamplingParametersData$SelectionTable$Stratum, weights$Stratum)]
    
  }
  
  #recode sampling units id
  map <- StoxBioticData[[level]][!duplicated(get(SamplingUnitId)),.SD,.SDcol=c(SamplingUnitId, DataRecordId)]
  
  PSUSamplingParametersData$SelectionTable$SamplingUnitId <- map[[DataRecordId]][match(PSUSamplingParametersData$SelectionTable$SamplingUnitId, map[[SamplingUnitId]])]
  
  return(PSUSamplingParametersData)
}

#' Estimate parameters for each Primary Sampling Unit
#' @description 
#'  Estimate abundance, frequencies, totals and means for each Primary Sampling Unit (PSU)
#'  in a multi-stage sampling design, by strata and domains.
#' @details 
#'  Provides estimates of abundance, frequencies, totals and means by a Horvitz-Thompson estimator.
#'  Abundance and totals are only provided if inclusion probabilities are known, while frequencies and means may be calculated
#'  with only sampling weights. See \code{\link[RstoxFDA]{IndividualSamplingParametersData}}.
#'  
#'  Results may be combined into population level estimates with \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}. For this
#'  reason it is also possible to provide groups of PSUs to be annotated on the output (the argument 'PSUDomainVariables').
#'  PSU domains has no effect on estimation, but are merely annotated on the results for further processing or reporting.
#'  
#'  Sampling parameters for the selection of individuals from a catch can be inferred for some common sub-sampling techniques
#'  with the function \code{\link[RstoxFDA]{DefineIndividualSamplingParameters}}. If samples of Individuals are not directly sampled from each
#'  PSU, any intermediate sampling levels can be incorporated with the function \code{\link[RstoxFDA]{DefineSamplingHierarchy}}
#' 
#'  If any strata are specified in the SampleTable of 'IndividualSamplingParametersData' but are not sampled per the SelectionTable
#'  all estimates will be provided as NAs for this stratum.
#'  
#'  Domains and that are not present in a sample are not reported, although their estimated abundance and total is zero. Use the function
#'  \code{\link[RstoxFDA]{LiftStrata}} to infer zero values for unreported domains, and mark unsampled strata as NA.
#'  
#'  In general unbiased estimates rely on known inclusion probabilites, and domain definitions that coincides
#'  with stratification. When the domain definitions are not aligned
#'  with the stratification, ratio estimates are provided for which unbiasedness is not guaranteed.
#'  
#'  Abundances, frequencies, totals, and means are estimated with the formulas below. A vocabulary of notation is provided after the equations.
#'  \describe{
#'   \item{Abundance:}{
#'   The estimate of the number of individuals in stratum \eqn{s} and domain \eqn{d} at a PSU:
#'   \deqn{\hat{N}^{(s,d)}=\sum_{i=1}^{m}\frac{1}{\pi_{i}}I^{s,d}_{i}}
#'   The inclusion probability is a function of the entire sample selection for a stratum.
#'   If the domain does not coincide with stratum, it must be considered approximate and hence this will be a ratio estimation in that case.
#'   }
#'   \item{Frequency:}{
#'   The estimate of the fraction of individuals in stratum \eqn{s} that are in domain \eqn{d} at a PSU:
#'   \deqn{\hat{f}^{(s,d)}=\sum_{i=1}^{m}w_{i}I^{s,d}_{i}}
#'   }
#'   The sampling weight is a function of the entire sample selection for a stratum.
#'   If the domain does not coincide with stratum, it must be considered approximate and hence this will be a ratio estimation in that case.
#'   \item{Total:}{
#'   The estimate of the total of a variable in stratum \eqn{s} and domain \eqn{d} at a PSU:
#'   \deqn{\hat{t}^{(s,d)}=\sum_{i=1}^{m}\frac{y_{i}}{\pi_{i}}I^{s,d}_{i}}
#'   }
#'   The inclusion probability is a function of the entire sample selection for a stratum.
#'   If the domain does not coincide with stratum, it must be considered approximate and hence this will be a ratio estimation in that case.
#'   \item{Mean:}{
#'   The mean value of a variable in stratum \eqn{s} and domain \eqn{d} at a PSU:
#'   \deqn{\hat{\mu}^{(s,d)}=\frac{1}{\hat{D}^{(s,d)}}\sum_{i=1}^{m}w_{i}y_{i}I^{s,d}_{i}}
#'   }
#'   This depends explicitly on the ratio to the estimate of relative domain size. When the domain coincides with strata
#'   this is in principle known, but in practice reported strata sizes for samples of individuals are often estimated, bringing
#'   into question the exact computation of inclusion probabilities.
#'   In addition, the sampling weight is a function of the entire sample selection for a stratum.
#'   If the domain does not coincide with stratum, it must also be considered approximate and an additional ratio-estimated quantity.
#'  }
#'  \describe{
#'    \item{\eqn{I^{(s,d)}_{i}}}{The indicator function for domain \eqn{d} and stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s} and domain \eqn{d}, otherwise it is zero.}
#'    \item{\eqn{m}}{The total number of individuals sampled at PSU.}
#'    \item{\eqn{\pi_{i}}}{The inclusion probability of individual \eqn{i} in PSU.}
#'    \item{\eqn{w_{i}}}{The normalized Horvitz-Thompson sample weight of an individual \eqn{i}.}
#'    \item{\eqn{y_{i}}}{The value of a variable observed for an individual \eqn{i}.}
#'    \item{\eqn{\hat{D}^{(s,d)}}}{The estimated relative domain size of domain \eqn{d} in stratum \eqn{s} at PSU: \eqn{\sum_{i=1}^{m}w_{i}I^{s,d}_{i}}}
#'  }
#'  
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} with the actual observations of individuals.
#' @param IndividualSamplingParametersData \code{\link[RstoxFDA]{IndividualSamplingParametersData}} with sampling parameters for individuals
#' @param Variables names of variables that means and totals should be estimated for. Must be columns of the Individual table in 'StoxBioticData'
#' @param DomainVariables names of variables that define domains of individuals that estimates should be made for. Must be columns of 'Individual' or some higher level table in 'StoxBioticData'.
#' @param PSUDomainVariables names of variables that define groups of PSUs to be annotated on the results for later processing. Must be columns of 'Individual' or some higher level table in 'StoxBioticData', and must have a unique value for each PSU.
#' @return \code{\link[RstoxFDA]{AnalyticalPSUEstimate}} with estimates for each PSU of abundance, frequencies, totals and means by stratum and domain.
#' @concept Analytical estimation
#' @md
#' @export
AnalyticalPSUEstimate <- function(StoxBioticData, IndividualSamplingParametersData, Variables=character(), DomainVariables=character(), PSUDomainVariables=character()){

  checkMandatory(StoxBioticData, "StoxBioticData")
  checkMandatory(IndividualSamplingParametersData, "IndividualSamplingParametersData")
  
  ind <- RstoxData::MergeStoxBiotic(StoxBioticData, "Individual")
  ind <- ind[ind$Individual %in% IndividualSamplingParametersData$SelectionTable$IndividualId,]

  reservedNames <- c("PSUDomain", "SampleId", "Individual")
  namingConflicts <- PSUDomainVariables[PSUDomainVariables %in% reservedNames]
  if (length(namingConflicts)>0){
    stop("The PSU domain variables", paste(paste(namingConflicts, collapse=","), " are specified. The following variable names cannot be used for variables  in domain definitions:", paste(reservedNames, collapse=",")))
  }
  
  missing <- PSUDomainVariables[!(PSUDomainVariables %in% names(ind))]
  if (length(missing)>0){
    stop(paste("All PSUDomainVariables must be columns in StoxBioticData. The following are not valid:", truncateStringVector(missing)))
  }
    
  PSUdomains <- ind[,.SD, .SDcol=c("Individual", PSUDomainVariables)]
  PSUdomains$PSUDomain <- apply(PSUdomains[,.SD,.SDcol=PSUDomainVariables], FUN=paste, MARGIN = 1, collapse="/")
  PSUdomains <- merge(PSUdomains, IndividualSamplingParametersData$SelectionTable[,.SD,.SDcol=c("SampleId", "IndividualId")], by.x="Individual", by.y="IndividualId")
  for (dom in PSUDomainVariables){
    uniqueness <- PSUdomains[,list(total=length(unique(get(dom)))), by="SampleId"]
    nonunique <- uniqueness$SampleId[uniqueness$total!=1]
    if (length(nonunique)>0){
      stop("PSUDomainVariables must be unique to each PSU. Duplicates found for ", dom, "for PSUs:", truncateStringVector(nonunique))
    }
  }
  PSUdomains <- PSUdomains[!duplicated(PSUdomains$SampleId),.SD,.SDcol=c("SampleId", "PSUDomain", PSUDomainVariables)]
  if (length(PSUDomainVariables)==0){
    PSUdomains$PSUDomain <- "All"
  }

  reservedNames <- c("Stratum", "Domain", "SampleId")
  namingConflicts <- DomainVariables[DomainVariables %in% reservedNames]
  if (length(namingConflicts)>0){
    stop("The domain variables", paste(paste(namingConflicts, collapse=","), " are specified. The following variable names cannot be used for variables  in domain definitions:", paste(reservedNames, collapse=",")))
  }

  reservedNames <- c(names(IndividualSamplingParametersData$SelectionTable), "Domain")
  namingConflicts <- Variables[Variables %in% reservedNames]
  if (length(namingConflicts)>0){
    stop("The variables ", paste(paste(namingConflicts, collapse=","), " are specified. The following variable names cannot be used for variables that estimates should be provided for:", paste(reservedNames, collapse=",")))
  }
  
  missingDomainIds <- DomainVariables[!(DomainVariables %in% names(ind))]
  if (length(missingDomainIds)>0){
    stop(paste("Invalid speficiation of domain variables. The following variables does not exist in StoxBioticData:", paste(missingDomainIds, collapse=",")))
  }
  
  missingvariables <- Variables[!(Variables %in% names(StoxBioticData$Individual))]
  if (length(missingvariables)>0){
    stop(paste("Invalid speficiation of variables. The following variables does not exist on the Individual table in StoxBioticData:", paste(missingvariables, collapse=",")))
  }

  unsampled <- IndividualSamplingParametersData$SampleTable[!(paste(IndividualSamplingParametersData$SampleTable$SampleId, IndividualSamplingParametersData$SampleTable$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum)),]
  if (nrow(unsampled)>0){
    warning(paste("Not all strata are sampled. Estimates will not be provided for some strata for SampleIds: ", truncateStringVector(unique(unsampled$SampleId))))
  }
  
  #put Domain in ind
  ind$Domain <- "All"
  if (length(DomainVariables)>0){
    ind$Domain <- apply(ind[,.SD, .SDcol=DomainVariables], FUN=paste, 1, collapse="/")
  }
  
  domaintable <- ind[,.SD,.SDcol=c("Domain", DomainVariables)]
  domaintable <- domaintable[!duplicated(ind$Domain),]
    
  ind <- ind[,.SD,.SDcol=c("Individual", "Domain", Variables)]
  ind <- merge(ind, IndividualSamplingParametersData$SelectionTable, by.x=c("Individual"), by.y=c("IndividualId"))
  
  abundance <- ind[,list(Abundance=sum(1/InclusionProbability), Frequency=sum(HTsamplingWeight)), by=c("SampleId", "Stratum", "Domain")]

  #add zero domains
  allDomains <- data.table::CJ(SampleId=IndividualSamplingParametersData$SampleTable$SampleId, Domain=domaintable$Domain, unique = T)
  allDomains <- merge(allDomains, IndividualSamplingParametersData$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum")], by="SampleId", allow.cartesian=T)
  missingDomains <- allDomains[!(paste(SampleId, Stratum, Domain) %in% paste(abundance$SampleId, abundance$Stratum, abundance$Domain)),]
  missingDomains$Abundance <- 0
  missingDomains$Frequency <- 0
  #add NAs for non-sampled strata
  missingDomains$Abundance[!(paste(missingDomains$SampleId, missingDomains$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomains$Frequency[!(paste(missingDomains$SampleId, missingDomains$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomains <- missingDomains[,.SD,.SDcol=names(abundance)]
  abundance <- rbind(abundance, missingDomains)
  
  estimates <- NULL
  for (v in Variables){
    est <- ind[,list(Variable=v, Total=sum(get(v)/InclusionProbability), Mean=sum(get(v)*HTsamplingWeight)/sum(HTsamplingWeight)), by=c("SampleId", "Stratum", "Domain")]

    est$Variable <- v
    estimates <- rbind(estimates, est)
  }

  #add zero domains
  allDomains <- data.table::CJ(SampleId=IndividualSamplingParametersData$SampleTable$SampleId, Domain=domaintable$Domain, Variable=estimates$Variable, unique = T)
  allDomains <- merge(allDomains, IndividualSamplingParametersData$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum")], by="SampleId", allow.cartesian=T)
  missingDomains <- allDomains[!(paste(SampleId, Stratum, Domain, Variable) %in% paste(estimates$SampleId, estimates$Stratum, estimates$Domain, estimates$Variable)),]
  missingDomains$Total <- 0
  missingDomains$Mean <- NaN
  #add NA for non-sampled strata
  missingDomains$Total[!(paste(missingDomains$SampleId, missingDomains$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomains$Mean[!(paste(missingDomains$SampleId, missingDomains$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomains <- missingDomains[,.SD,.SDcol=names(estimates)]
  estimates <- rbind(estimates, missingDomains)
  
  output <- list()
  output$Abundance <- abundance[order(SampleId, Stratum, Domain),]
  output$Variables <- estimates[order(SampleId, Stratum, Domain, Variable),]
  output$DomainVariables <- domaintable[order(Domain),]
  output$PSUDomainVariables <- PSUdomains
  output$StratificationVariables <- IndividualSamplingParametersData$StratificationVariables[order(SampleId, Stratum),]

  return(output)
}

#' Collapse strata in sampling design
#' @description
#'  Transforms a sampling design to an equivalent one with simpler stratification
#' @details 
#'  The sampling information in \code{\link[RstoxFDA]{IndividualDesignParamatersData}} allows for specification of several stratification variables.
#'  This function facilitates removal of some of these stratificaiton vairables, redefining strata to a coarser stratification scheme that still has known
#'  stratum sizes.
#'  
#'  If all stratification variables are removed, all samples will be assigned to a single stratum named 'All'.
#'  
#' @param IndividualDesignParamatersData \code{\link[RstoxFDA]{IndividualDesignParamatersData}} with sampling parameters for sample selection
#' @param RetainStrata character() with the names of stratification variables to retain. Stratification variables not specified here will be removed.
#' @return \code{\link[RstoxFDA]{IndividualDesignParamatersData}} with simplified stratification
#' @concept Analytical estimation
#' @md
#' @export
CollapseStrata <- function(IndividualDesignParamatersData, RetainStrata=character()){
  
  checkMandatory(IndividualDesignParamatersData, "IndividualDesignParamatersData")
  
  missing <- RetainStrata[!(RetainStrata %in% names(IndividualDesignParamatersData$StratificationVariables))]
  if (length(missing)>0){
    stop(paste("The variables", paste(missing, collapse=", ", "are not stratification variables in 'IndividualDesignParametersData")))
  }
  
  collapseStrata <- names(IndividualDesignParamatersData$StratificationVariables)[!names(IndividualDesignParamatersData$StratificationVariables) %in% c(RetainStrata, "Stratum", "SampleId")]
  return(collapseStrataIndividualDesignParamaters(IndividualDesignParamatersData, collapseVariables = collapseStrata))
}

#' Unify strata for AnalyticalPSUEstimateData
#' @description 
#'  Populates unreported strata with zeroes and NaNs.
#' @details
#'  AnalyticalPSUEstimateData may be provided with stratification for each Primary Sampling Unit. These estimates cannot be combined into population level estimates
#'  unless the same strata are provided for all Primary Sampling Units (PSUs). See \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}. This may not be the case for two reasons. The stratification may be local to each PSU,
#'  and no common stratification exists between PSUs. For instance, stratified sub-sampling of a catch may have taken place where the criteria for 
#'  stratification was only used for that particular catch. Otherwise, some strata may not be estimated for some samples, simply because they did not exist there.
#'  For instance stratification by length groups can use a common length stratification scheme for all PSUs, but not all of these strata will exist for each catch.
#' 
#'  If strata are omitted because they do not exist for a particular Sample, Abundance, Frequencies and Totals can be inferred to be zero, while Means are undefined.
#'  Application of this functions introduces zeros (Abundance, Frequency and Total) and NaN (Means) for all missing strata for all samples in 'AnalyticalPSUEstimateData'
#' 
#'  In the case where strata definitions are local, the stratification are only useful for the calculation of sampling parameters. If this is the case
#'  consider using the function \code{\link[RstoxFDA]{CollapseStrata}} before producing estimates for each sample.
#' @param AnalyticalPSUEstimateData \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}} with estimates for all strata for each PSU that has positive abundance.
#' @return \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}} with zeroes and NaNs inferred for strata that have zero abundance in a PSU.
#' @concept Analytical estimation
#' @md
#' @export
LiftStrata <- function(AnalyticalPSUEstimateData){

  allStrata <- data.table::CJ(SampleId=AnalyticalPSUEstimateData$StratificationVariables$SampleId, Stratum=AnalyticalPSUEstimateData$StratificationVariables$Stratum, unique = T)
  missingStrata <- allStrata[!(paste(SampleId, Stratum) %in% paste(AnalyticalPSUEstimateData$StratificationVariables$SampleId, AnalyticalPSUEstimateData$StratificationVariables$Stratum)),]
  
  domains <- data.table::CJ(Stratum=AnalyticalPSUEstimateData$StratificationVariables$Stratum, Domain=AnalyticalPSUEstimateData$DomainVariables$Domain, unique = T)
  missingStrataWdomains <- merge(missingStrata, domains, by="Stratum", allow.cartesian = T)
  
  AbundanceAdditions <- missingStrataWdomains
  AbundanceAdditions$Abundance <- 0
  AbundanceAdditions$Frequency <- 0
  AbundanceAdditions <- AbundanceAdditions[,.SD,.SDcol=names(AnalyticalPSUEstimateData$Abundance)]
  
  variables <- data.table::CJ(Domain=AnalyticalPSUEstimateData$DomainVariables$Domain, Variable=AnalyticalPSUEstimateData$Variables$Variable, unique = T)
  missingStrataWdomainsAndVariables <- merge(missingStrataWdomains, variables, by="Domain", allow.cartesian = T)
  
  VariableAdditions <- missingStrataWdomainsAndVariables
  VariableAdditions$Total <- 0
  VariableAdditions$Mean <- NaN
  VariableAdditions <- VariableAdditions[,.SD,.SDcol=names(AnalyticalPSUEstimateData$Variables)]
  
  stratvars <- AnalyticalPSUEstimateData$StratificationVariables[!duplicated(Stratum),.SD,.SDcol=names(AnalyticalPSUEstimateData$StratificationVariables)[names(AnalyticalPSUEstimateData$StratificationVariables)!="SampleId"]]
  StratVarAdditions <- merge(missingStrata, stratvars, by="Stratum")
  StratVarAdditions <- StratVarAdditions[,.SD,.SDcol=names(AnalyticalPSUEstimateData$StratificationVariables)]
  
  AnalyticalPSUEstimateData$Abundance <- rbind(AnalyticalPSUEstimateData$Abundance, AbundanceAdditions)[order(SampleId, Stratum, Domain)]
  AnalyticalPSUEstimateData$Variables <- rbind(AnalyticalPSUEstimateData$Variables, VariableAdditions)[order(SampleId, Stratum, Domain)]
  AnalyticalPSUEstimateData$StratificationVariables <- rbind(AnalyticalPSUEstimateData$StratificationVariables, StratVarAdditions)[order(SampleId, Stratum)]
  
  return(AnalyticalPSUEstimateData)
}

covarAbundance <- function(Totals, PSUSampling, MeanOfMeans){
  
  sampleSize <- PSUSampling[,list(n=length(unique(SamplingUnitId))), by="Stratum"]
  
  covars <- NULL
  for (PSUstrat in unique(PSUSampling$Stratum.PSU)){
    stratatable <- merge(PSUSampling[PSUSampling$Stratum.PSU==PSUstrat,], Totals, by=c("Stratum", "Domain"), suffixes=c(".PSU", ".Total"))

    for (PSUDom in unique(stratatable$PSUDomain)){
      table <- stratatable[stratatable$PSUDomain == PSUDom,]

      relPSUDomainSize <- sum(table$HHsamplingWeight[!duplicated(table$SamplingUnitId)])
      relDomainSizes <- table[,list(relDomainSize=sum(HHsamplingWeight[!duplicated(SamplingUnitId)])), by=c("Stratum", "Domain")]
      stopifnot(relPSUDomainSize<=1+1e-1)
      stopifnot(all(relDomainSizes$relDomainSize <= relPSUDomainSize))

      table <- merge(table, relDomainSizes, by=c("Stratum", "Domain"))
      
      table$AbundanceDev <- table$relDomainSize*table$Abundance.PSU/table$SelectionProbability - table$Abundance.Total
      table$FrequencyDev <- table$Frequency.PSU - table$Frequency.Total
      table <- table[,.SD,.SDcol=c("Stratum", "Domain", "SamplingUnitId", "AbundanceDev", "FrequencyDev")]

      cross <- data.table::CJ(Domain1=unique(table$Domain), Domain2=unique(table$Domain))
      cross <- cross[cross$Domain1>=cross$Domain2,]
      cross <- merge(cross, table, by.x=c("Domain1"), by.y=c("Domain"), allow.cartesian = T)
      cross <- merge(cross, table, by.x=c("SamplingUnitId", "Stratum", "Domain2"), by.y=c("SamplingUnitId", "Stratum", "Domain"), suffixes = c("1", "2"))
      cross$AbundanceDevProduct <- cross$AbundanceDev1 * cross$AbundanceDev2
      cross$FrequencyDevProduct <- cross$FrequencyDev1 * cross$FrequencyDev2
      
      sumOfProducts <- cross[,list(AbundanceSOP=sum(AbundanceDevProduct), FrequencySOP=sum(FrequencyDevProduct)), by=c("Stratum", "Domain1", "Domain2")]
      sumOfProducts <- merge(sumOfProducts, sampleSize, by="Stratum")
      covar <- sumOfProducts[,list(AbundanceCovariance=AbundanceSOP*(1/(n*(n-1)))*(1/relPSUDomainSize**2), FrequencyCovariance=FrequencySOP*(1/(n*(n-1)))*(1/relPSUDomainSize**2)), by=c("Stratum", "Domain1", "Domain2")]
      
      if (!MeanOfMeans){
        StrataAbundance <- Totals[,list(StrataAbundance=sum(Abundance)), by="Stratum"]
        covar$FrequencyCovariance <- covar$AbundanceCovariance * (1/StrataAbundance$StrataAbundance[match(covar$Stratum, StrataAbundance$Stratum)])**2
      }
      
      covars <- rbind(covars, covar)    
    }
    
  }
  
  return(covars)
}

covarVariables <- function(Totals, PSUSampling, MeanOfMeans, Abundance){
  
  
  sampleSize <- PSUSampling[,list(n=length(unique(SamplingUnitId))), by="Stratum"]
  
  covars <- NULL
  for (PSUstrat in unique(PSUSampling$Stratum.PSU)){
    strataTable <- merge(PSUSampling[PSUSampling$Stratum.PSU==PSUstrat,], Totals, by=c("Stratum", "Domain", "Variable"), suffixes=c(".PSU", ".Total"))
    
    for (PSUDom in unique(strataTable$PSUDomain)){
      table <- strataTable[strataTable$PSUDomain == PSUDom,]
      
      relPSUDomainSize <- sum(table$HHsamplingWeight[!duplicated(table$SamplingUnitId)])
      relDomainSizes <- table[,list(relDomainSize=sum(HHsamplingWeight[!duplicated(SamplingUnitId)])), by=c("Stratum", "Domain")]
      stopifnot(relPSUDomainSize<=1+1e-1)
      stopifnot(all(relDomainSizes$relDomainSize <= relPSUDomainSize))
      
      table <- merge(table, relDomainSizes, by=c("Stratum", "Domain"))
      
      table$TotalDev <- table$relDomainSize*table$Total.PSU/table$SelectionProbability - table$Total.Total
      table$MeanDev <- table$Mean.PSU - table$Mean.Total
      table <- table[,.SD,.SDcol=c("Stratum", "Domain", "SamplingUnitId", "Variable", "TotalDev", "MeanDev", "HHsamplingWeight", "Total.PSU", "SelectionProbability")]
      
      cross <- data.table::CJ(Domain1=unique(Totals$Domain), Variable1=unique(Totals$Variable), Domain2=unique(Totals$Domain), Variable2=unique(Totals$Variable))
      cross <- cross[cross$Domain1>=cross$Domain2 & cross$Variable1 >= cross$Variable2]
      cross <- merge(cross, table, by.x=c("Domain1", "Variable1"), by.y=c("Domain", "Variable"), allow.cartesian = T)
      cross <- merge(cross, table, by.x=c("SamplingUnitId", "Stratum", "Domain2", "Variable2"), by.y=c("SamplingUnitId", "Stratum", "Domain", "Variable"), suffixes = c("1", "2"))
      cross$TotalDevProduct <- cross$TotalDev1 * cross$TotalDev2
      cross$MeanDevProduct <- cross$MeanDev1 * cross$MeanDev2
      cross$coSampled <- as.numeric(!is.na(cross$MeanDev1)) * as.numeric(!is.na(cross$MeanDev2))
      
      sumOfProducts <- cross[,list(TotalSOP=sum(TotalDevProduct), 
                                   MeanSOP=sum(MeanDevProduct[coSampled==1]), 
                                   Freq1=sum(HHsamplingWeight1[coSampled==1]), 
                                   Freq2=sum(HHsamplingWeight2[coSampled==1])), 
                             by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]
      sumOfProducts <- merge(sumOfProducts, sampleSize, by="Stratum")
      
      covar <- sumOfProducts[,list(TotalCovariance=TotalSOP*(1/(n*(n-1)))*(1/relPSUDomainSize**2), 
                                   MeanCovariance=(1/Freq1**2)*MeanSOP/(n*(n-1))), 
                             by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]
      
      if (!MeanOfMeans){
        m1 <- match(paste(covar$Stratum, covar$Domain1), paste(Abundance$Stratum, Abundance$Domain))
        m2 <- match(paste(covar$Stratum, covar$Domain2), paste(Abundance$Stratum, Abundance$Domain))
        covar$MeanCovariance <- covar$TotalCovariance * (1/Abundance$Abundance[m1]) * (1/Abundance$Abundance[m2])
      }

      
      covars <- rbind(covars, covar)    
    }
    
  }
  
  return(covars)
  
  
}

#' Analytical estimate of population parameters
#' @description 
#'  Provides analytical estimates of population parameters and their co-variances from a multi-stage sampling design.
#'  Estimates and co-variance estimates are provided for abundance, frequencies, totals, and means, by domain and strata.
#' @details 
#'  Estimates are provided with the Hansen-Hurwitz estimator, given estimates for each Primary Sampling Unit (PSU), 
#'  and the design parameters for the PSU selection. See \code{\link[RstoxFDA]{AnalyticalPSUEstimate}} for how to obtain estimates for each PSU. 
#'  
#'  Estimation of totals and abundance require selection probabilites to be known. In cases where only sampling weights are known,
#'  the option 'MeanOfMeans' provides estimation of means and frequencies, but not abundance and totals. These will be set to NA in such cases.
#'  
#'  Means may be unknown for a certain domain for two reasons. They estimate of abundance and frequencies for the domain may be zero.
#'  In this case unknown means are provded as NaNs, as they result from dividing by zero. 
#'  If either PSU means sampling parameters are unkown, or a the strata is not sampled, means will be provided as NA.
#'  
#'  The stratification incorporates both stratified estimates for each PSU and any stratified selection of PSUs. 
#'  E.g. each PSU may provide estimates by length strata, and at the same time the selection of PSUs may be stratified by area.
#'  In that case this function would return estimates for each combination of length stratum and area.
#'  This requires all PSUs to provide estimates for the same strata, and the functions halts with error if that is not the case.
#'  See \code{\link[RstoxFDA]{LiftStrata}} for a way to infer PSU-estimates for strata that have zero abundance.
#'  If simpler stratification is desired, see \code{\link[RstoxFDA]{CollapseStrata}}.
#'  
#'  The domains will be the combination of the domain the PSU belongs to (PSU-domains), and
#'  the domains defined in estimation for later sampling stages and encoded in 'AnalyticalPSUEstimateData'.
#'  Consider the arguments to \code{\link[RstoxFDA]{AnalyticalPSUEstimate}} if other 
#'  later stage domains are desired. For variance estimation, it will sometimes be necessary
#'  to distinguish the domain size of the PSU-domains. In these cases we will refer to the
#'  PSU-domain of a domain. For example, catches may be PSUs and a domain will be defined as 'age 5 fish caught with
#'  purse seine'. The gear (Purse seine) is a property of the catch and is naturally specified as a 
#'  PSU-domain, so the PSU-domain of "age 5 fish caught with purse seine" is "purse seine". The domain will be
#'  the total number of PSUs in the population that caught age 5 fish with purse seine, while the domain size of
#'  the PSU domain will be simply all purse seine catches in the population. Hence domain sizes are always smaller
#'  than their corresponding PSU-domain sizes.
#'  
#'  In general unbiased estimates rely on known selection probabilites, and domain definitions that coincides
#'  with stratification. When only sampling weights are known, or the domain definitions are not aligned
#'  with the stratification, ratio estimates are provided for which unbiasedness is not guaranteed.
#'  
#'  Only between-PSU variance is accounted for, ignoring the variance contribution from the later sampling stages.
#'  This provides and unbiased estimate of the co-variances when PSUs are selected completely independently.
#'  This is the case for Poission sampling or sampling with replacement. It is also approximately true for
#'  regular sampling without replacement when the sampling intensity is low (Only a small fraction of the population PSUs are sampled).
#'  
#'  For the quantities abundance, frequency of individuals, and the total and mean of variables, an estimate \eqn{\hat{x}} and an estimate of sampling co-variances \eqn{\widehat{CoVar}(\hat{x},\hat{y})}
#'  is provided. Variances may be extracted as \eqn{\widehat{Var}(\hat{x})=\widehat{CoVar}(\hat{x},\hat{x})}.
#'  Some other useful error statistics may be derived, such as standard error estimates as \eqn{\widehat{SE}(\hat{x})=\sqrt{\widehat{Var}(\hat{x})}} and the coefficient of variation as \eqn{CV(\hat{x})=\frac{\widehat{SE}(\hat{x})}{\hat{x}}}.
#'  All estimates are normally distributed for large enough sample sizes (asymptotically). When sample size is large enough,
#'  confidence intervals for estimates can be deduced from a normal distribution with \eqn{\mu=\hat{x}} and \eqn{\sigma=\hat{SE}({\hat{x}})}.
#'  
#'  Abundances, frequencies, totals, and means are estimated with the formulas below. A vocabulary of notation is provided after the equations.
#'  
#'  \describe{
#'   \item{Abundance:}{
#'   The estimate of the total number of individuals in domain \eqn{d} and stratum \eqn{s}:
#'   \deqn{\hat{N}^{(s,d)} = \frac{1}{n^{(s)}} \sum_{i=1}^{n} \hat{D}^{(s,d)}\frac{\hat{N}^{(s,d)}_{i}}{p_{i}}I^{(s,d)}_{i}} 
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})}) = \frac{1}{\hat{P}^{(s,d_{1})}\hat{P}^{(s,d_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)} \sum_{i=1}^{n} 
#'   I^{(s)}_{i} J^{(s,d_{1})}_{i} J^{(s,d_{2})}_{i} (\hat{D}^{(s,d)}\frac{\hat{N}^{(s,d_{1})}_{i}}{p_{i}}I^{(s,d_{1})}_{i} - \hat{N}^{(s,d_{1})}) (\hat{D}^{(s,d)}\frac{\hat{N}^{(s,d_{2})}_{i}}{p_{i}}I^{(s,d_{2})}_{i} - \hat{N}^{(s,d_{2})})}
#'   
#'   Note that unless \eqn{\hat{P}^{(s,d_{1})}=\hat{P}^{(s,d_{2})}} the covariance is zero.
#'   
#'   In the general case \eqn{\hat{D}^{(s,d)}} and \eqn{\hat{P}^{(s,d)}} is estimated by a ratio estimator, and the error those estimates are ignored. When \eqn{d} covers all of \eqn{s}, \eqn{\hat{D}^{(s,d)}=1} and \eqn{\hat{P}^{(s,d)}=1} is known. 
#'   In this case both expressions can be shown to be unbiased. In addition the estimate may depend on any ratio estimation for \eqn{\hat{N}^{(s,d)}}. See \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}. 
#'   
#'   These quantities can only be calculated when \eqn{p_{i}} is provided, and will otherwise be NA.}
#'   
#'   \item{Frequency:}{
#'   The estimate of the fraction of individuals in stratum \eqn{s} that are in domain \eqn{d}, when MeanOfMeans is false:
#'   \deqn{ \hat{f}^{(s,d)} = \frac{\hat{N}^{(s,d)}}{\hat{N}^{(s)}} }
#'   with co-variance:
#'   \deqn{ \widehat{CoVar}(\hat{f}^{(s,d_{1})}, \hat{f}^{(s,d_{2})}) = \frac{1}{(\hat{N}^{(s)})^{2}}\widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})})}
#'   
#'   These are ratio estimates depending on the ratio to the estimate value \eqn{\hat{N}^{(s)}}, and the error in this estimate is ignored.
#'   In addition, the estimate may depend on a ratio estimate for \eqn{\hat{N}^{(s,d)}} and \eqn{\hat{N}^{(s,d)}_{i}}, as explained for 'Abundance'
#'   and in \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}.
#'   }
#'   
#'   \item{Frequency, Mean of Means:}{
#'   The estimate of the fraction of individuals in stratum \eqn{s} that are in domain \eqn{d}, when MeanOfMeans is true:
#'   \deqn{ \hat{f}^{(s,d)} =  \sum_{i=1}^{n}\frac{w_{i}}{\hat{D}^{(s,d)}}\hat{f}^{(s,d)}_{i}I^{(s,d)}_{i}}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{f}^{(s,d_{1})}, \hat{f}^{(s,d_{2})}) = \frac{1}{\hat{P}^{(s,d_{1})}\hat{P}^{(s,d_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)} \sum_{i=1}^{n} 
#'   I^{(s)}_{i} J^{(s,d_{1})}_{i} J^{(s,d_{2})}_{i} (\hat{f}_{i}I^{(s,d_{1})}_{i} - \hat{f}^{(s,d_{1})}) (\hat{f}_{i}I^{(s,d_{2})}_{i} - \hat{f}^{(s,d_{2})})}.}
#'   
#'   Note that unless \eqn{\hat{P}^{(s,d_{1})}=\hat{P}^{(s,d_{2})}} the covariance is zero.
#'   
#'   These are ratio estimates depending on the ratio ratio estimation of \eqn{\hat{f}^{(s,d)}}, \eqn{\hat{f}^{(s,d)}_{i}}, \eqn{\hat{D}^{(s,d)}}, and \eqn{\hat{P}^{(s,d)}}, and the error in these estimates are ignored.
#'   See comments above (\eqn{\hat{f}^{(s,d)}}) and in \code{\link[RstoxFDA]{AnalyticalPSUEstimate}} (\eqn{\hat{f}^{(s,d)}_{i}}).
#'   
#'   \item{Total:}{
#'   The estimate of the total value of some variable in domain \eqn{d} and stratum \eqn{s}.
#'   \deqn{\hat{t}^{(s,d)}=\frac{1}{n^{(s,d)}}{\sum_{i=1}^{n}}\hat{D}^{(s,d)}\frac{\hat{t}^{(s,d)}_{i}}{p_{i}}I^{(s,d)}_{i}}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{t}^{(s,d_{1})}, \hat{t}^{(s,d_{2})}) = \frac{1}{\hat{P}^{(s,d_{1})}\hat{P}^{(s,d_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)} \sum_{i=1}^{n} 
#'   I^{(s)}_{i} J^{(s,d_{1})}_{i} J^{(s,d_{2})}_{i}(\hat{D}^{(s,d)}\frac{\hat{t}^{(s,d_{1})}_{i}}{p_{i}}I^{(s,d_{1})}_{i} - \hat{t}^{(s,d_{1})}) (\hat{D}^{(s,d)}\frac{\hat{t}^{(s,d_{2})}_{i}}{p_{i}}I^{(s,d_{2})}_{i} - \hat{t}^{(s,d_{2})})}.
#'   
#'   Note that unless \eqn{\hat{P}^{(s,d_{1})}=\hat{P}^{(s,d_{2})}} the covariance is zero.
#'   
#'   In the general case \eqn{\hat{D}^{(s,d)}} and \eqn{\hat{P}^{(s,d)}} is estimated by a ratio estimator, and the error in these estimates are ignored. When \eqn{d} covers all of \eqn{s}, \eqn{\hat{D}^{(s,d)}=1} and \eqn{\hat{P}^{(s,d)}=1}is known. In this case both expressions can be shown to be unbiased. These quantities can only be calculated when \eqn{p_{i}} is provided, and will otherwise be NA.}
#'
#'   \item{Mean:}{
#'   The estimate of the mean value of some variable in domain \eqn{d} and stratum \eqn{s}, when MeanOfMeans is false:
#'   \deqn{\hat{\mu}^{(s,d)} = \frac{\hat{t}^{(s,d)}}{\hat{N}^{(s,d)}}}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{\mu}^{(s,d_{1})}, \hat{\mu}^{(s,d_{2})}) = \frac{1}{\hat{N}^{(s,d_{1})} \hat{N}^{(s,d_{2})}}\widehat{CoVar}(\hat{t}^{(s,d_{1})}, \hat{t}^{(s,d_{2})})}
#'   These are ratio estimates depending on the ratio to the estimated value \eqn{\hat{N}^{(s,d)}}, and the error in this estimate is ignored.
#'   In addition, the estimate may depend on a ratio estimate for \eqn{\hat{t}^{(s,d)}} and \eqn{\hat{t}^{(s,d)}_{i}}, as explained for 'Abundance'
#'   and in \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}.
#'   }
#'   
#'   \item{Mean, Mean of Means:}{
#'   The estimate of the mean value of some variable in domain \eqn{d} and stratum \eqn{s}, when MeanOfMeans is true:
#'   \deqn{\hat{\mu}^{(s,d)}=\sum_{i=1}^{n}\frac{w_{i}}{\hat{d}^{(s,d)}}\hat{\mu}_{i}I^{(s,d)}_{i}H(\hat{N}^{(s,d)}_{i})}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{\mu}^{(s,d_{1})}, \hat{\mu}^{(s,d_{2})}) = \frac{1}{(\hat{d}^{(s,d_{1} \cap d_{2})})^{2}}\frac{1}{n^{(s)}(n^{(s)}-1)} \sum_{i=1}^{n} 
#'   I^{(s,d_{1})}_{i}I^{(s,d_{2})}_{i} J^{(s,d_{1})}_{i}J^{(s,d_{2})}_{i} H(\hat{f}^{(s,d_{1})}_{i})H(\hat{f}^{(s,d_{2})}_{i})( \hat{\mu}^{(s,d_{1})}_{i} - \hat{\mu}^{(s,d_{1})}) (\hat{\mu}^{(s,d_{2})}_{i} - \hat{\mu}^{(s,d_{2})})}}
#'   These are ratio estimates depending on the ratio ratio estimation of \eqn{\hat{d}^{(s,d)}}, \eqn{\hat{d}^{(s,d_{1} \cap d_{2})}} and \eqn{\hat{\mu}^{(s,d)}_{i}}, and the error in these estimates are ignored.
#'  }
#'  
#'  Vocabulary for notation used above:
#'  \describe{
#'    \item{\eqn{H(x)}}{A step function which is 1 when \eqn{x>0}, otherwise it is zero.}
#'    \item{\eqn{I^{(s)}_{i}}}{The indicator function for stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s}, otherwise it is zero.}
#'    \item{\eqn{I^{(s,d)}_{i}}}{The indicator function for domain \eqn{d} and stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s} and domain \eqn{d}, otherwise it is zero.}
#'    \item{\eqn{J^{(s,d)}_{i}}}{The indicator function for domain \eqn{d} and stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s} and the PSU-domain of domain \eqn{d}, otherwise it is zero.}
#'    \item{\eqn{n}}{Sample size, the number of PSUs sampled.}
#'    \item{\eqn{n^{(s)}}}{Stratum sample size, the number of PSUs sampled in stratum \eqn{s}: \eqn{n_{s}=\sum_{i=1}^{n}I^{(s)}_{i}}.}
#'    \item{\eqn{n^{(s,d)}}}{Domain sample size, the number of PSUs sampled in domain{d} and stratum \eqn{s}: \eqn{n^{(s,d)}=\sum_{i=1}^{n}I^{(s,d)}_{i}}.}
#'    \item{\eqn{p_{i}}}{The selection probability of PSU \eqn{i}. 'SelectionProbability' in \code{\link[RstoxFDA]{PSUSamplingParametersData}}.}
#'    \item{\eqn{w_{i}}}{The normalized Hansen-Hurwitz sampling weight: \eqn{w_{i}=\frac{1}{p_{i}Q_{i}}}, \eqn{Q_{i}=\sum_{j=1}^{n}\frac{I^{(s(i))}_{j}}{p_{j}}}, where \eqn{s(i)} denote the strata of sample \eqn{i}. 'HHsamplingWeight' in \code{\link[RstoxFDA]{PSUSamplingParametersData}}.}
#'    \item{\eqn{\hat{D}^{(s,d)}}}{The estimated relative domain size (fraction of PSUs) of domain \eqn{d} in stratum \eqn{s}: \eqn{\hat{D}^{(s,d)}=\sum_{i=1}^{n}w_{i}I^{(s,d)}_{i}}.}
#'    \item{\eqn{\hat{P}^{(s,d)}}}{The estimated relative domain size (fraction of PSUs) of the PSU-domain of domain \eqn{d} in stratum \eqn{s}: \eqn{\hat{P}^{(s,d)}=\sum_{i=1}^{n}w_{i}J^{(s,d)}_{i}}.}
#'    \item{\eqn{\hat{d}^{(s,d)}}}{The estimated relative domain size (fraction of PSUs) that has observations (positive abundance or frequency) in domain \eqn{d} in stratum \eqn{s}: \eqn{\hat{d}^{(s,d)}=\sum_{i=1}^{n}w_{i}I^{(s,d)}_{i}H(\hat{f}^{(s,d)}_{i})}.}
#'    \item{\eqn{\hat{d}^{(s,d_{1} \cap d_{2})}}}{The estimated relative domain size (total number of PSUs) that has observations (positive abundance or frequency) in both domain \eqn{d_{1}} and \eqn{d_{2}} in stratum \eqn{s}: \eqn{\hat{d}^{(s,d)}=\sum_{i=1}^{n}w_{i}I^{(s,d_{1})}_{i}I^{(s,d_{2})}_{i}H(\hat{f}^{(s,d_{1})}_{i})H(\hat{f}^{(s,d_{2})}_{j})}.}
#'    \item{\eqn{\hat{N}^{(s)}}}{The estimated abundance in stratum \eqn{s}: \eqn{\hat{N}^{(s)}=\frac{1}{n_{s}}\sum_{i=1}^{n}\frac{\hat{N}_{i}}{p_{i}}I^{(s)}_{i}}.}
#'    \item{\eqn{\hat{N}^{(s)}_{i}}}{The estimated total abundance in stratum \eqn{s} at PSU \eqn{i}. 'Abundance' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'    \item{\eqn{\hat{N}^{(s,d)}_{i}}}{The estimated abundance in domain \eqn{d} and stratum \eqn{s} at PSU \eqn{i}. 'Abundance' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'    \item{\eqn{\hat{f}^{(s,d)}_{i}}}{The estimated frequency in domain \eqn{d} for stratum \eqn{s} at PSU \eqn{i}. 'Frequency' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'    \item{\eqn{\hat{\mu}_{i}}}{The estimated mean in domain \eqn{d} and stratum \eqn{s} at PSU \eqn{i}. 'Mean' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'  }
#' @param PSUSamplingParametersData \code{\link[RstoxFDA]{PSUSamplingParametersData}} with sampling parameters for a sample of Primary Samplig Units.
#' @param AnalyticalPSUEstimateData \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}} with estimates for each of the Primary Sampling Units in PSUSamplingParametersData
#' @param MeanOfMeans logical. Determines which estimators are used for frequencies and means. See details.
#' @return \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with estimated population parameters by stratum and domain.
#' @examples 
#'  PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
#'                                        RstoxFDA::CatchLotterySamplingExample, 
#'                                        RstoxFDA::CatchLotteryExample, 
#'                                        "lotterySerialnumber", "Haul", "MissingAtRandom")
#'  individualSamplingParameters <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, 
#'                                        RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))
#'                                        
#'  psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
#'                                        individualSamplingParameters, 
#'                                        c("IndividualRoundWeight"), c("IndividualAge"))
#'  popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
#'  
#'  #tabulate abundance
#'  abundance <- popEst$Abundance[,list(Abundance=Abundance), by=c("Stratum", "Domain")]
#'  #add SE and CV
#'  abundance <- merge(abundance, popEst$AbundanceCovariance[Domain1==Domain2,
#'                    list(SE=sqrt(AbundanceCovariance)), 
#'                    by=list(Stratum=Stratum, Domain=Domain1)])
#'  abundance$CV <- abundance$SE/abundance$Abundance
#'  
#'  #need to order as numeric. Domain is always a chr:
#'  abundance <- abundance[order(as.numeric(abundance$Domain)),]
#'  abundance
#' @concept Analytical estimation
#' @export
#' @md
AnalyticalPopulationEstimate <- function(PSUSamplingParametersData, AnalyticalPSUEstimateData, MeanOfMeans=F){

  checkMandatory(PSUSamplingParametersData, "PSUSamplingParametersData")
  checkMandatory(AnalyticalPSUEstimateData, "AnalyticalPSUEstimateData")

  NestimatesByStrata <- AnalyticalPSUEstimateData$Abundance[,list(estimates=.N),by="Stratum"]
  if (!length(unique(NestimatesByStrata$estimates))==1){
    stop("Cannot Estimate with heterogeneous lower level stratification. Consider the functions LiftStrata or CollapseStrata.")
  }
  
  LowerLevelStrata <- AnalyticalPSUEstimateData$StratificationVariables[!duplicated(Stratum),.SD, .SDcol=names(AnalyticalPSUEstimateData$StratificationVariables)[names(AnalyticalPSUEstimateData$StratificationVariables)!="SampleId"]]
  names(LowerLevelStrata)[names(LowerLevelStrata)=="Stratum"] <- "LowerStratum"
  
  if (any(names(LowerLevelStrata) %in% names(PSUSamplingParametersData$StratificationVariables))){
    stop("Strata naming conflict. The same column names are used for PSU stratification and lower level stratification")
  }

  missingAbund <- AnalyticalPSUEstimateData$Abundance$SampleId[is.na(AnalyticalPSUEstimateData$Abundance$Abundance)]
  missingFreq <- AnalyticalPSUEstimateData$Abundance$SampleId[is.na(AnalyticalPSUEstimateData$Abundance$Frequency)]
  missingTotal <- AnalyticalPSUEstimateData$Variables$SampleId[is.na(AnalyticalPSUEstimateData$Variables$Total)]
  missingMean <- AnalyticalPSUEstimateData$Variables$SampleId[is.na(AnalyticalPSUEstimateData$Variables$Mean) & !is.nan(AnalyticalPSUEstimateData$Variables$Mean)]
  
  if (!MeanOfMeans & (length(missingAbund)>0 | length(missingTotal)>0)){
    msg <- "Cannot estimate. Estimates are not provided for all samples in 'AnalyticalPSUEstimateData'."
    if (length(missingFreq)==0 & length(missingMean)==0){
      msg <- paste(msg, "Consider the option MeanOfMeans.")
    }

    missing <- unique(c(missingAbund, missingTotal))
    msg <- paste(msg, "Missing for SamplingUnitIds:", truncateStringVector(missing))
    stop(msg)
  }

  if (MeanOfMeans & (length(missingFreq)>0 | length(missingMean)>0)){
    msg <- "Cannot estimate. Estimates are not provided for all samples in 'AnalyticalPSUEstimateData'."
    missing <- unique(c(missingFreq, missingMean))
    
    msg <- paste(msg, "Missing for SamplingUnitIds:", truncateStringVector(missing))
    stop(msg)
  }
  
  #
  # Combine lower level stratification with PSU stratification
  #
  CombinedStrata <- data.table::CJ(Stratum=PSUSamplingParametersData$StratificationVariables$Stratum, LowerStratum=LowerLevelStrata$LowerStratum)
  CombinedStrata <- merge(CombinedStrata, PSUSamplingParametersData$StratificationVariables, by="Stratum")
  CombinedStrata <- merge(CombinedStrata, LowerLevelStrata, by="LowerStratum")
  CombinedStrata$Stratum <- paste("PSU-stratum:", CombinedStrata$Stratum, " Lower-stratum:", CombinedStrata$LowerStratum, sep="")

  #
  # Annotate PSU domains
  #
  PSUSamplingParametersData$SelectionTable <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$PSUDomainVariables, by.x="SamplingUnitId", by.y="SampleId")
  PSUDomainSize <- PSUSamplingParametersData$SelectionTable[,list(PSUDomainRelativeSize=sum(HHsamplingWeight)), by=c("PSUDomain", "Stratum")]
  PSUDomainVariables <- AnalyticalPSUEstimateData$PSUDomainVariables
  PSUDomainVariables$SampleId <- NULL
  PSUDomainVariables <- PSUDomainVariables[!duplicated(PSUDomain),]
  
  CombinedDomains <- data.table::CJ(PSUDomain=PSUDomainVariables$PSUDomain, LowerDomain=AnalyticalPSUEstimateData$DomainVariables$Domain, unique = T)
  CombinedDomains <- merge(CombinedDomains, PSUDomainVariables, by="PSUDomain")
  CombinedDomains <- merge(CombinedDomains, AnalyticalPSUEstimateData$DomainVariables, by.x="LowerDomain", by.y="Domain")
  CombinedDomains$Domain <- paste("PSU-domain: ", CombinedDomains$PSUDomain, " Individual domain:", CombinedDomains$LowerDomain)
  
  #
  # Estimate abundance
  #
  selAbundance <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$Abundance, by.x="SamplingUnitId", by.y="SampleId", suffixes = c(".PSU", ".lower"))
  selAbundance$Stratum <- paste("PSU-stratum:", selAbundance$Stratum.PSU, " Lower-stratum:", selAbundance$Stratum.lower, sep="")
  selAbundance$Domain <- paste("PSU-domain: ", selAbundance$PSUDomain, " Individual domain:", selAbundance$Domain)
  AbundanceTable <- selAbundance[,list(Abundance=mean(Abundance/SelectionProbability)*sum(HHsamplingWeight), Frequency=sum(HHsamplingWeight*Frequency)/sum(HHsamplingWeight)), by=c("Stratum", "Domain")]    
  
  if (!MeanOfMeans){
    StrataAbundance <- AbundanceTable[,list(StrataAbundance=sum(Abundance)), by=c("Stratum")]
    AbundanceTable <- merge(AbundanceTable, StrataAbundance, by=c("Stratum"))
    AbundanceTable$Frequency <- AbundanceTable$Abundance / AbundanceTable$StrataAbundance
    AbundanceTable$StrataAbundance <- NULL
  }
  
  AbundanceCovarianceTable <- covarAbundance(AbundanceTable, selAbundance, MeanOfMeans)
  
  
  #
  # Estimate variables
  #
  
  selVariables <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$Variables, by.x="SamplingUnitId", by.y="SampleId", suffixes = c(".PSU", ".lower"))
  selVariables$Stratum <- paste("PSU-stratum:", selVariables$Stratum.PSU, " Lower-stratum:", selVariables$Stratum.lower, sep="")
  selVariables$Domain <- paste("PSU-domain: ", selVariables$PSUDomain, " Individual domain:", selVariables$Domain)
  VariablesTable <- selVariables[,list(Total=mean(Total/SelectionProbability)*sum(HHsamplingWeight), Mean=sum(Mean[!is.nan(Mean)]*HHsamplingWeight[!is.nan(Mean)])/sum(HHsamplingWeight[!is.nan(Mean)]), NoMeans=all(is.nan(Mean))), by=c("Stratum", "Domain", "Variable")]
  VariablesTable$Mean[VariablesTable$NoMeans] <- NaN
  VariablesTable$NoMeans <- NULL
  
  if (!MeanOfMeans){
    VariablesTable$Mean <- VariablesTable$Total / AbundanceTable$Abundance[match(paste(VariablesTable$Stratum, VariablesTable$Domain), paste(AbundanceTable$Stratum, AbundanceTable$Domain))]
  }

  VariablesCovarianceTable <- covarVariables(VariablesTable, selVariables, MeanOfMeans, AbundanceTable)
  
  
  #
  # Format output
  #
  
  output <- list()
  output$Abundance <- AbundanceTable
  output$Variables <- VariablesTable
  output$AbundanceCovariance <- AbundanceCovarianceTable
  output$VariablesCovariance <- VariablesCovarianceTable
  
  CombinedDomains$LowerDomain <- NULL
  CombinedDomains$PSUDomain <- NULL
  output$DomainVariables <- CombinedDomains[,.SD,.SDcol=c("Domain", names(CombinedDomains)[names(CombinedDomains)!="Domain"])]
  
  CombinedStrata$LowerStratum <- NULL
  output$StratificationVariables <- CombinedStrata[,.SD,.SDcol=c("Stratum", names(CombinedStrata)[names(CombinedStrata)!="Stratum"])]
  
  missingStrata <- output$StratificationVariables$Stratum[!(output$StratificationVariables$Stratum %in% AbundanceTable$Stratum)]
  if (length(missingStrata)>0){
    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain=output$DomainVariables$Domain, unique = T)
    output$Abundance <- rbind(output$Abundance, merge(output$Abundance, strataDomains, all.y=T))
    
    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain1=output$DomainVariables$Domain, Domain2=output$DomainVariables$Domain, unique = T)
    strataDomains <- strataDomains[Domain1>=Domain2,]
    output$AbundanceCovariance <- rbind(output$AbundanceCovariance, merge(output$AbundanceCovariance, strataDomains, all.y = T))
    
    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain=output$DomainVariables$Domain, Variable=output$Variables$Variable, unique = T)
    output$Variables <- rbind(output$Variables, merge(output$Variables, strataDomains, all.y=T))

    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain1=output$DomainVariables$Domain, Domain2=output$DomainVariables$Domain, Variable1=output$Variables$Variable, Variable2=output$Variables$Variable, unique = T)
    strataDomains <- strataDomains[Domain1>=Domain2 & Variable1>=Variable2,]
    output$VariablesCovariance <- rbind(output$VariablesCovariance, merge(output$VariablesCovariance, strataDomains, all.y = T))
    
  }

  return(output)
}

#' Ratio estimate of abundance
#' @description 
#'  Performs ratio estimate of abundance, based on either the ratio of abundance in domains to total weight in a stratum, 
#'  or the frequency and mean weights of each domain.
#' @details
#'  Ratio estimates of abundance are obtained by relating abundance to weight and utilizing census data on landed weight to potentially improve estimates.
#'  Ratio estimation generally incurs some bias in estimation, and analytical expressions for variances are approximate.
#'  
#'  Landings are partitioned and assigned to domains in 'AnalyticalPopulationEstimateData' by matching column names.
#'  Column names in 'StoxLandingData' that are also either Stratification Columns or Domain Columns in 'AnalyticalPopulationEstimateData'
#'  are used to construct the landings partitions that provide total weigts for the ratio estimates.
#'  
#'  Ratio estimation of abundance may either improve an estimate of abundance obtained by other means, or provide an estimate of abundance
#'  when only proportions in domains are known. When only proprotions (frequencies) are known, the Method 'MeanDomainWeight'
#'  must be used. This requires that landing partitions are not covering more than one strata, although they may cover less.
#'  That is all Stratification columns in 'AnalyticalPopulationEstimateData' must have a corresponding column in 'StoxLandingData'
#'  
#'  The function obtains a ratio estimate of total abundance in landings by one of the following methods (provided in the argument 'Method'):
#'  \describe{
#'   \item{TotalDomainWeight}{
#'     When 'Method' is 'TotalDomainWeight', the variable 'Abundance' will be estimated as \eqn{\widehat{rN}^{(s,d)}}, and the variable 'Frequency' as \eqn{\widehat{rf}^{(s,d)}}.
#'     These estimators and their corresponding variances ('AbundanceCovariance' and 'FrequencyCovariance') are given below. 
#'     The estimates are based on the ratio:
#'     \deqn{\hat{R}^{(s,d)}=\frac{W^{(L)}}{\hat{t}^{(L)}}}
#'     where \eqn{L=part(s,d)} is a partition of the landings containing the domain, \eqn{W^{(L)}} is the total landed weight in this partition and \eqn{\hat{t}^{(L)}} is the estimated total weight in this partition.
#'     The abundance is estimated as:
#'     \deqn{\widehat{rN}^{(s,d)} = \hat{R}^{(s,d)}\hat{N}^{(s,d)}}
#'     The frequency is estimated as:
#'     \deqn{\widehat{rf}^{(s,d)} = \frac{\widehat{rN}^{(s,d)}}{\widehat{rN}^{(s)}}}
#'     And covariances are estimated as:
#'     \deqn{\widehat{CoVar}(\widehat{rN}^{(s,d_{1})}, \widehat{rN}^{(s,d_{2})}) = \hat{R}^{(s,d_{1})}\hat{R}^{(s,d_{2})}\widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})})}
#'     \deqn{\widehat{CoVar}(\widehat{rf}^{(s,d_{1})}, \widehat{rf}^{(s,d_{2})}) = \frac{1}{\widehat{rN}^{(s)}}\widehat{CoVar}(\widehat{rN}^{(s,d_{1})}, \widehat{rN}^{(s,d_{2})})}
#'     ignoring the error in \eqn{\hat{R}^{(s,d)}} and \eqn{\widehat{rN}^{(s)}}.     
#'     
#'   
#'   }
#'   \item{MeanDomainWeight}{
#'      When 'Method' is 'MeanDomainWeight', 'Abundance' will be estimated as \eqn{\widehat{qN}^{(s,d)}}, and the variable 'Frequency' as \eqn{\widehat{qf}^{(s,d)}}.
#'     These estimators and their corresponding variances ('AbundanceCovariance' and 'FrequencyCovariance') are given below. 
#'     The estimates are based on the mean individual weight, and an estimate of total landings in each domain:
#'     \deqn{\hat{W}^{(s,d)}=\frac{\hat{f}^{(s,d)}W^{(L)}}{\sum_{(s,d) \in L}\hat{f}^{(s,d)}}}
#'     where \eqn{L=part(s,d)} is a partition of the landings containing the domain. 
#'     Since frequencies are normalized to strata, L cannot contain several strata.
#'     The abundance is estimated as:
#'     \deqn{\widehat{qN}^{(s,d)} = \frac{\hat{W}^{(s,d)}}{\hat{\mu}^{(s,d)}} = \hat{f}^{(s,d)}\hat{Q}^{(s,d)}}
#'     with \eqn{\hat{Q}^{(s,d)}=\frac{W^{(L)}}{\hat{\mu}^{(s,d)}\sum_{(s,d) \in L}\hat{f}^{(s,d)}}}
#'     The frequency is estimated as:
#'     \deqn{\widehat{qf}^{(s,d)} = \frac{\widehat{qN}^{(s,d)}}{\widehat{qN}^{(s)}}}
#'     And covariances are estimated as:
#'     \deqn{\widehat{CoVar}(\widehat{qN}^{(s,d_{1})}, \widehat{qN}^{(s,d_{2})}) = \hat{Q}^{(s,d_{1})}\hat{Q}^{(s,d_{2})}\widehat{CoVar}(\hat{f}^{(s,d_{1})}, \hat{f}^{(s,d_{2})})}
#'     \deqn{\widehat{CoVar}(\widehat{qf}^{(s,d_{1})}, \widehat{qf}^{(s,d_{2})}) = \frac{1}{\widehat{qN}^{(s)}}\widehat{CoVar}(\widehat{qN}^{(s,d_{1})}, \widehat{qN}^{(s,d_{2})})}
#'     ignoring the error in \eqn{\hat{Q}^{(s,d)}} and \eqn{\widehat{qN}^{(s)}}.
#'   }
#'   }
#'  Vocabulary for equations given above:
#'  \describe{
#'   \item{\eqn{part(s,d)}}{The partition of the landings containing the domain \eqn{d} in stratum \eqn{s}.}
#'   \item{\eqn{\hat{N}^{(s,d)}}}{The estimated abundance in the domain \eqn{d} in stratum \eqn{s}. 'Abundance' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{t}^{(s,d)}}}{The estimated total weight in domain \eqn{d} in stratum \eqn{s}. The 'Total' for the 'Variable' identified by the argument 'WeightVariable' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{t}^{(L)}}}{The estimated total weight in the landing partition: \eqn{\hat{t}^{(L)}=\sum_{(s,d) \in L}\hat{t}^{(s,d)}}}
#'   \item{\eqn{\hat{Q}^{(L)}}}{}
#'   \item{\eqn{\widehat{rN}^{(s)}}}{The estimated total abundance in stratum \eqn{s}, based on total domain weight estimates: \eqn{\widehat{rN}^{(s)}=\sum_{d}\widehat{rN}^{(s,d)}}, where the sum runs over all domains in stratum \eqn{s}.}
#'   \item{\eqn{\widehat{qN}^{(s)}}}{The estimated total abundance in stratum \eqn{s}, based on mean domain weight estimates: \eqn{\widehat{qN}^{(s)}=\sum_{d}\widehat{qN}^{(s,d)}}, where the sum runs over all domains in stratum \eqn{s}.}
#'   \item{\eqn{\widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})})}}{The estimated covariance of abundance between the domains \eqn{d_{1}} and \eqn{d_{2}} in stratum \eqn{s}. 'AbundanceCovariance' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{f}^{(s,d)}}}{}
#'   \item{\eqn{\widehat{CoVar}(\hat{f}^{(s,d_{1})}, \hat{f}^{(s,d_{2})})}}{}
#'  }
#'  
#' @param AnalyticalPopulationEstimateData \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with estimates of mean or total weights and frequencies or abundance in domains
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} with census data on total weight in each stratum
#' @param WeightVariable character() name of variable in 'AnalyticalPopulationEstimateData' that represent weight of individuals in grams.
#' @param Method The method of ratio estimation to use. 'TotalDomainWeight' or 'MeanDomainWeight'. See details 
#' @return \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with ratio estimates of abundance and frequency.
#' @examples 
#' 
#'  PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
#'                                        RstoxFDA::CatchLotterySamplingExample, 
#'                                        RstoxFDA::CatchLotteryExample, 
#'                                        "lotterySerialnumber", "Haul", "MissingAtRandom")
#'  individualSamplingParameters <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, 
#'                                        RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))
#'  
#'  # Estimate for the domain 'CountryVessel'                                      
#'  psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
#'                                        individualSamplingParameters, 
#'                                        c("IndividualRoundWeight"), 
#'                                        c("IndividualAge"), c("CountryVessel"))
#'  popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
#'  
#'  # perform ration estimate, assigning total weights by 'CountryVessel'
#'  ratioEst <- RstoxFDA::AnalyticalRatioEstimate(popEst, 
#'                                         RstoxFDA::CatchLotteryLandingExample, 
#'                                         "IndividualRoundWeight", 
#'                                         "TotalDomainWeight")
#'  
#' @concept Analytical estimation
#' @export
#' @md
AnalyticalRatioEstimate <- function(AnalyticalPopulationEstimateData, StoxLandingData, WeightVariable=character(), Method=c("TotalDomainWeight", "MeanDomainWeight")){
  
  if (WeightVariable %in% AnalyticalPopulationEstimateData$Variables){
    stop(paste("'WeightVariable'", WeightVariable, "is not estimated in 'AnalyticalPopulationEstimateData'"))
  }

  if (any(names(StoxLandingData$Landing) %in% c("Stratum", "Domain"))){
    stop("The names 'Stratum' or 'Domain' may not be used for stratification variables in 'StoxLandingData'")
  }
  
  if (!any(names(StoxLandingData$Landing) %in% c(names(AnalyticalPopulationEstimateData$StratificationVariables), names(AnalyticalPopulationEstimateData$DomainVariables)))){
    stop("None of the variables in 'StoxLandingData' are available as StratificationColumns or DomainVariables in 'AnalyticalPopulationEstimateData'")
  }
  
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  checkMandatory(StoxLandingData, "StoxLandingData")
  checkMandatory(WeightVariable, "WeightVariable")
  checkOptions(Method, "Method", c("TotalDomainWeight", "MeanDomainWeight"))
  
  if (Method == "TotalDomainWeight"){

    potentialNames <- c(names(AnalyticalPopulationEstimateData$StratificationVariables), names(AnalyticalPopulationEstimateData$DomainVariables))
    landingsPartition <- potentialNames[potentialNames %in% names(StoxLandingData$Landing)]
    totals <- merge(AnalyticalPopulationEstimateData$Variables[Variable==WeightVariable,.SD,.SDcol=c("Stratum", "Domain", "Total")], AnalyticalPopulationEstimateData$StratificationVariables, by="Stratum")
    totals <- merge(totals, AnalyticalPopulationEstimateData$DomainVariables, by="Domain")
    estTotalBylandingsPartition <- totals[,list(TotalWeightKg=sum(Total)/1000),by=landingsPartition]
    
    landingsByStratum <- StoxLandingData$Landing[,list(LandingsWeightKg=sum(RoundWeight)), by=landingsPartition]
    totalByStratum <- merge(estTotalBylandingsPartition, landingsByStratum)
    
    domainPartitionMap <- data.table::CJ(Stratum=AnalyticalPopulationEstimateData$StratificationVariables$Stratum, Domain=AnalyticalPopulationEstimateData$DomainVariables$Domain, unique = T)
    domainPartitionMap <- merge(domainPartitionMap, AnalyticalPopulationEstimateData$StratificationVariables[,.SD,.SDcol=c("Stratum", landingsPartition[landingsPartition %in% names(AnalyticalPopulationEstimateData$StratificationVariables)])], by="Stratum")
    domainPartitionMap <- merge(domainPartitionMap, AnalyticalPopulationEstimateData$DomainVariables[,.SD,.SDcol=c("Domain", landingsPartition[landingsPartition %in% names(AnalyticalPopulationEstimateData$DomainVariables)])], by="Domain")
    domainPartitionMap <- domainPartitionMap[!duplicated(apply(domainPartitionMap, FUN=paste, 1, collapse=",")),]
    totalByStratum <- merge(totalByStratum, domainPartitionMap)
    
    #
    # Ratio-estimate abundance
    #
    m <- match(paste(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain), paste(totalByStratum$Stratum, totalByStratum$Domain))
    AnalyticalPopulationEstimateData$Abundance$Abundance <- AnalyticalPopulationEstimateData$Abundance$Abundance * totalByStratum$LandingsWeightKg[m] / totalByStratum$TotalWeightKg[m]
    m1 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1), paste(totalByStratum$Stratum, totalByStratum$Domain))
    m2 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2), paste(totalByStratum$Stratum, totalByStratum$Domain))
    AnalyticalPopulationEstimateData$AbundanceCovariance$AbundanceCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance$AbundanceCovariance * (totalByStratum$LandingsWeightKg[m1] / totalByStratum$TotalWeightKg[m1])*(totalByStratum$LandingsWeightKg[m2] / totalByStratum$TotalWeightKg[m2])

    #
    # Ratio-estimate frequencies
    #
    abundanceByStratum <- AnalyticalPopulationEstimateData$Abundance[,list(totalAbundance=sum(Abundance)), by="Stratum"]
    m <- match(AnalyticalPopulationEstimateData$Abundance$Stratum, abundanceByStratum$Stratum)
    AnalyticalPopulationEstimateData$Abundance$Frequency <- AnalyticalPopulationEstimateData$Abundance$Abundance / abundanceByStratum$totalAbundance[m]
    m <- match(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, abundanceByStratum$Stratum)
    AnalyticalPopulationEstimateData$AbundanceCovariance$FrequencyCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance$AbundanceCovariance * (1/abundanceByStratum$totalAbundance[m])**2
    
        
    return(AnalyticalPopulationEstimateData)
  }
  
  if (Method == "MeanDomainWeight"){
    
    stratificationVariables <- names(AnalyticalPopulationEstimateData$StratificationVariables)
    stratificationVariables <- stratificationVariables[stratificationVariables != "Stratum"]
    if (!all(stratificationVariables %in% names(StoxLandingData$Landing))){
      stop("The ratio estimation method 'MeanDomainWeight' can only be used if landings can be identified for all strata. All stratification variables in 'AnalyticalPopulationEstimate, must be columns in 'StoxLandingData'")
    }
    
    potentialNames <- c(names(AnalyticalPopulationEstimateData$StratificationVariables), names(AnalyticalPopulationEstimateData$DomainVariables))
    landingsPartition <- potentialNames[potentialNames %in% names(StoxLandingData$Landing)]
    
    frequencies <- AnalyticalPopulationEstimateData$Abundance[,.SD,.SDcol=c("Stratum", "Domain", "Frequency")]
    frequencies <- merge(frequencies, AnalyticalPopulationEstimateData$StratificationVariables, by="Stratum")
    frequencies <- merge(frequencies, AnalyticalPopulationEstimateData$DomainVariables, by="Domain")
    
    # normalize frequencies to landingsPartition within samplingstrata
    totalFrequencies <- frequencies[,list(totalFreq=sum(Frequency)), by=c("Stratum", landingsPartition)]
    frequencies <- merge(frequencies, totalFrequencies, by=c("Stratum", landingsPartition), all.x = T)
    
    # estimate total landings in domain
    totalLandings <- StoxLandingData$Landing[,list(totalLanding=sum(RoundWeight)*1000), by=landingsPartition] #WeightVariable is in grams.
    frequencies <- merge(frequencies, totalLandings, by=landingsPartition, all.x=T)
    frequencies$domainLanding <- (frequencies$Frequency / frequencies$totalFreq) * frequencies$totalLanding
    
    means <- AnalyticalPopulationEstimateData$Variables[AnalyticalPopulationEstimateData$Variables$Variable == WeightVariable,]
    
    #
    # Ratio-estimate abundance from estimated domain landings and domain means
    #
    freqMatch <- match(paste(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain), paste(frequencies$Stratum, frequencies$Domain))
    meanMatch <- match(paste(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain), paste(means$Stratum, means$Domain))
    AnalyticalPopulationEstimateData$Abundance$Abundance <- frequencies$domainLanding[freqMatch] / means$Mean[meanMatch]
    m1 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1), paste(frequencies$Stratum, frequencies$Domain))
    m2 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2), paste(frequencies$Stratum, frequencies$Domain))
    meanMatch1 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1), paste(means$Stratum, means$Domain))
    meanMatch2 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2), paste(means$Stratum, means$Domain))
    AnalyticalPopulationEstimateData$AbundanceCovariance$AbundanceCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance$FrequencyCovariance * (frequencies$totalLanding[m1] / (frequencies$totalFreq[m1] * means$Mean[meanMatch1])) * (frequencies$totalLanding[m2] / (frequencies$totalFreq[m2] * means$Mean[meanMatch2]))
    
    #
    # Ratio-estimate frequencies
    #
    abundanceByStratum <- AnalyticalPopulationEstimateData$Abundance[,list(totalAbundance=sum(Abundance)), by="Stratum"]
    m <- match(AnalyticalPopulationEstimateData$Abundance$Stratum, abundanceByStratum$Stratum)
    AnalyticalPopulationEstimateData$Abundance$Frequency <- AnalyticalPopulationEstimateData$Abundance$Abundance / abundanceByStratum$totalAbundance[m]
    m <- match(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, abundanceByStratum$Stratum)
    AnalyticalPopulationEstimateData$AbundanceCovariance$FrequencyCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance$AbundanceCovariance * (1/abundanceByStratum$totalAbundance[m])**2
    
    
    return(AnalyticalPopulationEstimateData)
  }

  
}

#' @noRd
ProbabilisticSuperIndividuals <- function(StoxBioticData, PSUSamplingParametersData, IndividualSamplingParametersData){
  
}
