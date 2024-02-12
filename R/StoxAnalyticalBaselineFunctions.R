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
#' @return \code{\link[RstoxFDA]{IndividualSamplingParametersData}}
#' @noRd
DefineSamplingHierarchy <- function(IndividualSamplingParametersData, Hierarchy=character(), Stratification=character(), StrataSizes=character(), SelectionMetod=character(), CollapseStrata=character()){
  
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

#'
AnalyticalPSUEstimate <- function(StoxBioticData, IndividualSamplingParametersData, Variables=character(), DomainVariables=character()){

  ind <- RstoxData::MergeStoxBiotic(StoxBioticData, "Individual")
  
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
  
  missingvariables <- Variables[!(Variables %in% names(ind))]
  if (length(missingvariables)>0){
    stop(paste("Invalid speficiation of variables. The following variables does not exist in StoxBioticData:", paste(missingvariables, collapse=",")))
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
  #get NAs for non-sampled strata
  abundance <- merge(abundance, IndividualSamplingParametersData$SampleTable[,c("SampleId", "Stratum")], by=c("SampleId", "Stratum"), all.y=T)
  
  estimates <- NULL
  for (v in Variables){
    est <- ind[,list(Variable=v, Total=sum(get(v)/InclusionProbability), Mean=sum(get(v)*HTsamplingWeight)/sum(HTsamplingWeight)), by=c("SampleId", "Stratum", "Domain")]
    #get NAs for non-sampled strata
    est <- merge(est, IndividualSamplingParametersData$SampleTable[,c("SampleId", "Stratum")], by=c("SampleId", "Stratum"), all.y=T)
    est$Variable <- v
    estimates <- rbind(estimates, est)
  }
  
  output <- list()
  output$Abundance <- abundance
  output$Variables <- estimates
  output$DomainVariables <- domaintable
  output$StratificationVariables <- IndividualSamplingParametersData$StratificationVariables

  return(output)
}

#' @noRd
addZeroesAbundance <- function(table, domains){
  samplingParameters <- table[!duplicated(SamplingUnitId), .SD, .SDcol=c("SamplingUnitId", "InclusionProbability", "HTsamplingWeight", "SelectionProbability", "HHsamplingWeight")]

  allCombos <- data.table::CJ(Domain=domains$Domain, SamplingUnitId=table$SamplingUnitId, unique = T)
  allCombos <- merge(allCombos, table[!duplicated(SamplingUnitId),c("SamplingUnitId", "Stratum", "Stratum.lower")], by="SamplingUnitId")
  missingCombos <- allCombos[!(paste(Stratum, Domain) %in% paste(table$Stratum, table$Domain)),]

  zeroes <- merge(table[0,], missingCombos, by.x=c("SamplingUnitId", "Stratum", "Domain", "Stratum.lower"), by.y=c("SamplingUnitId", "Stratum", "Domain", "Stratum.lower"), all.y=T)
  zeroes$InclusionProbability <- samplingParameters$InclusionProbability[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$HTsamplingWeight < samplingParameters$HTsamplingWeight[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$SelectionProbability <- samplingParameters$SelectionProbability[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$HHsamplingWeight <- samplingParameters$HHsamplingWeight[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$Abundance <- 0
  zeroes$Frequency <- 0

  return(rbind(table, zeroes))
  
}

#' @noRd
addZeroesVariables <- function(table, domains){
  
  samplingParameters <- table[!duplicated(SamplingUnitId), .SD, .SDcol=c("SamplingUnitId", "InclusionProbability", "HTsamplingWeight", "SelectionProbability", "HHsamplingWeight")]
  
  allCombos <- data.table::CJ(Domain=domains$Domain, SamplingUnitId=table$SamplingUnitId, Variable=unique(table$Variable), unique = T)
  allCombos <- merge(allCombos, strata[!duplicated(SamplingUnitId),c("SamplingUnitId", "Stratum", "Stratum.lower")], by="SamplingUnitId")
  missingCombos <- allCombos[!(paste(allCombos$LowerStratum, allCombos$Domain, allCombos$Variable) %in% paste(table$Stratum.lower, table$Domain, table$Variable))]

  zeroes <- merge(table[0,], missingCombos, by.x=c("SamplingUnitId", "Stratum", "Stratum.lower", "Domain", "Variable"), by.y=c("SamplingUnitId", "Stratum", "Stratum.lower", "Domain", "Variable"), all.y=T)

  zeroes$InclusionProbability <- samplingParameters$InclusionProbability[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$HTsamplingWeight < samplingParameters$HTsamplingWeight[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$SelectionProbability <- samplingParameters$SelectionProbability[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$HHsamplingWeight <- samplingParameters$HHsamplingWeight[match(zeroes$SamplingUnitId, samplingParameters$SamplingUnitId)]
  zeroes$Total <- 0
  zeroes$Mean <- NaN
  
  browser()
  
  return(rbind(table, zeroes))
  
}

covarAbundance <- function(Totals, PSUSampling, MeanOfMeans){

  tab <- merge(PSUSampling, Totals, by=c("Stratum", "Domain"), suffixes=c(".PSU", ".Total"))
  tab$AbundanceDev <- tab$Abundance.PSU/tab$SelectionProbability - tab$Abundance.Total
  tab$FrequencyDev <- tab$Frequency.PSU - tab$Frequency.Total
  tab <- tab[,.SD,.SDcol=c("Stratum", "Domain", "SamplingUnitId", "AbundanceDev", "FrequencyDev")]
  
  sampleSize <- PSUSampling[,list(n=length(unique(SamplingUnitId))), by="Stratum"]

  cross <- data.table::CJ(Domain1=unique(Totals$Domain), Domain2=unique(Totals$Domain))
  cross <- cross[cross$Domain1>=cross$Domain2,]
  cross <- merge(cross, tab[!duplicated(paste(Stratum, Domain, SamplingUnitId)),.SD,.SDcol=c("Stratum", "Domain", "SamplingUnitId")], by.x=c("Domain1"), by.y=c("Domain"), allow.cartesian = T)
  cross <- merge(cross, tab, by.x=c("SamplingUnitId", "Stratum", "Domain2"), by.y=c("SamplingUnitId", "Stratum", "Domain"), suffixes = c("1", "2"))
  cross$AbundanceDevProduct <- cross$AbundanceDev1 * cross$AbundanceDev2
  cross$FrequencyDevProduct <- cross$FrequencyDev1 * cross$FrequencyDev2

  browser()
    
  sumOfProducts <- cross[,list(AbundanceSOP=sum(AbundanceDevProduct), FrequencySOP=sum(FrequencyDevProduct)), by=c("Stratum", "Domain1", "Domain2")]
  sumOfProducts <- merge(sumOfProducts, sampleSize, by="Stratum")
  
  covar <- sumOfProducts[,list(AbundanceCovariance=AbundanceSOP/(n*(n-1)), FrequencyCovariance=FrequencySOP/(n*(n-1))), by=c("Stratum", "Domain1", "Domain2")]

  if (!MeanOfMeans){
    StrataAbundance <- Totals[,list(StrataAbundance=sum(Abundance)), by="Stratum"]
    covar$FrequencyCovariance <- covar$AbundanceCovariance * (1/StrataAbundance$StrataAbundance[match(covar$Stratum, StrataAbundance$Stratum)])**2
  }
  
  return(covar)
}

covarVariables <- function(Totals, PSUSampling, MeanOfMeans){
  
  tab <- merge(PSUSampling, Totals, by=c("Stratum", "Domain", "Variable"), suffixes=c(".PSU", ".Total"))
  tab$TotalDev <- tab$Total.PSU/tab$SelectionProbability - tab$Total.Total
  tab$MeanDev <- tab$Mean.PSU*tab$HHsamplingWeight - tab$Mean.Total
  tab <- tab[,.SD,.SDcol=c("Stratum", "Domain", "SamplingUnitId", "Variable", "TotalDev", "MeanDev", "HHsamplingWeight")]
  
  sampleSize <- PSUSampling[,list(n=length(unique(SamplingUnitId))), by="Stratum"]
  
  cross <- data.table::CJ(Domain1=unique(Totals$Domain), Variable1=unique(Totals$Variable), Domain2=unique(Totals$Domain), Variable2=unique(Totals$Variable))
  cross <- merge(cross, tab, by.x=c("Domain1", "Variable1"), by.y=c("Domain", "Variable"), allow.cartesian = T)
  
  
  cross <- merge(cross, tab, by.x=c("Stratum", "Domain2", "Variable2"), by.y=c("Stratum", "Domain", "Variable"), suffixes = c("1", "2"))
  cross$TotalDevProduct <- cross$TotalDev1 * cross$TotalDev2
  cross$MeanDevProduct <- cross$MeanDev1 * cross$MeanDev2
  cross$coSampled <- as.numeric(!is.na(cross$MeanDev1)) * as.numeric(!is.na(cross$MeanDev2))
  browser()
  stop("Figure out how to deal with cross domain covariance (cosampled domains)")
  sumOfProducts <- cross[,list(TotalSOP=sum(TotalDevProduct), MeanSOP=sum(MeanDevProduct[coSampled==1]), CoSampled=sum(coSampled), Freq1=sum(HHsamplingWeight1), Freq2=sum(HHsamplingWeight2)), by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]
  sumOfProducts <- merge(sumOfProducts, sampleSize, by="Stratum")
  
  covar <- sumOfProducts[,list(TotalCovariance=TotalSOP/(n*(n-1)), MeanCovariance=MeanSOP/(n*(n-1))), by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]
  
  return(covar)
  
}

#' For strata with zero abundance, Means are NaN.
#' For strata with missing sampling or missing totals, Abundance, Frequencies, Totals and Means are NA
#' @noRd
AnalyticalPopulationEstimate <- function(PSUSamplingParametersData, AnalyticalPSUEstimateData, MeanOfMeans=F){

  LowerLevelStrata <- AnalyticalPSUEstimateData$StratificationVariables[!duplicated(Stratum),.SD, .SDcol=names(AnalyticalPSUEstimateData$StratificationVariables)[names(AnalyticalPSUEstimateData$StratificationVariables)!="SampleId"]]
  names(LowerLevelStrata)[names(LowerLevelStrata)=="Stratum"] <- "LowerStratum"
  
  if (any(names(LowerLevelStrata) %in% names(PSUSamplingParametersData$StratificationVariables))){
    stop("Strata naming conflict. The same column names are used for PSU stratification and lower level stratification")
  }

  missing <- AnalyticalPSUEstimateData$Abundance$SampleId[is.na(AnalyticalPSUEstimateData$Abundance$Domain)]
  missing <- c(missing, AnalyticalPSUEstimateData$Variables$SampleId[is.na(AnalyticalPSUEstimateData$Variables$Domain)])
  
  if (length(missing)>0){
    missing <- unique(missing)
    stop("Cannot estimate. Estimates are not provided for all samples in 'AnalyticalPSUEstimateData'. Missing for SamplingUnitIds: ", truncateStringVector(missing))
  }
  
  CombinedStrata <- data.table::CJ(Stratum=PSUSamplingParametersData$StratificationVariables$Stratum, LowerStratum=LowerLevelStrata$LowerStratum)
  CombinedStrata <- merge(CombinedStrata, PSUSamplingParametersData$StratificationVariables, by="Stratum")
  CombinedStrata <- merge(CombinedStrata, LowerLevelStrata, by="LowerStratum")
  CombinedStrata$Stratum <- paste("PSU-stratum:", CombinedStrata$Stratum, " Lower-stratum:", CombinedStrata$LowerStratum, sep="")
  
  selAbundance <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$Abundance, by.x="SamplingUnitId", by.y="SampleId", suffixes = c(".PSU", ".lower"))
  selAbundance$Stratum <- paste("PSU-stratum:", selAbundance$Stratum.PSU, " Lower-stratum:", selAbundance$Stratum.lower, sep="")
  selAbundance <- addZeroesAbundance(selAbundance, AnalyticalPSUEstimateData$DomainVariables)
  AbundanceTable <- selAbundance[,list(Abundance=mean(Abundance/SelectionProbability)*sum(HHsamplingWeight), Frequency=sum(HHsamplingWeight*Frequency)/sum(HHsamplingWeight)), by=c("Stratum", "Domain")]    
  
  if (!MeanOfMeans){
    StrataAbundance <- AbundanceTable[,list(StrataAbundance=sum(Abundance)), by=c("Stratum")]
    AbundanceTable <- merge(AbundanceTable, StrataAbundance, by=c("Stratum"))
    AbundanceTable$Frequency <- AbundanceTable$Abundance / AbundanceTable$StrataAbundance
    AbundanceTable$StrataAbundance <- NULL
  }
  
  AbundanceCovarianceTable <- covarAbundance(AbundanceTable, selAbundance, MeanOfMeans)
  
  selVariables <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$Variables, by.x="SamplingUnitId", by.y="SampleId", suffixes = c(".PSU", ".lower"))
  selVariables$Stratum <- paste("PSU-stratum:", selVariables$Stratum.PSU, " Lower-stratum:", selVariables$Stratum.lower, sep="")
  selVariables <- addZeroesVariables(selVariables, AnalyticalPSUEstimateData$DomainVariables)
  VariablesTable <- selVariables[,list(Total=mean(Total/SelectionProbability)*sum(HHsamplingWeight), Mean=sum(Mean*HHsamplingWeight)/sum(HHsamplingWeight)), by=c("Stratum", "Domain", "Variable")]
  
  if (!MeanOfMeans){
    VariablesTable$Mean <- VariablesTable$Total / AbundanceTable$Abundance[match(paste(VariablesTable$Stratum, VariablesTable$Domain), paste(AbundanceTable$Stratum, AbundanceTable$Domain))]
  }

  VariablesCovarianceTable <- covarVariables(VariablesTable, selVariables, MeanOfMeans)
  
  output <- list()
  output$Abundance <- AbundanceTable
  output$Variables <- VariablesTable
  output$AbundanceCovariance <- AbundanceCovarianceTable
  output$VariablesCovariance <- VariablesCovarianceTable
  output$DomainVariables <- AnalyticalPSUEstimateData$DomainVariables
  
  CombinedStrata$LowerStratum <- NULL
  output$StratificationVariables <- CombinedStrata
warning("Handled unsampled PSU strata")
  return(output)
}

AnalyticalRatioEstimate <- function(AnalyticalPopulationEstimateData, StoxLandingData, DomainVariables){
  
  #ratio estimate for total number in domain. Domain variables not in landings are taken to be estimated domain of interest. Additional domain variables are specified in DomainVariables
  
}

#' @noRd
ProbabilisticSuperIndividuals <- function(StoxBioticData, PSUSamplingParametersData, IndividualSamplingParametersData){
  
}
