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
    stop("Not all stratification columns were found at", targetTable, ", where the SamplingUnitId", SamplingUnitId, "is found.")
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
  flatStox$Stratum <- apply(flatStox[,.SD, .SDcol=StratificationColumns], 1, paste, collapse="/")
  flatStox$SamplingUnitId <- flatStox[[SamplingUnitId]]
  flatStox$Order <- as.numeric(NA)
  
  CommonSelectionData <- flatStox[,list(InclusionProbability=as.numeric(NA), SelectionProbability=as.numeric(NA), RelativeSelectionProbability=1/length(unique(SamplingUnitId)), SelectionDescription=as.character(NA)), by=c("Stratum")]
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
  
  colClasses <- c(Stratum="character", N="numeric", n="numeric", SelectionMethod="character", FrameDescription="character", Order="numeric", SamplingUnitId="character", InclusionProbability="numeric", SelectionProbability="numeric", RelativeSelectionProbability="numeric", SelectionDescription="character")
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

  selectionTable <- designParameters[,.SD,.SDcol=c("Stratum", "Order", "SamplingUnitId", "InclusionProbability", "SelectionProbability", "RelativeSelectionProbability", "SelectionDescription")]
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

#' Define Multi-Stage Sampling Design Parameters
#' @description 
#'  Define sampling design parameters for intermediate sampling units in multi-stage sampling.
#' @details 
#'  The DefintionMethod 'ResourceFile' reads design parameters from a tab delimited file with headers corresponding to those listed in 
#'  \code{\link[RstoxFDA]{MultiStageSamplingParametersData}}. The data is provided as one table, so that the information in 'sampleTable' is repeated for each entry in 'selectionTable'.
#'  Any columns not named in \code{\link[RstoxFDA]{MultiStageSamplingParametersData}} are assumed to be stratification variables.
#'  The conditions listed for the variables in \code{\link[RstoxFDA]{MultiStageSamplingParametersData}} are checked upon reading the data, and
#'  execution halts with error if any are violated.
#'  
#'  The DefinitionMethod 'AdHocStoxBiotic' constructs Sampling Design Parameters from data, 
#'  assuming equal probability sampling with fixed sample size, selection with replacement and complete response.
#'  This is a reasonable approximation if within-strata sampling is approximately simple random selections, 
#'  non-response is believed to be at random, and only a small fraction of the strata is sampled, 
#'  so that with and without replacement sampling probabilities are approximately equal.
#' @param processData \code{\link[RstoxFDA]{MultiStageSamplingParametersData}} as returned from this function.
#' @param DefinitionMethod 'ResourceFile' or 'AdHocStoxBiotic'
#' @param FileName path to resource file
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} Sample data to construct design parameters from
#' @param SamplingUnitId name of column in 'StoxBioticData' that identifies the sampling unit the design is constructed for.
#' @param StratificationColumns name of any column (at the same table as 'SamplingUnitId') that are to be used to define Strata for sampling.
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{MultiStageSamplingParametersData}}
#' @export
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
DefineMultiStageSamplingParameters <- function(processData, DefinitionMethod=c("ResourceFile", "AdHocStoxBiotic"), FileName=character(), StoxBioticData, SamplingUnitId=character(), StratificationColumns=character(), UseProcessData=F){

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

#' make IndividualDesignParameters for stratified selection of Individuals
#' if StratificationColumn contains only one column and this is called Stratum, do not add any stratification columns.
#' @noRd
extractIndividualDesignParametersStoxBiotic <- function(StoxBioticData, StratificationColumns, Parameters){
  
  individuals <- RstoxData::mergeByIntersect(StoxBioticData$Individual, StoxBioticData$Sample)
  
  #check first, so no restrictions need to be put on names of Parameters.
  hasParam <- rep(FALSE, nrow(individuals))
  for (p in Parameters){
    hasParam <- hasParam | !is.na(individuals[[p]])
  }
  
  individuals$Stratum <- apply(individuals[,.SD, .SDcol=StratificationColumns], 1, paste, collapse="/")
  StratificationColumns <- StratificationColumns[StratificationColumns!="Stratum"]
  
  individuals$SampleId <- individuals$Sample
  
  stratificationTable <- individuals[!duplicated(paste(individuals$SampleId, individuals$Stratum)), .SD,.SDcol=c("SampleId", "Stratum", StratificationColumns)]
  observationTable <- data.table::data.table(Parameter=Parameters)
  
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
  selectedIndividuals$InclusionProbability <- as.numeric(NA) #Need order. Could possibly be obtained by convention from StoxBioticiIndividual$IndividualKey, would have to be user choice.
  selectedIndividuals$SelectionProbability <- 1/selectedIndividuals$N
  selectedIndividuals$RelativeSelectionProbability <- 1/selectedIndividuals$N
  selectedIndividuals$SelectionDescription <- as.character(NA)
  
  selectionTable <- selectedIndividuals[,.SD,.SDcol=c("SampleId", "Stratum", "Order", "IndividualId", "InclusionProbability", "SelectionProbability", "RelativeSelectionProbability", "SelectionDescription")]
  
  designParams <- list()
  designParams$SampleTable <- sampleTable
  designParams$SelectionTable <- selectionTable
  designParams$ObservationVariables <- observationTable
  designParams$StratificationVariables <- stratificationTable
  
  return(designParams)
}

#' Define Sub-Sampling Parameters for Individuals
#' @description 
#'  Define approximate sampling design parameters for a sub-sample of individuals. Design parameters are inferred from data provided in ~\code{\link[RstoxData]{StoxBioticData}},
#'  and specify how a set of individuals recorded on the Individual table were selected for observation/measurement from a sample recorded on the Sample table.
#' @details 
#'  Sampling parameters are approximate inferred, assuming that all selected individuals are recorded, and based on some user-controllable assumptions about the selection process,
#'  specified by the appropriate 'DefinitionMethod'. Individuals with a non-missing value for any of the parameters in 'Parameters' are treated as selected for observation.
#'  
#'  The available DefinitionMethods are:
#'  \describe{
#'   \item{SRS}{Simple Random Selection. Individuals are selected for measurment by simple random selection without replacement}
#'   \item{Stratified}{Stratified Selection. Individuals are selected for measurement by stratified random selection without replacement. Strata are specified as the combination of columns provided in 'StratificationColumns'. The number of fish in each stratum is estimated by the total in sample and the proportion of measured fish in each stratum.}
#'   \item{LengthStratified}{Length stratified selection. Individuals are selected for measurement by stratified random selection without replacement. Strata are length groups, specified by the left closed intervals starting with [0,'LengthInterval'>.}
#'  }
#' @param processData \code{\link[RstoxFDA]{IndividualSamplingParametersData}} as returned from this function.
#' @param StoxBioticData Data to define individual sampling parameters for
#' @param DefinitionMethod Method to infer sampling parameters, 'SRS', 'Stratified' or 'LengthStratified'. See details.
#' @param Parameters Measurements / observations of individuals included in the design specification. Must all be column on the Individual-table of StoxBioticData. 
#' @param LengthInterval width of length strata in cm. Specifies left closed intervals used for Length Stratified selection (DefinitionMethod 'Stratified'). A value of 5 indicates that observation are selected stratified on length groups [0 cm,5 cm>, [5 cm, 10 cm>, and so on.
#' @param StratificationColumns names of columns in the Individual table of StoxBioticData that identify strata for Stratified selection (DefinitionMethod 'Stratified').
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{IndividualSamplingParametersData}}
#' @export
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
DefineIndividualSamplingParameters <- function(processData, StoxBioticData, DefinitionMethod=c("SRS", "Stratified", "LengthStratified"), Parameters=c(), LengthInterval=numeric(), StratificationColumns=character(), UseProcessData=F){

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
    StoxBioticData$Individual$Stratum <- rep("All", nrow(StoxBioticData$Individual))
    StratificationColumns <- c("Stratum")
    return(extractIndividualDesignParametersStoxBiotic(StoxBioticData, StratificationColumns, Parameters))
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
    if (any(is.na(StoxBioticData$Individual$IndividualTotalLength))){
      missing <- StoxBioticData$Individual$Individual[is.na(StoxBioticData$Individual$IndividualTotalLength)]
      if (lengt(missing)>5){
        missing <- c(missing[1:5], "...")
      }
      stop(paste("Cannot specify length stratified selection when some individuals are not measured. Missing IndividualTotalLength for:", paste(missing, collapse=",")))
    }
    
    lengthGroups <- seq(0,max(StoxBioticData$Individual$IndividualTotalLength)+LengthInterval,LengthInterval)
    StoxBioticData$Individual$LengthStratum <- paste(as.character(cut(StoxBioticData$Individual$IndividualTotalLength, lengthGroups, right=F)), "cm")
    StratificationColumns <- c("LengthStratum")
    return(extractIndividualDesignParametersStoxBiotic(StoxBioticData, StratificationColumns, Parameters))
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
    return(extractIndividualDesignParametersStoxBiotic(StoxBioticData, StratificationColumns, Parameters))
  }
  
}

#' @noRd
AssignPSUDesignParameters <- function(){}

#' @noRd
DefinePSUCoInclusionProbabilities <- function(){}

#' @noRd
ProbabilisticSuperIndividuals <- function(){}