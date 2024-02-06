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
  
  CommonSelectionData <- flatStox[,list(InclusionProbability=as.numeric(NA), HTsamplingWeight=1/length(unique(SamplingUnitId)), SelectionProbability=as.numeric(NA), HHsamplingWeight=as.numeric(NA), SelectionDescription=as.character(NA)), by=c("Stratum")]
  selectionUnits <- flatStox[,.SD, .SDcol=c("Stratum", "Order", "SamplingUnitId")]
  selectionUnits <- selectionUnits[!duplicated(selectionUnits$SamplingUnitId),]
  selectionTable <- merge(flatStox[,.SD, .SDcol=c("Stratum", "Order", "SamplingUnitId")], CommonSelectionData)
  sampleTable <- flatStox[,list(N=as.numeric(NA), n=length(unique(SamplingUnitId)), SelectionMethod="FSWR", FrameDescription=as.character(NA)), by=c("Stratum")]
  sampleTable <- sampleTable[,.SD,.SDcol=c("Stratum", "N", "n", "SelectionMethod", "FrameDescription")]
  stratificationTable <- flatStox[,.SD,.SDcol=c("Stratum", StratificationColumns)]
  stratificationTable <- stratificationTable[!duplicated(stratificationTable$Stratum),]
  assignmentTable <- data.table::data.table(DataRecordsId=character())
  
  designParameters <- list()
  designParameters$SampleTable <- sampleTable
  designParameters$SelectionTable <- selectionTable
  designParameters$StratificationVariables <- stratificationTable
  designParameters$Assignment <- assignmentTable
  
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
  assignmentTable <- data.table::data.table(DataRecordsId=character())
  
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
  designParameters$Assignment <- assignmentTable
  
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
#'  assuming equal probability sampling with fixed sample size, selection without replacement and complete response.
#'  This is a reasonable approximation if within-strata sampling is approximately simple random selections, 
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
    
  Nstrata <- designParam$StratificationVariables[,list(Nstrata=.N), by="SampleId"]
  if (all(Nstrata$Nstrata==1)){
    if (length(retain)==0){
      designParam$StratificationVariables$Stratum <- "All" 
    }
    designParam$StratificationVariables <- designParam$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum", retain)]
    return(designParam)
  }

  selectionStratumIndex <- match(paste(designParam$SelectionTable$Stratum, designParam$SelectionTable$SampleId), paste(designParam$StratificationVariables$Stratum, designParam$StratificationVariables$SampleId))
  sampleStratumIndex <- match(paste(designParam$SampleTable$Stratum, designParam$SampleTable$SampleId), paste(designParam$StratificationVariables$Stratum, designParam$StratificationVariables$SampleId))
  
  if (length(retain)==0){
    designParam$StratificationVariables$Stratum <- "All"
  }
  else{
    designParam$StratificationVariables$Stratum <- apply(designParam$StratificationVariables[,.SD, .SDcol=retain], 1, paste, collapse="/")
  }
  
  designParam$SampleTable$Stratum <- designParam$StratificationVariables$Stratum[sampleStratumIndex]
  designParam$SelectionTable$Stratum <- designParam$StratificationVariables$Stratum[selectionStratumIndex]
  
  NselectionMethods <- designParam$SampleTable[,list(NselMet=length(unique(SelectionMethod))), by=c("SampleId", "Stratum")]
  if (any(NselectionMethods$NselMet>1)){
    stop("Cannot collapse strate with heterogenous selection methods")
  }
  
  weights <- designParam$SelectionTable[,list(HTsum=sum(1/InclusionProbability), HHsum=sum(1/SelectionProbability)),by=c("SampleId", "Stratum")]
  designParam$SelectionTable <- merge(designParam$SelectionTable, weights, by=c("SampleId", "Stratum"))
  designParam$SelectionTable$HTsamplingWeight <- 1/(designParam$SelectionTable$InclusionProbability * designParam$SelectionTable$HTsum)
  designParam$SelectionTable$HHsamplingWeight <- 1/(designParam$SelectionTable$SelectionProbability * designParam$SelectionTable$HHsum)
  designParam$SelectionTable$HHsum <- NULL
  designParam$SelectionTable$HTsum <- NULL
  
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
    stop(paste("Cannot infer sampling parameters for individuals from Samples with missing total number. CatchFractionNumber missing for Sample:", paste(truncateStringVector(missing), collapse=",")))
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
  CollapseStrata=TRUE
  
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
      stop(paste("Cannot specify length stratified selection when some individuals are not measured. Missing IndividualTotalLength for:", paste(truncateStringVector(missing), collapse=",")))
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
#'  Assigns data records to PSU Sampling Parameters and provides non-response adjustments for
#'  selected PSUs that was not sampled.
#' @details 
#'  Some sampling parameters provided in ~\code{\link[RstoxFDA]{PSUSamplingParametersData}} are only
#'  interpretable for sampling with complete response. This function adjusts these parameters, removes non-respondents from the 
#'  ~\code{\link[RstoxFDA]{PSUSamplingParametersData}}, and checks that all responding PSUs are present in data records.
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
#' @param PSUSamplingParametersData ~\code{\link[RstoxFDA]{PSUSamplingParametersData}} with sampling parameters for PSU selection
#' @param StoxBioticData ~\code{\link[RstoxData]{StoxBioticData}} with data records for responding PSUs.
#' @param DataRecordsId name of Variable in ~\code{\link[RstoxData]{StoxBioticData}} that represent records of sampled PSUs
#' @param DefinitionMethod The method for dealing with non-response, e.g. 'MissingAtRandon'
#' @return ~\code{\link[RstoxFDA]{PSUSamplingParametersData}}
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
#' @export
AssignPSUSamplingParameters <- function(PSUSamplingParametersData, StoxBioticData, DataRecordsId, DefinitionMethod=c("MissingAtRandom")){
  checkMandatory(PSUSamplingParametersData, "PSUSamplingParametersData")
  checkMandatory(StoxBioticData, "StoxBioticData")
  checkMandatory(DataRecordsId, "DataRecordsId")
  checkOptions(DefinitionMethod, "DefinitionMethod", c("MissingAtRandom"))
  
  level <- NULL
  for (l in names(StoxBioticData)){
    if (DataRecordsId %in% names(StoxBioticData[[l]])){
      level <- l
    }
  }
  if (is.null(level)){
    stop(paste("The variable provided for DataRecordsId (", DataRecordsId,") is not a variable in 'StoxBioticData'"), sep="")
  }
  
  records <- PSUSamplingParametersData$SelectionTable$SamplingUnitId[!is.na(PSUSamplingParametersData$SelectionTable$SamplingUnitId)]
  if (!all(records %in% StoxBioticData[[l]][[DataRecordsId]])){
    missing <- records[!(records %in% StoxBioticData[[l]][[DataRecordsId]])]
    stop(paste("Records are not found for all sampled PSUs. Missing for the following SamplingUnitIds (", DataRecordsId,"): ", paste(truncateStringVector(missing), collapse=","), sep=""))
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
  
  PSUSamplingParametersData$Assignment$DataRecordsId <- DataRecordsId
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
  namingConflicts <- names(ind)[names(ind) %in% reservedNames]
  if (length(namingConflicts)>0){
    stop("The variables", paste(paste(namingConflicts, collapse=","), " are specified. The following variable names cannot be used for variables that estimates should be provided for:", paste(reservedNames, collapse=",")))
  }
  
  missingDomainIds <- DomainVariables[!(DomainVariables %in% names(ind))]
  if (length(missingDomainIds)>0){
    stop(paste("Invalid speficiation of domain variables. The following variables does not exist in StoxBioticData:", paste(missingDomainIds, collapse=",")))
  }
  
  missingvariables <- Variables[!(Variables %in% names(ind))]
  if (length(missingvariables)>0){
    stop(paste("Invalid speficiation of variables. The following variables does not exist in StoxBioticData:", paste(missingvariables, collapse=",")))
  }

  #put Domain in ind
  ind$Domain <- "All"
  if (length(DomainVariables)>0){
    ind$Domain <- apply(ind[,.SD, .SDcol=DomainVariables], FUN=paste, 1, collapse="/")
  }
  domaintable <- unique(ind[,c("Domain", DomainVariables)])
    
  ind <- ind[,.SD,.SDcol=c("Individual", "Domain", Variables)]
  
  ind <- merge(ind, IndividualSamplingParametersData$SelectionTable, by.x=c("Individual"), by.y=c("IndividualId"))
  
  abundance <- ind[,list(Abundance=sum(1/InclusionProbability), Frequency=sum(HTsamplingWeight)), by=c("SampleId", "Stratum", "Domain")]
  
  estimates <- NULL
  for (v in Variables){
    est <- ind[,list(Variable=v, Total=sum(get(v)/InclusionProbability), Mean=sum(get(v)*HTsamplingWeight)), by=c("SampleId", "Stratum", "Domain")]
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
AnalyticalPopulationEstimate <- function(StoxBioticData, PSUSamplingParametersData, AnalyticalPSUEstimateData, DomainVariables=character(), MeanOfMeans=F, IncludeStratumInDomain=FALSE){

  #calculate total and means for all counts and totalvariables in AnalyticalPSUEstimateData. If MeanOfMeans, calculate mean of Means in stead.
    
  #estimate by stratum
  
  #add over strata if not stratum is included in domain
  
}

AnalyticalRatioEstimate <- function(AnalyticalPopulationEstimateData, StoxLandingData, DomainVariables){
  
  #ratio estimate for total number in domain. Domain variables not in landings are taken to be estimated domain of interest. Additional domain variables are specified in DomainVariables
  
}

#' @noRd
ProbabilisticSuperIndividuals <- function(StoxBioticData, PSUSamplingParametersData, IndividualSamplingParametersData){
  
}
