#' Construct design parameters assuming FSWOR, non-finite, equal prob, potentially stratified
#' @noRd
assumeDesignParametersStoxBiotic <- function(StoxBioticData, SamplingUnitId, StratificationColumns=c()){
  
  flatStox <- RstoxData::MergeStoxBiotic(StoxBioticData, TargetTable = "Sample")
    
  if (!(SamplingUnitId %in% names(flatStox))){
    stop(paste("The SamplingUnitId", SamplingUnitId, "was not found in 'StoxBioticData'"))
  }

  if (isGiven(StratificationColumns) & length(StratificationColumns)>0 & !all(StratificationColumns %in% names(flatStox))){
    stop("Not all stratification columns were found in 'StoxBioticData'")
  }
  
  if ("Stratum" %in% StratificationColumns){
    stop("'Stratum' is a reserved name that cannot be used as a stratification column")
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
  
  nStrata <- flatStox[,list(nStrata=length(unique(get("Stratum")))), by=SamplingUnitId]
  if (any(nStrata$nStrata>1)){
    stop("Invalid stratification. Some PSUs are in several strata.")
  }
  
  CommonSelectionData <- flatStox[!duplicated(flatStox$SamplingUnitId),list(InclusionProbability=as.numeric(NA), HTsamplingWeight=as.numeric(NA), SelectionProbability=as.numeric(NA), HHsamplingWeight=1/length(unique(SamplingUnitId)), SelectionDescription=as.character(NA)), by=c("Stratum")]
  selectionUnits <- flatStox[,.SD, .SDcol=c("Stratum", "Order", "SamplingUnitId")]
  selectionUnits <- selectionUnits[!duplicated(selectionUnits$SamplingUnitId),]
  
  selectionTable <- merge(selectionUnits, CommonSelectionData, by="Stratum")
  sampleTable <- flatStox[,list(N=as.numeric(NA), n=length(unique(get("SamplingUnitId"))), SelectionMethod="FSWR", FrameDescription=as.character(NA)), by=c("Stratum")]
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
    if (!(n %in% names(colClasses))){
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
    stopifnot(n %in% names(stratificationTable))
    if (any(is.na(stratificationTable[[n]]))){
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
    stratCount <- stratificationTable[,list(nStrata=length(unique(get("Stratum")))), by=list(stratVars=stratificationVariableStrings)]
    duplicatedStrata <- stratCount[get("nStrata")>1,]
    if (nrow(duplicatedStrata)>0){
      stop(paste("Invalid design specification. The stratification variables must uniquely identify a stratum. Duplicates found for:", paste(duplicatedStrata$stratVars, collapse=",")))
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

#' Read PSU Sampling Design Parameters
#' @description 
#'  Read sampling parameters for Primary Sampling Units in multi-stage sampling.
#' @details 
#'  Reads sampling parameters from a tab delimited file with headers corresponding to those listed in 
#'  \code{\link[RstoxFDA]{PSUSamplingParametersData}}. The file format provide the data as one table, so that the information in 'sampleTable' is repeated for each entry in 'selectionTable'.
#'  Any columns not named in \code{\link[RstoxFDA]{PSUSamplingParametersData}} are assumed to be stratification variables.
#'  The conditions listed for the variables in \code{\link[RstoxFDA]{PSUSamplingParametersData}} are checked upon reading the data, and
#'  execution halts with error if any are violated. Consult the examples in this documentation to see how the resource is formatted
#'  with a stratification variable 'Species'.
#'  
#' @param FileName path to sampling parameters
#' @return \code{\link[RstoxFDA]{PSUSamplingParametersData}}
#' @examples
#'  # embedded example file:
#'  exampleFile <- system.file("testresources", 
#'                         "lotteryParameters", 
#'                         "lotteryDesignNSHstrata.txt", package="RstoxFDA")
#'  
#'  # Read example file with StoX
#'  PSUSamplingParametersData <- RstoxFDA::ReadPSUSamplingParameters( 
#'                           FileName=exampleFile)
#'  
#'  # Read example file as flat table, to illustrate formatting
#'  FlatSamplingParametersData <- read.csv(exampleFile, sep="\t")
#' 
#' @export
#' @seealso \code{\link[RstoxFDA]{ComputePSUSamplingParameters}}, \code{\link[RstoxFDA]{AddPsuStratificationVariables}}, \code{\link[RstoxFDA]{AssignPSUSamplingParameters}}, \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} 
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
ReadPSUSamplingParameters <- function(FileName){
  return(parseDesignParameters(FileName))
}

#' @noRd
computePpsParametersStoxBiotic <- function(StoxBioticData, SamplingUnitId, Quota, StratumName, ExpectedSampleSize){
  
  checkMandatory(StoxBioticData, "StoxBioticData")
  checkMandatory(SamplingUnitId, "SamplingUnitId")
  checkMandatory(Quota, "Quota")
  checkMandatory(StratumName, "StratumName")
  checkMandatory(ExpectedSampleSize, "ExpectedSampleSize")
  
  if (length(unique(StoxBioticData$SpeciesCategory$SpeciesCategory))!=1){
    stop(paste("The DefinitionMethod 'ProportionalPoissonSampling', requires only one species category to be present in sample records. Found:", 
               truncateStringVector(unique(StoxBioticData$SpeciesCategory$SpeciesCategory))))
  }
  
  if (!(SamplingUnitId %in% names(StoxBioticData$Haul))){
    stop(paste("The argument 'SamplingUnitId' must identify a variable in the 'Haul'-table of 'StoxBioticData"))
  }
  
  if (any(is.na(StoxBioticData$Sample$CatchFractionWeight))){
    missing <- StoxBioticData$Sample$Sample[is.na(StoxBioticData$Sample$CatchFractionWeight)]
    stop(paste("Cannot computed sampling parameters with missing catch weights, missing for samples: ", truncateStringVector(missing)))
  }
  n <- ExpectedSampleSize
  
  flatBiotic <- RstoxData::mergeByStoxKeys(StoxBioticData$Haul, StoxBioticData$Sample, StoxDataType = "StoxBiotic")
  SelectionTable <- flatBiotic[,list(catchWeight=sum(get("CatchFractionWeight"))), by=list(SamplingUnitId=get(SamplingUnitId))]
  SelectionTable$SelectionProbability <- SelectionTable$catchWeight / Quota
  SelectionTable$HHsamplingWeight <- 1 / (SelectionTable$SelectionProbability * sum(1/SelectionTable$SelectionProbability))
  SelectionTable$InclusionProbability <- 1-((1-SelectionTable$SelectionProbability)**n)
  SelectionTable$HTsamplingWeight <- 1 / (SelectionTable$InclusionProbability * sum(1/SelectionTable$InclusionProbability))
  SelectionTable$Order <- as.numeric(NA)
  SelectionTable$Stratum <- StratumName
  SelectionTable$SelectionDescription <- ""
  SelectionTable <- SelectionTable[,.SD,.SDcol=c("Stratum", "Order", "SamplingUnitId", "InclusionProbability", "HTsamplingWeight", "SelectionProbability", "HHsamplingWeight", "SelectionDescription")]
  
  SampleTable <- data.table::data.table(Stratum=StratumName, N=as.numeric(NA), n=n, SelectionMethod="Poisson", FrameDescription="")
  
  StratificationVariables <- data.table::data.table(Stratum=StratumName)

  samplingParams <- list()
  samplingParams$SampleTable <- SampleTable
  samplingParams$SelectionTable <- SelectionTable
  samplingParams$StratificationVariables <- StratificationVariables
    
  return(samplingParams)
}

#' Compute PSU Sampling Design Parameters
#' @description 
#'  Compute sampling parameters for Primary Sampling Units in multi-stage sampling.
#' @details 
#'  Computes Sampling Design Parameters from data, given some assumptions specified by the 'DefinitionMethod'.
#'  
#'  Primary Sampling Units (the argument 'SamplingUnitId') may be identified by any categorical (character, factor) or ordinal (factor, integer) variable in 'StoxBioticData', that are associated with the 'Sample'-table
#'  or any table above 'Sample' in the StoxBiotic-hierarchy.
#'   
#'  If 'DefinitionMethod' is 'AdHocStoxBiotic', equal probability sampling with fixed sample size will be assumed withing strata, as well as
#'  selection with replacement and complete response.
#'  This is a reasonable approximation if within-strata sampling is approximately simple random selections,
#'  the sample intensitiy is low (only a small fraction of the population is sampled),
#'  and non-response is believed to be random. StratificationVariables may be any categorical or ordinal variables in 'StoxBioticData', that are associated with the 'Sample'-table
#'  or any table above 'Sample' in the StoxBiotic-hierarchy, and strata will be defined as the combination of these variables.
#'  Primary Sampling Units must be selected withing stratum, so the function will fail there are several strata in one PSU.
#'  
#'  If 'DefinitionMethod' is 'ProportionalPoissonSampling', Unstratified (singe stratum) Poission sampling with selection probabilities
#'  proportional to catch size is assumed. 'SamplingUnitId' must be a variable on the Haul table of 'StoxBioticData' for this option,
#'  and the data must contain only one species (SpeciesCategory in 'StoxBioticData'). SelectionProbabilities are assigned
#'  based on the total catch of the species in each haul. Specifically, for a haul \eqn{i}; selectionprobabilites, \eqn{p_{i}} and inclusionprobabilities \eqn{\pi_{i}}
#'  are calculated as:
#'  
#'  \deqn{p_{i}=\frac{w_{i}}{W}}
#'  
#'  \deqn{\pi_{i}=1-(1-p_{i})^{n}}
#'  
#'  where:
#'  \itemize{ 
#'    \item \eqn{w_{i}} is the sum of all catch weights in haul \eqn{i} ('CatchFractionWeight' on the 'Sample' table of 'StoxBioticData') 
#'    \item \eqn{W} is the expected total catch in the fishery (argument 'Quota')
#'    \item \eqn{n} is the expected sample size (argument 'ExpectedSampleSize')
#'  }
#'  If proportional poisson sampling was actually used to select the sampled records in 'StoxBioticData',
#'  sampling parameters would have been obtained prior to sampling, and it is generally preferable to obtain these, 
#'  and import those via \code{\link[RstoxFDA]{ReadPSUSamplingParameters}}. Weight-records are sometimes corrected after
#'  sampling parameters are calculated, and proper information about non-response can not be recalculated after the fact.
#'  
#'  Proportional poisson sampling also allows the sampler to combine rigour and pragmatism, by varying sampling parameters
#'  in the course of sample selection. For instance 'n' may be changed during the sampling period, if non-response 
#'  turns out to be higher than expected. Such flexibilities are not
#'  provided by this function, and the approximation may be severely compromised, if such pragmatism is not accounted for.
#'  
#' @param DefinitionMethod 'AdHocStoxBiotic' or 'ProportionalPoissonSampling'
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} Sample data to construct design parameters from
#' @param SamplingUnitId name of column in 'StoxBioticData' that identifies the Primary Sampling Unit the design is constructed for.
#' @param StratificationColumns name of columns that are to be used to define Strata for sampling. (for DefinitionMethod 'AdHocStoxBiotic'). See \code{\link[RstoxFDA]{PSUSamplingParametersData}}
#' @param StratumName name of the stratum sampling parameters are calculated for (for DefinitionMethod 'ProportionalPoissonSampling')
#' @param Quota expected total catch in sampling frame in kg (for DefinitionMethod 'ProportionalPoissonSampling')
#' @param ExpectedSampleSize the expected sample size for Possion sampling (for DefinitionMethod 'ProportionalPoissonSampling')
#' @return \code{\link[RstoxFDA]{PSUSamplingParametersData}}
#' @examples
#'  # parameters for simple random haul-selection, stratified by GearGroup
#'  PSUparams <- ComputePSUSamplingParameters(RstoxFDA::StoxBioticDataExample, 
#'   "AdHocStoxBiotic", 
#'   "Haul", 
#'   "GearGroup")
#'   
#'  # parameters for haul selection proportional to catch size.
#'  calculatedPps <- RstoxFDA::ComputePSUSamplingParameters(RstoxFDA::CatchLotteryExample, 
#'     "ProportionalPoissonSampling", 
#'     "serialnumber", StratumName = 
#'     "Nordsjo", Quota = 124*1e6, 
#'     ExpectedSampleSize = 110)
#' 
#' @seealso \code{\link[RstoxFDA]{ReadPSUSamplingParameters}}, \code{\link[RstoxFDA]{AddPsuStratificationVariables}}, \code{\link[RstoxFDA]{AssignPSUSamplingParameters}}, \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} 
#' @export
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
ComputePSUSamplingParameters <- function(StoxBioticData, DefinitionMethod=c("AdHocStoxBiotic", "ProportionalPoissonSampling"), SamplingUnitId=character(), StratificationColumns=character(), StratumName=character(), Quota=numeric(), ExpectedSampleSize=numeric()){

  DefinitionMethod <- checkOptions(DefinitionMethod, "DefinitionMethod", c("AdHocStoxBiotic", "ProportionalPoissonSampling"))

  if (DefinitionMethod=="AdHocStoxBiotic"){
    return(assumeDesignParametersStoxBiotic(StoxBioticData, SamplingUnitId, StratificationColumns))    
  }
  else if (DefinitionMethod=="ProportionalPoissonSampling"){
    return(computePpsParametersStoxBiotic(StoxBioticData, SamplingUnitId, Quota, StratumName, ExpectedSampleSize))    
  }
  else{
    stop(paste("The option", DefinitionMethod, "is not recognized for the argument 'DefinitionMethod'"))
  }

}

is.StratificationVariablesData <- function(StratificationVariablesTable){
  if(!data.table::is.data.table(StratificationVariablesTable)){
    return(FALSE)
  }
  if (!("Stratum" %in% names(StratificationVariablesTable))){
    return(FALSE)
  }
  return(TRUE)
}

#' Add Stratification columns to 'PSUSamplingParametersData'
#' @description
#'  Add additional variables to encode strata and its correspondance with census data (e.g. landings data).
#' @details
#'  \code{\link[RstoxFDA]{PSUSamplingParametersData}} provide sampling parameters by strata.
#'  Optionally, it may also contain additional variables that encode the stratification in terms of variables
#'  available in other data sources, such as \code{\link[RstoxData]{StoxLandingData}}. This function allows
#'  such variables to be added, if not already present.
#'  
#'  More detailed encoding of stratification is useful for 
#'  encoding the sampling frame of the design provided by 'PSUSamplingParametersData'. By encoding all strata
#'  in terms of variables that are available in census-data, the correspondance between sampling frame and
#'  target population can be encoded. This information will be available in downstream estimates (e.g.
#'  \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}) and allow for pragmatic inference to
#'  out-of-frame strata (via \code{\link[RstoxFDA]{ExtendAnalyticalSamplingFrameCoverage}}).
#' 
#' @param PSUSamplingParametersData Sampling parameters stratification variables should be added to
#' @param StratificationVariables name of variables to add
#' @param StratificationVariablesTable value-combinations for the variables to add to each stratum
#' @return \code{\link[RstoxFDA]{PSUSamplingParametersData}}
#' @export
#' @seealso \code{\link[RstoxFDA]{ReadPSUSamplingParameters}}, \code{\link[RstoxFDA]{ComputePSUSamplingParameters}}, \code{\link[RstoxFDA]{AssignPSUSamplingParameters}}, \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} 
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
AddPsuStratificationVariables <- function(PSUSamplingParametersData, StratificationVariables, StratificationVariablesTable=data.table::data.table()){
  
  checkMandatory(PSUSamplingParametersData, "PSUSamplingParametersData")
  checkMandatory(StratificationVariables, "StratificationVariables")
  checkMandatory(StratificationVariablesTable, "StratificationVariablesTable")
  
  if (length(names(PSUSamplingParametersData$StratificationVariables))>1){
    stop("'PSUSamplingParametersData' already has StratificationVariables")
  }
  
  if (!is.StratificationVariablesData(StratificationVariablesTable)){
    stop("Invalid 'StratificationVariablesTable'.")
  }
  
  if (!all(StratificationVariablesTable$Stratum %in% PSUSamplingParametersData$StratificationVariables$Stratum)){
    missing <- StratificationVariablesTable$Stratum[!(StratificationVariablesTable$Stratum %in% PSUSamplingParametersData$StratificationVariables$Stratum)]
    stop(paste("Not all strata in 'StratificationVariablesTable' exist in 'PSUSamplingParametersData'. Missing", truncateStringVector(missing)))
  }
  
  if (!all(PSUSamplingParametersData$StratificationVariables$Stratum %in% StratificationVariablesTable$Stratum)){
    missing <- PSUSamplingParametersData$StratificationVariables$Stratum[!(PSUSamplingParametersData$StratificationVariables$Stratum %in% StratificationVariablesTable$Stratum)]
    stop(paste("Stratification variables are not provided for strata:", truncateStringVector(missing)))
  }
  
  if (!all(StratificationVariables %in% names(StratificationVariablesTable))){
    stop("Not all StratificationVariables are in the StratificationVariablesTable")
  }
  if (!all(names(StratificationVariablesTable) %in% c("Stratum", StratificationVariables))){
    stop("Some StratificationVariables are not in the StratificationVariablesTable")
  }
  
  stratCount <- StratificationVariablesTable[,list(strata=length(unique(get("Stratum")))), by=list(stratVarString=apply(StratificationVariablesTable[,.SD,.SDcol=names(StratificationVariablesTable)[names(StratificationVariablesTable) != "Stratum"]], 1, paste, collapse="/"))]
  manyStrata <- stratCount[get("strata")>1,]
  if (nrow(manyStrata)>0){
    stop(paste("Stratification variables does not identify strata. Several strata overlap with:", truncateStringVector(manyStrata$stratVarString)))
  }

  PSUSamplingParametersData$StratificationVariables <- merge(PSUSamplingParametersData$StratificationVariables,
                                                             StratificationVariablesTable,
                                                             by="Stratum")
  
  return(PSUSamplingParametersData)
  
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
  
  NselectionMethods <- designParam$SampleTable[,list(NselMet=length(unique(get("SelectionMethod")))), by=c("SampleId", "newStratum")]
  if (any(NselectionMethods$NselMet>1)){
    stop("Cannot collapse strata with heterogenous selection methods.")
  }

  totalN <- designParam$SampleTable[,list(totalN=sum(get("N")), allSampled=all(get("n")>0), totalStrata=length(unique(get("Stratum")))), by=c("SampleId", "newStratum")]
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

  designParam$SampleTable <- designParam$SampleTable[,list(N=sum(get("N")), n=sum(get("n")), SelectionMethod=get("SelectionMethod")[1], SampleDescription=as.character(NA)), by=c("SampleId", "Stratum")]
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
  stratumTotals <- individuals[,list(totalInStratum=get(".N"), sampledInStratum=sum(get("Sampled"))), by=c("Stratum", "SampleId")]
  sampleTotals <- individuals[,list(totalInSample=get(".N")), by=c("SampleId")]
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

#' Compute Sampling Parameters for Individuals
#' @description 
#'  Compute approximate sampling parameters for the selection of individuals from a haul. Design parameters are inferred from data provided in ~\code{\link[RstoxData]{StoxBioticData}},
#'  and specify how a set of individuals recorded on the Individual table were selected for observation/measurement from a Haul (the table Haul in StoxBioticData).
#' @details 
#'  StoxBioticData represents sorting of species as a separate level in the hierarchy (SpeciesCategory) and Samples are selected stratified by the species categories.
#'  This represent sampling stratified on taxons in addition to some additional stratification criteria in the cases where more than one sample is present for
#'  a species-category in a Haul. The exact criteria for stratification is not important for the calculation of sampling parameters, but only clearly encoded criteria can be used
#'  in subsequent analysis, so sampling parameters are reported stratified only on SpeciesCategory. Any other stratification has been incorporated into selection or inclusion probabilities.
#'  
#'  Sampling parameters are approximately inferred, assuming that all selected individuals are recorded, and based on some user-controllable assumptions about the selection process,
#'  specified by the appropriate 'DefinitionMethod'. 
#'  
#'  Individuals with a non-missing value for any of the parameters in 'Parameters' are treated as selected for observation,
#'  and their sampling probabilities are calculated in accordance with assumptions encoded in 'DefinitionMethod'.
#'  In this way selection of individuals may be specified differently for different parameters. One could for instance compute 
#'  one \code{\link[RstoxFDA]{IndividualSamplingParametersData}} reflecting the probability of selecting individuals for length-measurment,
#'  and another for selecting more detailed measurements, e.g. weight, age, sex, and maturity. The individual is identified as selected for measurement
#'  if ANY of the parameters in 'Parameters' has a value. This allows for approximate inference of missing observations; as long as
#'  one of the variables selected for observation is recorded, missing values in any of the other is inferred. 
#'  An assumption that a certain variable is missing at random can be encoded by only specifying this variable in 'Parameters'.
#'  For instance for catch-at-age estimation, if only the variable 'IndividualAge' is specified in 'Parameters', individuals with missing ages
#'  will not be assigned sampling parameters, and implicitly be assumed to be missing at random. Subsequent analysis will then not have
#'  NA-age groups or NAs in mean age estimates.
#'  
#'  The sample size 'n' is in all cases inferred by counting the total number of fish in the sample that meets the selection criteria (non-missing value for at least one of the 'Parameters')
#'  The total number of fish in the catch sampled, 'N' is the CatchFractionNumber in StoxBioticData.
#'  When fish selection is stratified by fish-properties (e.g. DefinitionMethod 'Stratified' or 'LengthStratified), the fraction in the stratum, 'f' is estimated by the fraction of fish in that stratum in the sample.
#'  
#'  Inclusion probabilities are then set to n*f/N, and Normalized Horvitz-Thompson sampling weights are calculated (see \code{\link[RstoxFDA]{IndividualSamplingParametersData}}). 
#'  
#'  The available DefinitionMethods specifies how Individuals are selected from a Sample, and are:
#'  \describe{
#'   \item{SRS}{Simple Random Selection. Individuals are selected for measurment by simple random selection without replacement from each Sample.}
#'   \item{Stratified}{Stratified Selection. Individuals are selected for measurement by stratified random selection without replacement from each Sample. Strata are specified as the combination of columns provided in 'StratificationColumns'. The number of fish in each stratum is estimated by the total in sample and the proportion of measured fish in each stratum.}
#'   \item{LengthStratified}{Length stratified selection. Individuals are selected for measurement by stratified random selection without replacement from each Sample. Strata are length groups, specified by the left closed intervals starting with [0,'LengthInterval'>.}
#'  }
#'  
#'  
#' @param StoxBioticData Data to define individual sampling parameters for
#' @param DefinitionMethod Method to infer sampling parameters, 'SRS', 'Stratified' or 'LengthStratified'. See details.
#' @param Parameters Measurements / observations of individuals included in the design specification. Must all be columns on the Individual-table of StoxBioticData. 
#' @param LengthInterval width of length strata in cm. Specifies left closed intervals used for Length Stratified selection (DefinitionMethod 'Stratified'). A value of 5 indicates that observation are selected stratified on length groups [0 cm,5 cm>, [5 cm, 10 cm>, and so on.
#' @param StratificationColumns names of columns in the Individual table of StoxBioticData that identify strata for Stratified selection (DefinitionMethod 'Stratified').
#' @return \code{\link[RstoxFDA]{IndividualSamplingParametersData}} where SampleId refers to the variable 'Haul' on the 'Haul' table in StoxBioticData, and IndividualId refers to the variable 'Individual' on the 'Individual' table of StoxBioticData.
#' @export
#' @seealso \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
ComputeIndividualSamplingParameters <- function(StoxBioticData, DefinitionMethod=c("SRS", "Stratified", "LengthStratified"), Parameters=character(), LengthInterval=numeric(), StratificationColumns=character()){

  #May want to expose this option if DefinitionMethods are added that only provides relative selection probabilities.
  CollapseStrata=FALSE
  
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
#' @noRd
DefineSamplingHierarchy <- function(StoxBioticData, IndividualSamplingParametersData, Hierarchy=character(), Stratification=character(), StrataSizes=character(), SelectionMethod=character()){
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
#'  by an ID (argument 'DataRecordId') so that sampling units can be brought into correspondence with how they are identified in lower
#'  level sampling (\code{\link[RstoxFDA]{IndividualSamplingParametersData}})
#'  
#'  If any respondents (rows of the SelectionTable of PSUSamplingParametersData that does not have NA for SamplingUnitId) are not
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
#' @seealso \code{\link[RstoxFDA]{ReadPSUSamplingParameters}}, \code{\link[RstoxFDA]{ComputePSUSamplingParameters}}, \code{\link[RstoxFDA]{AddPsuStratificationVariables}}, \code{\link[RstoxFDA]{ComputeIndividualSamplingParameters}}, \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} 
#' @md
#' @export
AssignPSUSamplingParameters <- function(PSUSamplingParametersData, StoxBioticData, SamplingUnitId=character(), DataRecordId=character(), DefinitionMethod=c("MissingAtRandom")){
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
    missing <- records[!(records %in% StoxBioticData[[level]][[SamplingUnitId]])]
    stoxWarning(paste("Records are not found for all sampled PSUs. These will be treated as missing. Records missing for the following SamplingUnitIds (", SamplingUnitId,"): ", truncateStringVector(missing), sep=""))
    PSUSamplingParametersData$SelectionTable$SamplingUnitId[!is.na(PSUSamplingParametersData$SelectionTable$SamplingUnitId) &
                                                              !(PSUSamplingParametersData$SelectionTable$SamplingUnitId %in% StoxBioticData[[level]][[SamplingUnitId]])] <- NA
  }
  
  if (DefinitionMethod == "MissingAtRandom"){
    responsePropensity <- PSUSamplingParametersData$SelectionTable[,list(ResponsePropensity=sum(!is.na(get("SamplingUnitId")))/get(".N")), by=c("Stratum")]
    
    PSUSamplingParametersData$SampleTable$n <- PSUSamplingParametersData$SampleTable$n * responsePropensity$ResponsePropensity[match(PSUSamplingParametersData$SampleTable$Stratum, responsePropensity$Stratum)]
    
    # correct sampling probabilities
    PSUSamplingParametersData$SelectionTable$InclusionProbability <- PSUSamplingParametersData$SelectionTable$InclusionProbability * responsePropensity$ResponsePropensity[match(PSUSamplingParametersData$SelectionTable$Stratum, responsePropensity$Stratum)]
    
    #remove non respondants
    PSUSamplingParametersData$SelectionTable <- PSUSamplingParametersData$SelectionTable[!is.na(PSUSamplingParametersData$SelectionTable$SamplingUnitId)]
    
    #correct normalized sampling weights
    weights <- PSUSamplingParametersData$SelectionTable[,list(HHsum=sum(get("HHsamplingWeight")), HTsum=sum(get("HTsamplingWeight"))), by=c("Stratum")]
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
#'  with the function \code{\link[RstoxFDA]{ComputeIndividualSamplingParameters}}.
#' 
#'  If any strata are specified in the SampleTable of 'IndividualSamplingParametersData' but are not sampled per the SelectionTable
#'  all estimates will be provided as NAs for this stratum.
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
#'   The estimate of the total of a variable \eqn{v} in stratum \eqn{s} and domain \eqn{d} at a PSU:
#'   \deqn{\hat{t}^{(s,d,v)}=\sum_{i=1}^{m}\frac{y^{v}_{i}}{\pi_{i}}I^{s,d}_{i}}
#'   }
#'   The inclusion probability is a function of the entire sample selection for a stratum.
#'   If the domain does not coincide with stratum, it must be considered approximate and hence this will be a ratio estimation in that case.
#'   \item{Mean:}{
#'   The mean value of a variable \eqn{v} in stratum \eqn{s} and domain \eqn{d} at a PSU:
#'   \deqn{\hat{\mu}^{(s,d,v)}=\frac{1}{\hat{D}^{(s,d)}}\sum_{i=1}^{m}w_{i}y^{v}_{i}I^{s,d}_{i}}
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
#'    \item{\eqn{y^{v}_{i}}}{The value of a variable \eqn{v} observed for an individual \eqn{i}.}
#'    \item{\eqn{\hat{D}^{(s,d)}}}{The estimated relative domain size of domain \eqn{d} in stratum \eqn{s} at PSU: \eqn{\sum_{i=1}^{m}w_{i}I^{s,d}_{i}}}
#'  }
#'  
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} with the actual observations of individuals.
#' @param IndividualSamplingParametersData \code{\link[RstoxFDA]{IndividualSamplingParametersData}} with sampling parameters for individuals
#' @param Variables names of variables that means and totals should be estimated for. Must be numeric columns of the Individual table in 'StoxBioticData'
#' @param DomainVariables names of variables that define domains of individuals that estimates should be made for. Must be columns of 'Individual' or some higher level table in 'StoxBioticData'.
#' @param PSUDomainVariables names of variables that define groups of PSUs to be annotated on the results for later processing. Must be columns of 'Individual' or some higher level table in 'StoxBioticData', and must have a unique value for each PSU.
#' @return \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}} with estimates for each PSU of abundance, frequencies, totals and means by stratum and domain.
#' @concept Analytical estimation
#' @seealso \code{\link[RstoxFDA]{ComputeIndividualSamplingParameters}}
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
  
  samplecount <- ind[,list(nIndividuals=length(unique(get("Individual")))), by=c("SampleId", "Stratum", "Domain")]
  
  abundance <- ind[,list(Abundance=sum(1/get("InclusionProbability")), Frequency=sum(get("HTsamplingWeight"))), by=c("SampleId", "Stratum", "Domain")]

  #add zero domains
  allDomains <- data.table::CJ(SampleId=IndividualSamplingParametersData$SampleTable$SampleId, Domain=domaintable$Domain, unique = T)
  allDomains <- merge(allDomains, IndividualSamplingParametersData$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum")], by="SampleId", allow.cartesian=T)
  missingDomainsAbundance <- allDomains[!(paste(get("SampleId"), get("Stratum"), get("Domain")) %in% paste(abundance$SampleId, abundance$Stratum, abundance$Domain)),]
  missingDomainsAbundance$Abundance <- 0
  missingDomainsAbundance$Frequency <- 0
  
  #add NAs for non-sampled strata
  missingDomainsAbundance$Abundance[!(paste(missingDomainsAbundance$SampleId, missingDomainsAbundance$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomainsAbundance$Frequency[!(paste(missingDomainsAbundance$SampleId, missingDomainsAbundance$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomainsAbundance <- missingDomainsAbundance[,.SD,.SDcol=names(abundance)]
  abundance <- rbind(abundance, missingDomainsAbundance)
  
  #add 0 for all missing domains for samplecount
  missingDomainsSampleCount <- allDomains[!(paste(get("SampleId"), get("Stratum"), get("Domain")) %in% paste(samplecount$SampleId, samplecount$Stratum, samplecount$Domain)),]
  missingDomainsSampleCount$nIndividuals <- 0
  missingDomainsSampleCount <- missingDomainsSampleCount[,.SD,.SDcol=names(samplecount)]
  samplecount <- rbind(samplecount, missingDomainsSampleCount)
  
  
  estimates <- NULL
  for (v in Variables){
    est <- ind[,list(Variable=v, Total=sum(get(v)/get("InclusionProbability")), Mean=sum(get(v)*get("HTsamplingWeight"))/sum(get("HTsamplingWeight"))), by=c("SampleId", "Stratum", "Domain")]

    est$Variable <- v
    estimates <- rbind(estimates, est)
  }

  #add zero domains
  allDomains <- data.table::CJ(SampleId=IndividualSamplingParametersData$SampleTable$SampleId, Domain=domaintable$Domain, Variable=estimates$Variable, unique = T)
  allDomains <- merge(allDomains, IndividualSamplingParametersData$StratificationVariables[,.SD, .SDcol=c("SampleId", "Stratum")], by="SampleId", allow.cartesian=T)
  missingDomains <- allDomains[!(paste(get("SampleId"), get("Stratum"), get("Domain"), get("Variable")) %in% paste(estimates$SampleId, estimates$Stratum, estimates$Domain, estimates$Variable)),]
  missingDomains$Total <- 0
  missingDomains$Mean <- NaN
  #add NA for non-sampled strata
  missingDomains$Total[!(paste(missingDomains$SampleId, missingDomains$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomains$Mean[!(paste(missingDomains$SampleId, missingDomains$Stratum) %in% paste(IndividualSamplingParametersData$SelectionTable$SampleId, IndividualSamplingParametersData$SelectionTable$Stratum))] <- as.numeric(NA)
  missingDomains <- missingDomains[,.SD,.SDcol=names(estimates)]
  estimates <- rbind(estimates, missingDomains)
  
  output <- list()
  output$Abundance <- abundance[order(get("SampleId"), get("Stratum"), get("Domain")),]
  output$Variables <- estimates[order(get("SampleId"), get("Stratum"), get("Domain"), get("Variable")),]
  output$DomainVariables <- domaintable[order(get("Domain")),]
  output$PSUDomainVariables <- PSUdomains
  output$StratificationVariables <- IndividualSamplingParametersData$StratificationVariables[order(get("SampleId"), get("Stratum")),]
  output$SampleCount <- samplecount

  return(output)
}

#' Collapse strata in sampling design
#' @description
#'  Transforms a sampling design to an equivalent one with simpler stratification
#' @details 
#'  The sampling information in \code{\link[RstoxFDA]{IndividualSamplingParametersData}} allows for specification of several stratification variables.
#'  This function facilitates removal of some of these stratificaiton vairables, redefining strata to a coarser stratification scheme that still has known
#'  stratum sizes.
#'  
#'  If all stratification variables are removed, all samples will be assigned to a single stratum named 'All'.
#'  
#' @param IndividualSamplingParametersData \code{\link[RstoxFDA]{IndividualSamplingParametersData}} with sampling parameters for sample selection
#' @param RetainStrata character() with the names of stratification variables to retain. Stratification variables not specified here will be removed.
#' @return \code{\link[RstoxFDA]{IndividualSamplingParametersData}} with simplified stratification
#' @concept Analytical estimation
#' @md
#' @export
CollapseStrata <- function(IndividualSamplingParametersData, RetainStrata=character()){
  
  checkMandatory(IndividualSamplingParametersData, "IndividualSamplingParametersData")
  
  missing <- RetainStrata[!(RetainStrata %in% names(IndividualSamplingParametersData$StratificationVariables))]
  if (length(missing)>0){
    stop(paste("The variables", paste(missing, collapse=", ", "are not stratification variables in 'IndividualDesignParametersData")))
  }
  
  collapseStrata <- names(IndividualSamplingParametersData$StratificationVariables)[!names(IndividualSamplingParametersData$StratificationVariables) %in% c(RetainStrata, "Stratum", "SampleId")]
  return(collapseStrataIndividualDesignParamaters(IndividualSamplingParametersData, collapseVariables = collapseStrata))
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

  if (!is.AnalyticalPSUEstimateData(AnalyticalPSUEstimateData)){
    stop("Invalid 'AnalyticalPSUEstimateData'")
  }
  
  allStrata <- data.table::CJ(SampleId=AnalyticalPSUEstimateData$StratificationVariables$SampleId, Stratum=AnalyticalPSUEstimateData$StratificationVariables$Stratum, unique = T)
  missingStrata <- allStrata[!(paste(allStrata$SampleId, allStrata$Stratum) %in% paste(AnalyticalPSUEstimateData$StratificationVariables$SampleId, AnalyticalPSUEstimateData$StratificationVariables$Stratum)),]
  
  domains <- data.table::CJ(Stratum=AnalyticalPSUEstimateData$StratificationVariables$Stratum, Domain=AnalyticalPSUEstimateData$DomainVariables$Domain, unique = T)
  missingStrataWdomains <- merge(missingStrata, domains, by="Stratum", allow.cartesian = T)
  
  SampleCountAdditions <- missingStrataWdomains
  SampleCountAdditions$nIndividuals <- 0
  
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
  
  stratvars <- AnalyticalPSUEstimateData$StratificationVariables[!duplicated(AnalyticalPSUEstimateData$StratificationVariables$Stratum),.SD,.SDcol=names(AnalyticalPSUEstimateData$StratificationVariables)[names(AnalyticalPSUEstimateData$StratificationVariables)!="SampleId"]]
  StratVarAdditions <- merge(missingStrata, stratvars, by="Stratum")
  StratVarAdditions <- StratVarAdditions[,.SD,.SDcol=names(AnalyticalPSUEstimateData$StratificationVariables)]
  
  AnalyticalPSUEstimateData$Abundance <- rbind(AnalyticalPSUEstimateData$Abundance, AbundanceAdditions)
  AnalyticalPSUEstimateData$Abundance <- AnalyticalPSUEstimateData$Abundance[order(AnalyticalPSUEstimateData$Abundance$SampleId, AnalyticalPSUEstimateData$Abundance$Stratum, AnalyticalPSUEstimateData$Abundance$Domain),]
  AnalyticalPSUEstimateData$Variables <- rbind(AnalyticalPSUEstimateData$Variables, VariableAdditions)
  AnalyticalPSUEstimateData$Variables <- AnalyticalPSUEstimateData$Variables[order(AnalyticalPSUEstimateData$Variables$SampleId, AnalyticalPSUEstimateData$Variables$Stratum, AnalyticalPSUEstimateData$Variables$Domain),]
  AnalyticalPSUEstimateData$StratificationVariables <- rbind(AnalyticalPSUEstimateData$StratificationVariables, StratVarAdditions)
  AnalyticalPSUEstimateData$StratificationVariables <- AnalyticalPSUEstimateData$StratificationVariables[order(AnalyticalPSUEstimateData$StratificationVariables$SampleId, AnalyticalPSUEstimateData$StratificationVariables$Stratum),]
  
  AnalyticalPSUEstimateData$SampleCount <- rbind(AnalyticalPSUEstimateData$SampleCount, SampleCountAdditions)
  AnalyticalPSUEstimateData$SampleCount <- AnalyticalPSUEstimateData$SampleCount[order(AnalyticalPSUEstimateData$SampleCount$SampleId, AnalyticalPSUEstimateData$SampleCount$Stratum, AnalyticalPSUEstimateData$SampleCount$Domain),]
  
  return(AnalyticalPSUEstimateData)
}

covarAbundance <- function(Totals, PSUSampling, MeanOfMeans){
  
  sampleSize <- PSUSampling[,list(n=length(unique(get("SamplingUnitId")))), by="Stratum"]
  
  covars <- NULL
  for (PSUstrat in unique(PSUSampling$Stratum.PSU)){
    stratatable <- merge(PSUSampling[PSUSampling$Stratum.PSU==PSUstrat,], Totals, by=c("Stratum", "Domain"), suffixes=c(".PSU", ".Total"))

    for (PSUDom in unique(stratatable$PSUDomain)){
      table <- stratatable[stratatable$PSUDomain == PSUDom,]

      relPSUDomainSize <- sum(table$HHsamplingWeight[!duplicated(table$SamplingUnitId)])
      relDomainSizes <- table[,list(relDomainSize=sum(get("HHsamplingWeight")[!duplicated(get("SamplingUnitId"))])), by=c("Stratum", "Domain")]
      
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
      
      sumOfProducts <- cross[,list(AbundanceSOP=sum(get("AbundanceDevProduct")), FrequencySOP=sum(get("FrequencyDevProduct"))), by=c("Stratum", "Domain1", "Domain2")]
      sumOfProducts <- merge(sumOfProducts, sampleSize, by="Stratum")
      covar <- sumOfProducts[,list(AbundanceCovariance=get("AbundanceSOP")*(1/(get("n")*(get("n")-1)))*(1/get("relPSUDomainSize")**2), FrequencyCovariance=get("FrequencySOP")*(1/(get("n")*(get("n")-1)))*(1/get("relPSUDomainSize")**2)), by=c("Stratum", "Domain1", "Domain2")]
      
      if (!MeanOfMeans){
        StrataAbundance <- Totals[,list(StrataAbundance=sum(get("Abundance"))), by="Stratum"]
        covar$FrequencyCovariance <- covar$AbundanceCovariance * (1/StrataAbundance$StrataAbundance[match(covar$Stratum, StrataAbundance$Stratum)])**2
      }
      
      covars <- rbind(covars, covar)    
    }
    
  }
  
  return(covars)
}

covarVariables <- function(Totals, PSUSampling, MeanOfMeans, Abundance){
  
  
  sampleSize <- PSUSampling[,list(n=length(unique(get("SamplingUnitId")))), by="Stratum"]
  
  covars <- NULL
  for (PSUstrat in unique(PSUSampling$Stratum.PSU)){
    strataTable <- merge(PSUSampling[PSUSampling$Stratum.PSU==PSUstrat,], Totals, by=c("Stratum", "Domain", "Variable"), suffixes=c(".PSU", ".Total"))
    
    for (PSUDom in unique(strataTable$PSUDomain)){
      table <- strataTable[strataTable$PSUDomain == PSUDom,]
      
      relPSUDomainSize <- sum(table$HHsamplingWeight[!duplicated(table$SamplingUnitId)])
      relDomainSizes <- table[,list(relDomainSize=sum(get("HHsamplingWeight")[!duplicated(get("SamplingUnitId"))])), by=c("Stratum", "Domain")]
      
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
      
      sumOfProducts <- cross[,list(TotalSOP=sum(get("TotalDevProduct")), 
                                   MeanSOP=sum(get("MeanDevProduct")[get("coSampled")==1]), 
                                   Freq1=sum(get("HHsamplingWeight1")[get("coSampled")==1]), 
                                   Freq2=sum(get("HHsamplingWeight2")[get("coSampled")==1])), 
                             by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]
      sumOfProducts <- merge(sumOfProducts, sampleSize, by="Stratum")
      
      covar <- sumOfProducts[,list(TotalCovariance=get("TotalSOP")*(1/(get("n")*(get("n")-1)))*(1/get("relPSUDomainSize")**2), 
                                   MeanCovariance=(1/get("Freq1")**2)*get("MeanSOP")/(get("n")*(get("n")-1))), 
                             by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]
      
      #if (!MeanOfMeans){
      #  m1 <- match(paste(covar$Stratum, covar$Domain1), paste(Abundance$Stratum, Abundance$Domain))
      #  m2 <- match(paste(covar$Stratum, covar$Domain2), paste(Abundance$Stratum, Abundance$Domain))
      #  covar$MeanCovariance <- covar$TotalCovariance * (1/Abundance$Abundance[m1]) * (1/Abundance$Abundance[m2])
      #}

      
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
#'  Means may be unknown for a certain domain because the estimate of abundance and frequencies for the domain is be zero. In this case, 
#'  unknown means are provided as NaNs, as they result from dividing by zero. 
#'  Means may also be unkown because either PSU means or sampling parameters are unknown, or the strata is not sampled.
#'  In this case means will be provided as NA.
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
#'  In general unbiased estimates rely on known selection probabilities, and domain definitions that coincides
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
#'  Estimates of PSU domain sizes are provided without variance-estimations. These are also documented in the vocabulary below.
#'  
#'  \describe{
#'   \item{Abundance:}{
#'   The estimate of the total number of individuals in domain \eqn{d} and stratum \eqn{s}:
#'   \deqn{\hat{N}^{(s,d)} = \frac{1}{n^{(s)}} \sum_{i=1}^{n} \hat{D}^{(s,d)}\frac{\hat{N}^{(s,d)}_{i}}{p_{i}}I^{(s,d)}_{i}} 
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})}) = \frac{1}{\hat{P}^{(s,d_{1})}\hat{P}^{(s,d_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)} 
#'      \sum_{i=1}^{n} I^{(s,d_{1})}_{i} I^{(s,d_{2})}_{i} z_{i}^{(s,d_{1})} z_{i}^{(s,d_{2})}}
#'    \deqn{z_{i}^{(s,d)} = \hat{D}^{(s,d)}\frac{\hat{N}^{(s,d)}_{i}}{p_{i}}I^{(s,d)}_{i} - \hat{N}^{(s,d)}}
#'   
#'   Covariance can only be calculated when \eqn{p_{i}} is provided, and will otherwise be NA.}
#'   
#'   Note that when \eqn{d_{1}} and \eqn{d_{2}} are different PSU domains, the covariance is zero, and when they are the same PSU domain \eqn{\hat{P}^{(s,d_{1})}=\hat{P}^{(s,d_{2})}}.
#'   
#'   In the general case \eqn{\hat{D}^{(s,d)}} and \eqn{\hat{P}^{(s,d)}} are estimated by ratio estimators, and the error of those estimates are ignored. When \eqn{d} covers all of \eqn{s}, \eqn{\hat{D}^{(s,d)}=1} and \eqn{\hat{P}^{(s,d)}=1} is known. 
#'   In this case both expressions can be shown to be unbiased. In addition the estimate may depend on any ratio estimation for \eqn{\hat{N}^{(s,d)}}. See \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}. 
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
#'   I^{(s,d_{1})}_{i} I^{(s,d_{2})}_{i} z_{i}^{s,d_{1}} z_{i}^{s,d_{2}}}
#'   
#'   \deqn{z_{i}^{(s,d)} = \hat{f}_{i}I^{(s,d)}_{i} - \hat{f}^{(s,d)}}
#'   
#'   Note that when \eqn{d_{1}} and \eqn{d_{2}} are different PSU domains, the covariance is zero, and when they are the same PSU domain \eqn{\hat{P}^{(s,d_{1})}=\hat{P}^{(s,d_{2})}}.
#'   
#'   These are ratio estimates depending on the ratio ratio estimation of \eqn{\hat{f}^{(s,d)}}, \eqn{\hat{f}^{(s,d)}_{i}}, \eqn{\hat{D}^{(s,d)}}, and \eqn{\hat{P}^{(s,d)}}, and the error in these estimates are ignored.
#'   See comments above (\eqn{\hat{f}^{(s,d)}}) and in \code{\link[RstoxFDA]{AnalyticalPSUEstimate}} (\eqn{\hat{f}^{(s,d)}_{i}}).
#'   }
#'   \item{Total:}{
#'   The estimate of the total value of some variable \eqn{v} in domain \eqn{d} and stratum \eqn{s}.
#'   \deqn{\hat{t}^{(s,d,v)}=\frac{1}{n^{(s,d)}}{\sum_{i=1}^{n}}\hat{D}^{(s,d)}\frac{\hat{t}^{(s,d,v)}_{i}}{p_{i}}I^{(s,d)}_{i}}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{t}^{(s,d_{1},v_{1})}, \hat{t}^{(s,d_{2},v_{2})}) = \frac{1}{\hat{P}^{(s,d_{1})}\hat{P}^{(s,d_{2})}}\frac{1}{n^{(s)}(n^{(s)}-1)} \sum_{i=1}^{n} 
#'   I^{(s,d_{1})}_{i} I^{(s,d_{2})}_{i} z_{i}^{(s,d_{1},v_{1})} z_{i}^{(s,d_{2},v_{2})} }.
#'   \deqn{z_{i}^{(s,d,v)} = \hat{D}^{(s,d)}\frac{\hat{t}^{(s,d,v)}_{i}}{p_{i}}I^{(s,d)}_{i} - \hat{t}^{(s,d,v)}}
#'   
#'   Note that when \eqn{d_{1}} and \eqn{d_{2}} are different PSU domains, the covariance is zero, and when they are the same PSU domain \eqn{\hat{P}^{(s,d_{1})}=\hat{P}^{(s,d_{2})}}.
#'   
#'   In the general case \eqn{\hat{D}^{(s,d)}} and \eqn{\hat{P}^{(s,d)}} are estimated by ratio estimators, and the error in these estimates are ignored. When \eqn{d} covers all of \eqn{s}, \eqn{\hat{D}^{(s,d)}=1} and \eqn{\hat{P}^{(s,d)}=1}is known. In this case both expressions can be shown to be unbiased. These quantities can only be calculated when \eqn{p_{i}} is provided, and will otherwise be NA.}
#'
#'   \item{Mean:}{
#'   The estimate of the mean value of some variable \eqn{v} in domain \eqn{d} and stratum \eqn{s}, when MeanOfMeans is false:
#'   \deqn{\hat{\mu}^{(s,d,v)} = \frac{\hat{t}^{(s,d,v)}}{\hat{N}^{(s,d)}}}
#'   }
#'   \item{Mean, Mean of Means:}{
#'   The estimate of the mean value of some variable in domain \eqn{d} and stratum \eqn{s}, when MeanOfMeans is true:
#'   \deqn{\hat{\mu}^{(s,d,v)}=\sum_{i=1}^{n}\frac{w_{i}}{\hat{d}^{(s,d)}}\hat{\mu}_{i}I^{(s,d,v)}_{i}H(\hat{N}^{(s,d)}_{i})}
#'   }
#'   \item{Mean, Covariance:}{
#'   In either case, the covariance of means are estimated as:
#'   \deqn{\widehat{CoVar}(\hat{\mu}^{(s,d_{1},v_{1})}, \hat{\mu}^{(s,d_{2},v_{2})}) = \frac{1}{(\hat{d}^{(s,d_{1} \cap d_{2})})^{2}}\frac{1}{n^{(s)}(n^{(s)}-1)} \sum_{i=1}^{n} 
#'   I^{(s,d_{1})}_{i} I^{(s,d_{2})}_{i} z_{i}^{(s,d_{1},v_{1})} z_{i}^{(s,d_{2},v_{2})}}
#'   \deqn{z_{i}^{(s,d,v)} = H(\hat{f}^{(s,d)}_{i}) (\hat{\mu}^{(s,d,v)}_{i} - \hat{\mu}^{(s,d,v)})}
#'   
#'   These are ratio estimates depending on the ratio ratio estimation of \eqn{\hat{d}^{(s,d)}}, \eqn{\hat{d}^{(s,d_{1} \cap d_{2})}} and \eqn{\hat{\mu}^{(s,d,v)}_{i}}, and the error in these estimates are ignored.
#'  }
#'  }
#'  Vocabulary for notation used above:
#'  \describe{
#'    \item{\eqn{H(x)}}{A step function which is 1 when \eqn{x>0}, otherwise it is zero.}
#'    \item{\eqn{I^{(s)}_{i}}}{The indicator function for stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s}, otherwise it is zero.}
#'    \item{\eqn{I^{(s,d)}_{i}}}{The indicator function for domain \eqn{d} and stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s} and domain \eqn{d}, otherwise it is zero.}
#'    \item{\eqn{J^{(s,d)}_{i}}}{The indicator function for domain \eqn{d} and stratum \eqn{s}. Is 1 when \eqn{i} is in stratum \eqn{s} and the PSU-domain of domain \eqn{d}, otherwise it is zero.}
#'    \item{\eqn{n}}{Sample size, the number of PSUs sampled.}
#'    \item{\eqn{n^{(s)}}}{Stratum sample size, the number of PSUs sampled in stratum \eqn{s}: \eqn{n_{s}=\sum_{i=1}^{n}I^{(s)}_{i}}.}
#'    \item{\eqn{n^{(s,d)}}}{Domain sample size, the number of PSUs sampled in domain{d} and stratum \eqn{s}: \eqn{n^{(s,d)}=\sum_{i=1}^{n}I^{(s,d)}_{i}}. 'Samples' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'    \item{\eqn{p_{i}}}{The selection probability of PSU \eqn{i}. 'SelectionProbability' in \code{\link[RstoxFDA]{PSUSamplingParametersData}}.}
#'    \item{\eqn{w_{i}}}{The normalized Hansen-Hurwitz sampling weight: \eqn{w_{i}=\frac{1}{p_{i}Q_{i}}}, \eqn{Q_{i}=\sum_{j=1}^{n}\frac{I^{(s(i))}_{j}}{p_{j}}}, where \eqn{s(i)} denote the strata of sample \eqn{i}. 'HHsamplingWeight' in \code{\link[RstoxFDA]{PSUSamplingParametersData}}.}
#'    \item{\eqn{\hat{D}^{(s,d)}}}{The estimated relative domain size (fraction of PSUs) of domain \eqn{d} in stratum \eqn{s}: \eqn{\hat{D}^{(s,d)}=\sum_{i=1}^{n}w_{i}I^{(s,d)}_{i}}.}
#'    \item{\eqn{\hat{P}^{(s,d)}}}{The estimated relative domain size (fraction of PSUs) of the PSU-domain of domain \eqn{d} in stratum \eqn{s}: \eqn{\hat{P}^{(s,d)}=\sum_{i=1}^{n}w_{i}J^{(s,d)}_{i}}.  'PSURelativeDomainSize' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'    \item{\eqn{\hat{M}^{(s,d)}}}{The estimated domain size (number of PSUs) of the PSU-domain of domain \eqn{d} in stratum \eqn{s}: \eqn{\hat{M}^{(s,d)}=\sum_{i=1}^{n}\frac{1}{p_{i}}J^{(s,d)}_{i}}.  'PSURelativeDomainSize' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'    \item{\eqn{\hat{d}^{(s,d)}}}{The estimated relative domain size (fraction of PSUs) that has observations (positive abundance or frequency) in domain \eqn{d} in stratum \eqn{s}: \eqn{\hat{d}^{(s,d)}=\sum_{i=1}^{n}w_{i}I^{(s,d)}_{i}H(\hat{f}^{(s,d)}_{i})}.}
#'    \item{\eqn{\hat{d}^{(s,d_{1} \cap d_{2})}}}{The estimated relative domain size (total number of PSUs) that has observations (positive abundance or frequency) in both domain \eqn{d_{1}} and \eqn{d_{2}} in stratum \eqn{s}: \deqn{\hat{d}^{(s,d)}=\sum_{i=1}^{n}w_{i}I^{(s,d_{1})}_{i}I^{(s,d_{2})}_{i}H(\hat{f}^{(s,d_{1})}_{i})H(\hat{f}^{(s,d_{2})}_{j})}.}
#'    \item{\eqn{\hat{N}^{(s)}}}{The estimated abundance in stratum \eqn{s}: \eqn{\hat{N}^{(s)}=\frac{1}{n_{s}}\sum_{i=1}^{n}\frac{\hat{N}_{i}}{p_{i}}I^{(s)}_{i}}.}
#'    \item{\eqn{\hat{N}^{(s)}_{i}}}{The estimated total abundance in stratum \eqn{s} at PSU \eqn{i}. 'Abundance' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'    \item{\eqn{\hat{N}^{(s,d)}_{i}}}{The estimated abundance in domain \eqn{d} and stratum \eqn{s} at PSU \eqn{i}. 'Abundance' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'    \item{\eqn{\hat{f}^{(s,d)}_{i}}}{The estimated frequency in domain \eqn{d} for stratum \eqn{s} at PSU \eqn{i}. 'Frequency' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'    \item{\eqn{\hat{t}^{(s,d,v)}_{i}}}{The estimated total of some variable \eqn{v} in domain \eqn{d} and stratum \eqn{s} at PSU \eqn{i}. 'Total' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'    \item{\eqn{\hat{\mu}^{(s,d,v)}_{i}}}{The estimated mean of some variable \eqn{v} in domain \eqn{d} and stratum \eqn{s} at PSU \eqn{i}. 'Mean' in \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}}.}
#'  }
#' @param PSUSamplingParametersData \code{\link[RstoxFDA]{PSUSamplingParametersData}} with sampling parameters for a sample of Primary Samplig Units.
#' @param AnalyticalPSUEstimateData \code{\link[RstoxFDA]{AnalyticalPSUEstimateData}} with estimates for each of the Primary Sampling Units in PSUSamplingParametersData
#' @param MeanOfMeans logical. Determines which estimators are used for frequencies and means. See details.
#' @return \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with estimated population parameters by stratum and domain.
#' @examples 
#'  PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
#'                                        RstoxFDA::CatchLotterySamplingExample, 
#'                                        RstoxFDA::CatchLotteryExample, 
#'                                        "serialnumber", "Haul", "MissingAtRandom")
#'  individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
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
#' @seealso \code{\link[RstoxFDA]{ReadPSUSamplingParameters}}, \code{\link[RstoxFDA]{ComputePSUSamplingParameters}}, \code{\link[RstoxFDA]{AddPsuStratificationVariables}}, \code{\link[RstoxFDA]{AssignPSUSamplingParameters}}, \code{\link[RstoxFDA]{AnalyticalRatioEstimate}}, \code{\link[RstoxFDA]{AggregateAnalyticalEstimate}}, \code{\link[RstoxFDA]{ExtendAnalyticalSamplingFrameCoverage}}, \code{\link[RstoxFDA]{InterpolateAnalyticalDomainEstimates}}
#' @export
#' @md
AnalyticalPopulationEstimate <- function(PSUSamplingParametersData, AnalyticalPSUEstimateData, MeanOfMeans=F){

  checkMandatory(PSUSamplingParametersData, "PSUSamplingParametersData")
  checkMandatory(AnalyticalPSUEstimateData, "AnalyticalPSUEstimateData")

  if (!is.AnalyticalPSUEstimateData(AnalyticalPSUEstimateData)){
    stop("Invalid 'AnalyticalPSUEstimateData'")
  }
  
  NestimatesByStrata <- AnalyticalPSUEstimateData$Abundance[,list(estimates=get(".N")),by="Stratum"]
  if (!length(unique(NestimatesByStrata$estimates))==1){
    stop("Cannot Estimate with heterogeneous lower level stratification. Consider the function CollapseStrata.")
  }
  
  LowerLevelStrata <- AnalyticalPSUEstimateData$StratificationVariables[!duplicated(get("Stratum")),.SD, .SDcol=names(AnalyticalPSUEstimateData$StratificationVariables)[names(AnalyticalPSUEstimateData$StratificationVariables)!="SampleId"]]
  names(LowerLevelStrata)[names(LowerLevelStrata)=="Stratum"] <- "LowerStratum"
  
  if (any(names(LowerLevelStrata) %in% names(PSUSamplingParametersData$StratificationVariables))){
    stop("Strata naming conflict. The same column names are used for PSU stratification and lower level stratification")
  }
  
  if (any(names(AnalyticalPSUEstimateData$DomainVariables) %in% names(PSUSamplingParametersData$StratificationVariables))){
    stop("Domain Variables in 'AnalyticalPSUEstimateData' occur as Stratification Variables in 'PSUSamplingParametersData'")
  }

  missingAbund <- AnalyticalPSUEstimateData$Abundance$SampleId[is.na(AnalyticalPSUEstimateData$Abundance$Abundance)]
  missingFreq <- AnalyticalPSUEstimateData$Abundance$SampleId[is.na(AnalyticalPSUEstimateData$Abundance$Frequency)]
  missingTotal <- AnalyticalPSUEstimateData$Variables$SampleId[is.na(AnalyticalPSUEstimateData$Variables$Total)]
  missingMean <- AnalyticalPSUEstimateData$Variables$SampleId[is.na(AnalyticalPSUEstimateData$Variables$Mean) & !is.nan(AnalyticalPSUEstimateData$Variables$Mean)]
  
  if (!MeanOfMeans & (length(missingAbund)>0 | length(missingTotal)>0)){
    msg <- "Cannot estimate. Abundance- or total-estimates are not provided for all samples in 'AnalyticalPSUEstimateData'."
    if (length(missingFreq)==0 | length(missingMean)==0){
      msg <- paste(msg, "Consider the option MeanOfMeans.")
    }

    missing <- unique(c(missingAbund, missingTotal))
    msg <- paste(msg, "Missing for SamplingUnitIds:", truncateStringVector(missing))
    stop(msg)
  }

  if (MeanOfMeans & (length(missingFreq)>0 | length(missingMean)>0)){
    msg <- "Cannot estimate. Frequency- or means-estimates are not provided for all samples in 'AnalyticalPSUEstimateData'."
    missing <- unique(c(missingFreq, missingMean))
    
    msg <- paste(msg, "Missing for SamplingUnitIds:", truncateStringVector(missing))
    stop(msg)
  }
  
  #
  # Combine lower level stratification with PSU stratification
  #
  CombinedStrata <- data.table::CJ(Stratum=PSUSamplingParametersData$StratificationVariables$Stratum, LowerStratum=LowerLevelStrata$LowerStratum, unique = T)
  CombinedStrata <- merge(CombinedStrata, PSUSamplingParametersData$StratificationVariables, by="Stratum")
  CombinedStrata <- merge(CombinedStrata, LowerLevelStrata, by="LowerStratum")
  CombinedStrata$Stratum <- paste("PSU-stratum:", CombinedStrata$Stratum, " Lower-stratum:", CombinedStrata$LowerStratum, sep="")

  #
  # Annotate PSU domains
  #
  PSUSamplingParametersData$SelectionTable <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$PSUDomainVariables, by.x="SamplingUnitId", by.y="SampleId")
  PSUDomainSize <- PSUSamplingParametersData$SelectionTable[,list(PSUDomainRelativeSize=sum(get("HHsamplingWeight"))), by=c("PSUDomain", "Stratum")]
  PSUDomainVariables <- AnalyticalPSUEstimateData$PSUDomainVariables
  PSUDomainVariables$SampleId <- NULL
  PSUDomainVariables <- PSUDomainVariables[!duplicated(get("PSUDomain")),]
  
  CombinedDomains <- data.table::CJ(PSUDomain=PSUDomainVariables$PSUDomain, LowerDomain=AnalyticalPSUEstimateData$DomainVariables$Domain, unique = T)
  CombinedDomains <- merge(CombinedDomains, PSUDomainVariables, by="PSUDomain")
  CombinedDomains <- merge(CombinedDomains, AnalyticalPSUEstimateData$DomainVariables, by.x="LowerDomain", by.y="Domain")
  CombinedDomains$Domain <- "All"
  for (domainCol in names(CombinedDomains)[!(names(CombinedDomains) %in% c("PSUDomain", "LowerDomain", "Domain"))]){
    CombinedDomains$Domain <- paste(CombinedDomains$Domain, paste(domainCol, CombinedDomains[[domainCol]], sep=":"), sep="/")
  }
  
  #
  # Tally samples
  #
  
  sampleCount <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$SampleCount, by.x="SamplingUnitId", by.y="SampleId", suffixes = c(".PSU", ".lower"))
  sampleCount$Stratum <- paste("PSU-stratum:", sampleCount$Stratum.PSU, " Lower-stratum:", sampleCount$Stratum.lower, sep="")
  sampleCount$Domain <- CombinedDomains$Domain[match(paste(sampleCount$PSUDomain, sampleCount$Domain), paste(CombinedDomains$PSUDomain, CombinedDomains$LowerDomain))]
  sampleCount <- sampleCount[,list(nPSU=length(unique(get("SamplingUnitId"))), nIndividuals=sum(get("nIndividuals"))), by=c("Stratum", "Domain")]
  
  #
  # Estimate abundance
  #
  selAbundance <- merge(PSUSamplingParametersData$SelectionTable, AnalyticalPSUEstimateData$Abundance, by.x="SamplingUnitId", by.y="SampleId", suffixes = c(".PSU", ".lower"))
  selAbundance$Stratum <- paste("PSU-stratum:", selAbundance$Stratum.PSU, " Lower-stratum:", selAbundance$Stratum.lower, sep="")
  selAbundance$Domain <- CombinedDomains$Domain[match(paste(selAbundance$PSUDomain, selAbundance$Domain), paste(CombinedDomains$PSUDomain, CombinedDomains$LowerDomain))]
  AbundanceTable <- selAbundance[,list(Abundance=mean(get("Abundance")/get("SelectionProbability"))*sum(get("HHsamplingWeight")), Frequency=sum(get("HHsamplingWeight")*get("Frequency"))/sum(get("HHsamplingWeight"))), by=c("Stratum", "Domain")]    
  
  if (!MeanOfMeans){
    StrataAbundance <- AbundanceTable[,list(StrataAbundance=sum(get("Abundance"))), by=c("Stratum")]
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
  selVariables$Domain <- CombinedDomains$Domain[match(paste(selVariables$PSUDomain, selVariables$Domain), paste(CombinedDomains$PSUDomain, CombinedDomains$LowerDomain))]
  VariablesTable <- selVariables[,list(Total=mean(get("Total")/get("SelectionProbability"))*sum(get("HHsamplingWeight")), Mean=sum(get("Mean")[!is.nan(get("Mean"))]*get("HHsamplingWeight")[!is.nan(get("Mean"))])/sum(get("HHsamplingWeight")[!is.nan(get("Mean"))]), NoMeans=all(is.nan(get("Mean")))), by=c("Stratum", "Domain", "Variable")]
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
  
  output$SampleCount <- sampleCount
  
  #
  # Fill missing strata
  #
  missingStrata <- output$StratificationVariables$Stratum[!(output$StratificationVariables$Stratum %in% AbundanceTable$Stratum)]
  if (length(missingStrata)>0){
    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain=output$DomainVariables$Domain, unique = T)
    output$Abundance <- rbind(output$Abundance, merge(output$Abundance, strataDomains, all.y=T))
    
    output$SampleCount <- rbind(output$SampleCount, merge(output$SampleCount, strataDomains, all.y=T))
    output$SampleCount$nPSU[is.na(output$SampleCount$nPSU)]<-0
    output$SampleCount$nIndividuals[is.na(output$SampleCount$nIndividuals)]<-0
    
    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain1=output$DomainVariables$Domain, Domain2=output$DomainVariables$Domain, unique = T)
    strataDomains <- strataDomains[get("Domain1")>=get("Domain2"),]
    output$AbundanceCovariance <- rbind(output$AbundanceCovariance, merge(output$AbundanceCovariance, strataDomains, all.y = T))
    
    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain=output$DomainVariables$Domain, Variable=output$Variables$Variable, unique = T)
    output$Variables <- rbind(output$Variables, merge(output$Variables, strataDomains, all.y=T))

    strataDomains <- data.table::CJ(Stratum=missingStrata, Domain1=output$DomainVariables$Domain, Domain2=output$DomainVariables$Domain, Variable1=output$Variables$Variable, Variable2=output$Variables$Variable, unique = T)
    strataDomains <- strataDomains[get("Domain1")>=get("Domain2") & get("Variable1")>=get("Variable2"),]
    output$VariablesCovariance <- rbind(output$VariablesCovariance, merge(output$VariablesCovariance, strataDomains, all.y = T))
    
  }
  
  #order tables
  output$Abundance <- output$Abundance[order(output$Abundance$Stratum, output$Abundance$Domain),]
  output$Variables <- output$Variables[order(output$Variables$Stratum, output$Variables$Domain, output$Variables$Variable),]
  output$AbundanceCovariance <- output$AbundanceCovariance[order(output$AbundanceCovariance$Stratum, output$AbundanceCovariance$Domain1, output$AbundanceCovariance$Domain2),]
  output$VariablesCovariance <- output$VariablesCovariance[order(output$VariablesCovariance$Stratum, output$VariablesCovariance$Domain1, output$VariablesCovariance$Domain2, output$VariablesCovariance$Variable1, output$VariablesCovariance$Variable2),]
  output$DomainVariables <- output$DomainVariables[order(output$DomainVariables$Domain),]
  output$StratificationVariables <- output$StratificationVariables[order(output$StratificationVariables$Stratum),]
  output$SampleCount <- output$SampleCount[order(output$SampleCount$Stratum, output$SampleCount$Domain),]
  
  return(output)
}

#' Ratio estimate to census landings
#' @description 
#'  Performs ratio estimate adjustments of estimates, based on frequency and mean weights of each domain.
#' @details
#'  Ratio estimates of abundance are obtained by relating abundance to weight and utilizing census data on landed weight to potentially improve estimates.
#'  Ratio estimation generally incurs some bias in estimation, and analytical expressions for variances are approximate.
#'  
#'  Landings are partitioned and assigned to domains in 'AnalyticalPopulationEstimateData' by matching column names.
#'  Column names in 'StoxLandingData' that are also either Stratification Columns or Domain Columns in 'AnalyticalPopulationEstimateData'
#'  can be used to construct the 'landing partitions' that provide total weigths for the ratio estimates.
#'  
#'  Ratio estimation of abundance may either improve an estimate of abundance obtained by other means, or provide an estimate of abundance
#'  when only proportions in domains are known. This requires that landing partitions are not covering more than one strata, although they may cover less.
#'  That is the Stratification columns in 'StratificationColumns' must identify strata in 'AnalyticalPopulationEstimateData'
#'  
#'  The function obtains a ratio estimate of total abundance in landings by relating proportion in domains, mean weight in domains, and total weight in landings to each other:
#'        
#'   'Abundance' will be estimated as \eqn{\widehat{qN}^{(s,d)}}, the variable 'Frequency' as \eqn{\widehat{qf}^{(s,d)}}, and the variable 'Total' as \eqn{\widehat{qt}^{(s,d,v)}}.
#'     These estimators and their corresponding variances ('AbundanceCovariance', 'FrequencyCovariance', and 'TotalCovariance') are given below. 
#'     Note that no revised estimate for means are provided with this method. 'Mean' and MeanCovariance' are unchanged.
#'     The estimates are based on estimated frequencies and ratio of of total landings in each landing partition to the estimated mean individual weight ('WeightVariable'):
#'     \deqn{\hat{Q}^{(s,d)}=\frac{W^{(L)}}{\sum_{(s',d') \in L}\hat{f}^{(s',d')}\hat{\mu}^{(s',d',\mathrm{w})}}}
#'     where \eqn{L=part(s,d)} is a partition of the landings containing the domain, specified by the arguments 'StratificationVariables' and 'DomainVariables'. 
#'     Since frequencies are normalized to strata, L cannot contain several strata.
#'     The abundance is estimated as:
#'     \deqn{\widehat{qN}^{(s,d)} = \hat{f}^{(s,d)}\hat{Q}^{(s,d)}}
#'     And covariances are estimated as:
#'     \deqn{\widehat{CoVar}(\widehat{qN}^{(s,d_{1})}, \widehat{qN}^{(s,d_{2})}) = \hat{Q}^{(s,d_{1})}\hat{Q}^{(s,d_{2})}\widehat{CoVar}(\hat{f}^{(s,d_{1})}, \hat{f}^{(s,d_{2})})}
#'     ignoring the error in \eqn{\hat{Q}^{(s,d)}}.
#'     The frequency is estimated as:
#'     \deqn{\widehat{qf}^{(s,d)} = \frac{\widehat{qN}^{(s,d)}}{\widehat{qN}^{(s)}}}
#'     And covariances are estimated as:
#'     \deqn{\widehat{CoVar}(\widehat{qf}^{(s,d_{1})}, \widehat{qf}^{(s,d_{2})}) = \frac{1}{\widehat{qN}^{(s)}}\widehat{CoVar}(\widehat{qN}^{(s,d_{1})}, \widehat{qN}^{(s,d_{2})})}
#'     ignoring the error in \eqn{\widehat{qN}^{(s)}}.
#'     Totals are estimated as:
#'     \deqn{\widehat{qt}^{(s,d,v)} = \widehat{qN}^{(s,d)}\hat{\mu}^{(s,d,v)}}
#'     And covariances are estimated as:
#'     \deqn{\widehat{CoVar}(\widehat{qt}^{(s,d_{1},v_{1})}, \widehat{qt}^{(s,d_{2},v_{2})}) = \widehat{qN}^{(s,d_{1})}\widehat{qN}^{(s,d_{2})}\widehat{CoVar}(\hat{\mu}^{(s,d_{1},v_{1})}, \hat{\mu}^{(s,d_{2},v_{2})})}
#'     ignoring the error in \eqn{\widehat{qN}^{(s,d)}}.
#'     Note that no revised estimate for means are provided with this method. 'Mean' and MeanCovariance' is unchanged.
#'
#'  Vocabulary for equations given above:
#'  \describe{
#'   \item{\eqn{part(s,d)}}{The partition of the landings containing the domain \eqn{d} in stratum \eqn{s}.}
#'   \item{\eqn{\hat{N}^{(s,d)}}}{The estimated abundance in the domain \eqn{d} in stratum \eqn{s}. 'Abundance' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{t}^{(s,d,v)}}}{The estimated total of variable \eqn{v} in domain \eqn{d} in stratum \eqn{s}. The 'Total' for the 'Variable' \eqn{v} in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{t}^{(s,d,\mathrm{w})}}}{The estimated total weight in domain \eqn{d} in stratum \eqn{s}. The 'Total' for the 'Variable' identified by the argument 'WeightVariable' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{\mu}^{(s,d,v)}}}{The estimated mean of the variable \eqn{v} in domain \eqn{d} in stratum \eqn{s}. The 'Mean' for the 'Variable' \eqn{v} in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{\mu}^{(s,d,\mathrm{w})}}}{The estimated mean weight in domain \eqn{d} in stratum \eqn{s}. The 'Mean' for the 'Variable' identified by the argument 'WeightVariable' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{t}^{(L,\mathrm{w})}}}{The estimated total weight in the landing partition: \eqn{\hat{t}^{(L)}=\sum_{(s,d) \in L}\hat{t}^{(s,d,\mathrm{w})}}}
#'   \item{\eqn{\widehat{rN}^{(s)}}}{The estimated total abundance in stratum \eqn{s}, based on total domain weight estimates: \eqn{\widehat{rN}^{(s)}=\sum_{d}\widehat{rN}^{(s,d)}}, where the sum runs over all domains in stratum \eqn{s}.}
#'   \item{\eqn{\widehat{qN}^{(s)}}}{The estimated total abundance in stratum \eqn{s}, based on mean domain weight estimates: \eqn{\widehat{qN}^{(s)}=\sum_{d}\widehat{qN}^{(s,d)}}, where the sum runs over all domains in stratum \eqn{s}.}
#'   \item{\eqn{\widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})})}}{The estimated covariance of abundance between the domains \eqn{d_{1}} and \eqn{d_{2}} in stratum \eqn{s}. 'AbundanceCovariance' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\hat{f}^{(s,d)}}}{The estimated frequency in domain \eqn{d} in stratum \eqn{s}. 'Frequency' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\widehat{CoVar}(\hat{f}^{(s,d_{1})}, \hat{f}^{(s,d_{2})})}}{The estimated covariance of frequencies between the domains \eqn{d_{1}} and \eqn{d_{2}} in stratum \eqn{s}. 'FrequencyCovariance' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\widehat{CoVar}(\hat{t}^{(s,d_{1},v_{1})}, \hat{f}^{(s,d_{2},v_{2})})}}{The estimated covariance of totals between the variable \eqn{v_{1}} in domain \eqn{d_{1}} and the variable \eqn{v_{2}} in domain \eqn{d_{2}} in stratum \eqn{s}. 'TotalCovariance' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'   \item{\eqn{\widehat{CoVar}(\hat{\mu}^{(s,d_{1},v_{1})}, \hat{f}^{(s,d_{2},v_{2})})}}{The estimated covariance of means between the variable \eqn{v_{1}} in domain \eqn{d_{1}} and the variable \eqn{v_{2}} in domain \eqn{d_{2}} in stratum \eqn{s}. 'MeanCovariance' in \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}.}
#'  }
#'  
#' @param AnalyticalPopulationEstimateData \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with estimates of mean or total weights and frequencies or abundance in domains
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} with census data on total weight in each stratum
#' @param WeightVariable character() name of variable in 'AnalyticalPopulationEstimateData' that represent weight of individuals in grams.
#' @param StratificationVariables vector of stratification columns to include when matching estimates to landings. Variable must exist as Stratification Variable in 'AnalyticalPopulationEstimateData' and in 'StoxLandingData'
#' @param DomainVariables vector of domain columns to include when matching estimates to landings. Variable must exist as Domain Variable in 'AnalyticalPopulationEstimateData' and in 'StoxLandingData'
#' @return \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with ratio estimates.
#' @examples 
#' 
#'  PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
#'                                        RstoxFDA::CatchLotterySamplingExample, 
#'                                        RstoxFDA::CatchLotteryExample, 
#'                                        "serialnumber", "Haul", "MissingAtRandom")
#'  individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
#'                                        RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))
#'  
#'  # Estimate for the domain 'CountryVessel'                                      
#'  psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
#'                                        individualSamplingParameters, 
#'                                        c("IndividualRoundWeight"), 
#'                                        c("IndividualAge"))
#'  popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
#'  
#'  # perform ration estimate, assigning total weights by 'CountryVessel'
#'  ratioEst <- RstoxFDA::AnalyticalRatioEstimate(popEst, 
#'                                         RstoxFDA::CatchLotteryLandingExample, 
#'                                         "IndividualRoundWeight", 
#'                                         "CountryVessel")
#'  
#' @concept Analytical estimation
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}, \code{\link[RstoxFDA]{AggregateAnalyticalEstimate}}, \code{\link[RstoxFDA]{ExtendAnalyticalSamplingFrameCoverage}}, \code{\link[RstoxFDA]{InterpolateAnalyticalDomainEstimates}}
#' @export
#' @md
AnalyticalRatioEstimate <- function(AnalyticalPopulationEstimateData, StoxLandingData, WeightVariable=character(), StratificationVariables=character(), DomainVariables=character()){
  
  Method <- "MeanDomainWeight"
  
  if (!(WeightVariable %in% AnalyticalPopulationEstimateData$Variables$Variable)){
    stop(paste("'WeightVariable'", WeightVariable, "is not estimated in 'AnalyticalPopulationEstimateData'"))
  }

  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  checkMandatory(StoxLandingData, "StoxLandingData")
  checkLandingsNotEmpty(StoxLandingData)
  checkMandatory(WeightVariable, "WeightVariable")
  checkOptions(Method, "Method", c("TotalDomainWeight", "MeanDomainWeight"))
  checkMandatory(StratificationVariables, "StratificationVariables")

  if (!is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData)){
    stop("Invalid 'AnalyticalPopulationEstimateData'")
  }
  
  potentialNames <- c(StratificationVariables, DomainVariables)
  missing <- potentialNames[!(potentialNames %in% names(StoxLandingData$Landing))]
  if (length(missing)>0){
    stop(paste("Some Stratification Variables or Domain Variables could not be matched with landings:", paste(missing, collapse = ",")))
  }
  landingsPartition <- potentialNames
  
  if (length(DomainVariables)==0){
    domainVarsInEst <- names(AnalyticalPopulationEstimateData$DomainVariables)
    domainVarsInEst <- domainVarsInEst[domainVarsInEst != "Domain"]
    domainVarsInLandings <- names(StoxLandingData$Landing)
    domainVarsInLandings <- domainVarsInLandings[domainVarsInLandings %in% domainVarsInEst]
    if (length(domainVarsInLandings)>0){
      stoxWarning(paste("No Domain Variables configured, although appropriately named domains are available in Landings", truncateStringVector(domainVarsInLandings)))
    }
  }
  
  #
  # coerce character for variables in landings-partition, as all stratification variables and domain variables in estimates ar char
  #
  for (var in landingsPartition){
    StoxLandingData$Landing[[var]] <- as.character(StoxLandingData$Landing[[var]])
  }
  
  
  #check that strata are OK for application of frequencies
  strataCheck <- AnalyticalPopulationEstimateData$StratificationVariables
  landingsPart <- apply(strataCheck[,.SD,.SDcols = StratificationVariables], 1, FUN=paste, collapse="/")
  strataCount <- strataCheck[,list(nStrata=length(unique(get("Stratum")))), by=list(landingsPart=landingsPart)]
  strataCount <- strataCount[get("nStrata")>1,]
  if (nrow(strataCount)>0){
    stop("The specified Stratification Variables does not identify strata. More than one strata found for landings partitions: ", paste(strataCount$landingsPart, collapse=","))
  }
  
  # Check matches
  landDomains <- StoxLandingData$Landing[,.SD,.SDcol=landingsPartition]
  landDomains <- merge(landDomains, AnalyticalPopulationEstimateData$StratificationVariables, by=StratificationVariables)
  landDomainsString <- unique(apply(landDomains[,.SD,.SDcol=c("Stratum", DomainVariables)], FUN=paste, 1, collapse="/"))
  sampleDomains <- merge(AnalyticalPopulationEstimateData$Abundance, AnalyticalPopulationEstimateData$DomainVariables, by="Domain")
  sampleDomainString <- unique(apply(sampleDomains[,.SD,.SDcol=c("Stratum", DomainVariables)], FUN=paste, 1, collapse="/"))
  
  if (!any(sampleDomainString %in% landDomainsString)){
    stop(paste("None of the landing partitions (", paste(landingsPartition, collapse = ","), ") in 'StoxLandingData' have corresponding domains in 'AnalyticalPopulationEstimateData'", sep=""))
  }
  
  missingDomains <- landDomainsString[!(landDomainsString %in% sampleDomainString)]
  if (length(missingDomains)>0){
    stop(paste("Estimates missing for some domains in landings. Consider filtering data with 'FilterStoxBiotic' or interpolating with 'InterpolateAnalyticalDomainEstimates'. Missing domains:", truncateStringVector(missingDomains)))
  }
  
  if (Method == "MeanDomainWeight"){

    frequencies <- AnalyticalPopulationEstimateData$Abundance[,.SD,.SDcol=c("Stratum", "Domain", "Frequency")]
    frequencies <- merge(frequencies, AnalyticalPopulationEstimateData$DomainVariables, by="Domain")

    #add mean weights
    meanW <- AnalyticalPopulationEstimateData$Variables[AnalyticalPopulationEstimateData$Variables$Variable==WeightVariable,.SD,.SDcol=c("Stratum", "Domain", "Mean")]
    frequencies <- merge(frequencies, meanW, by=c("Stratum", "Domain"))
    
    #
    # normalize frequencies to landingsPartition within samplingstrata
    #
    
    # NaN means correspond to frequency of zero
    totalFrequencies <- frequencies[,list(totalFreq=sum(get("Frequency")[!is.nan(get("Mean"))]*get("Mean")[!is.nan(get("Mean"))])), by=c("Stratum", DomainVariables)]

    #
    # calculate total frequencies
    #
    frequencies <- merge(frequencies, totalFrequencies, by=c("Stratum", DomainVariables), all.x = T)
    
    # estimate total landings in domain
    domainStrata <- AnalyticalPopulationEstimateData$StratificationVariables$Stratum[
      match(
        apply(
          StoxLandingData$Landing[,.SD,.SDcol=StratificationVariables], 1, paste, collapse="/"
        ),
        apply(
          AnalyticalPopulationEstimateData$StratificationVariables[,.SD,.SDcol=StratificationVariables], 1, paste, collapse="/"
        )
            )
    ]
    
    byList <- list(Stratum=domainStrata)
    for (var in DomainVariables){
      byList[[var]] <- StoxLandingData$Landing[[var]]
    }
    totalLandings <- StoxLandingData$Landing[,list(totalLanding=sum(get("RoundWeight"))*1000), by=byList] #WeightVariable is in grams.
    frequencies <- merge(frequencies, totalLandings, by=c("Stratum", DomainVariables), all.x=T)
    frequencies$domainLanding <- (frequencies$Frequency / frequencies$totalFreq) * frequencies$totalLanding
    frequencies$domainLanding[is.na(frequencies$totalLanding)] <- 0 #domains not reported in landings are zero
    frequencies$totalLanding[is.na(frequencies$totalLanding)] <- 0 #domains not reported in landings are zero
    
    
    means <- AnalyticalPopulationEstimateData$Variables[AnalyticalPopulationEstimateData$Variables$Variable == WeightVariable,]
    
    #
    # Ratio-estimate abundance from estimated domain landings and domain means
    #
    freqMatch <- match(paste(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain), paste(frequencies$Stratum, frequencies$Domain))
    AnalyticalPopulationEstimateData$Abundance$Abundance <- frequencies$domainLanding[freqMatch]
    m1 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1), paste(frequencies$Stratum, frequencies$Domain))
    m2 <- match(paste(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2), paste(frequencies$Stratum, frequencies$Domain))
    AnalyticalPopulationEstimateData$AbundanceCovariance$AbundanceCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance$FrequencyCovariance * (frequencies$totalLanding[m1] / (frequencies$totalFreq[m1])) * (frequencies$totalLanding[m2] / (frequencies$totalFreq[m2]))
    
    #
    # Ratio-estimate frequencies
    #
    abundanceByStratum <- AnalyticalPopulationEstimateData$Abundance[,list(totalAbundance=sum(get("Abundance"))), by="Stratum"]
    stopifnot(all(!is.na(abundanceByStratum$totalAbundance)))
    stopifnot(all(!is.na(AnalyticalPopulationEstimateData$Abundance$Abundance)))
    m <- match(AnalyticalPopulationEstimateData$Abundance$Stratum, abundanceByStratum$Stratum)
    AnalyticalPopulationEstimateData$Abundance$Frequency <- AnalyticalPopulationEstimateData$Abundance$Abundance / abundanceByStratum$totalAbundance[m]
    #since totalAbundance and Abundance is asserted to not be NaN, NaN frequencies correspond to zero abundance and hence zero frequenct
    AnalyticalPopulationEstimateData$Abundance$Frequency[is.nan(AnalyticalPopulationEstimateData$Abundance$Frequency)] <- 0
    m <- match(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, abundanceByStratum$Stratum)
    AnalyticalPopulationEstimateData$AbundanceCovariance$FrequencyCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance$AbundanceCovariance * (1/abundanceByStratum$totalAbundance[m])**2
    
    #
    # Ratio estimate totals 
    #
    m <- match(paste(AnalyticalPopulationEstimateData$Variables$Stratum, AnalyticalPopulationEstimateData$Variables$Domain), paste(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain))
    AnalyticalPopulationEstimateData$Variables$Total <- AnalyticalPopulationEstimateData$Variables$Mean * AnalyticalPopulationEstimateData$Abundance$Abundance[m]
    m1 <- match(paste(AnalyticalPopulationEstimateData$VariablesCovariance$Stratum, AnalyticalPopulationEstimateData$VariablesCovariance$Domain1), paste(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain))
    m2 <- match(paste(AnalyticalPopulationEstimateData$VariablesCovariance$Stratum, AnalyticalPopulationEstimateData$VariablesCovariance$Domain2), paste(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain))
    AnalyticalPopulationEstimateData$VariablesCovariance$TotalCovariance <- AnalyticalPopulationEstimateData$VariablesCovariance$MeanCovariance * AnalyticalPopulationEstimateData$Abundance$Abundance[m1] * AnalyticalPopulationEstimateData$Abundance$Abundance[m2]
    
    #
    # Do nothing with means.
    #
    
    #order tables
    AnalyticalPopulationEstimateData$Abundance <- AnalyticalPopulationEstimateData$Abundance[order(AnalyticalPopulationEstimateData$Abundance$Stratum, AnalyticalPopulationEstimateData$Abundance$Domain),]
    AnalyticalPopulationEstimateData$Variables <- AnalyticalPopulationEstimateData$Variables[order(AnalyticalPopulationEstimateData$Variables$Stratum, AnalyticalPopulationEstimateData$Variables$Domain, AnalyticalPopulationEstimateData$Variables$Variable),]
    AnalyticalPopulationEstimateData$AbundanceCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance[order(AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1, AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2),]
    AnalyticalPopulationEstimateData$VariablesCovariance <- AnalyticalPopulationEstimateData$VariablesCovariance[order(AnalyticalPopulationEstimateData$VariablesCovariance$Stratum, AnalyticalPopulationEstimateData$VariablesCovariance$Domain1, AnalyticalPopulationEstimateData$VariablesCovariance$Domain2, AnalyticalPopulationEstimateData$VariablesCovariance$Variable1, AnalyticalPopulationEstimateData$VariablesCovariance$Variable2),]
    AnalyticalPopulationEstimateData$DomainVariables <- AnalyticalPopulationEstimateData$DomainVariables[order(AnalyticalPopulationEstimateData$DomainVariables$Domain),]
    AnalyticalPopulationEstimateData$StratificationVariables <- AnalyticalPopulationEstimateData$StratificationVariables[order(AnalyticalPopulationEstimateData$StratificationVariables$Stratum),]
    
    return(AnalyticalPopulationEstimateData)
  }

}

#' Aggregate Analytical Estimate across strata
#' @description
#'  Aggregates estimate across strata. One can optinally exclude some strata from being aggregated (using the argument 'RetainStrata').
#'  Retained strata are returned unchanged.
#'  
#'  Aggregation of means and frequencies across strata, requires information about strata size (abundance in strata).
#'  For sampling programs where these cannot be directly estimated, they must be inferred before application of this function.
#'  Otherwise means and frequencies will be NA. Consider for example ratio-estimation by strata (\code{\link[RstoxFDA]{AnalyticalRatioEstimate}})
#'  
#'  Specifically, a new stratum \eqn{s'} (named according to 'AggregateStratumName') is defined based on the strata in \eqn{S}, where \eqn{S} contains all strata in 'AnalyticalPopulationEstimateData',
#'  except those explicitly excluded by the argument 'RetainStrata'. The estimated parameters are obtained as follows:
#'  \describe{
#'   \item{Abundance:}{
#'   The estimate of the total number of individuals in domain \eqn{d} and stratum \eqn{s'}:
#'   \deqn{\hat{N}^{(s',d)} = \sum_{s \in S} \hat{N}^{(s,d)}}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{N}^{(s',d_{1})}, \hat{N}^{(s',d_{2})}) = \sum_{s \in S} \widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})})}
#'   
#'   where \eqn{\hat{N}^{(s,d)}} and \eqn{\widehat{CoVar}(\hat{N}^{(s,d_{1})}, \hat{N}^{(s,d_{2})})} are provided by the argument 'AnalyticalPopulationEstimateData'.
#'   }
#'   \item{Frequency:}{
#'   The estimate of the fraction of individuals in stratum \eqn{s'} that are in domain \eqn{d}:
#'   \deqn{ \hat{f}^{(s',d)} = \frac{\hat{N}^{(s',d)}}{\hat{N}^{(s')}} }
#'   with co-variance:
#'   \deqn{ \widehat{CoVar}(\hat{f}^{(s',d_{1})}, \hat{f}^{(s',d_{2})}) = \frac{1}{(\hat{N}^{(s')})^{2}}\widehat{CoVar}(\hat{N}^{(s',d_{1})}, \hat{N}^{(s',d_{2})})}
#'   
#'   where \eqn{\hat{N}^{(s')}} is the total estimated abundance in stratum, across all domains.
#'   }
#'   \item{Total:}{
#'   The estimate of the total value of some variable \eqn{v} in domain \eqn{d} and stratum \eqn{s'}.
#'   \deqn{\hat{t}^{(s',d,v)}=\sum_{s \in S}\hat{t}^{(s,d,v)}}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{t}^{(s',d_{1},v_{1})}, \hat{t}^{(s',d_{2},v_{2})}) = \sum_{s \in S} \widehat{CoVar}(\hat{t}^{(s,d_{1},v_{1})}, \hat{t}^{(s,d_{2},v_{2})})}
#'   
#'   where \eqn{\hat{t}^{(s,d,v)}} and \eqn{\widehat{CoVar}(\hat{t}^{(s,d_{1},v_{1})}, \hat{t}^{(s,d_{2},v_{2})})} are provided by the argument 'AnalyticalPopulationEstimateData'.
#'   }
#'
#'   \item{Mean:}{
#'   The estimate of the mean value of some variable \eqn{v} in domain \eqn{d} and stratum \eqn{s'}:
#'   \deqn{\hat{\mu}^{(s',d,v)} = \frac{1}{\hat{N}^{(s',d)}}\sum_{s \in S} \hat{\mu}^{(s,d,v)}\hat{N}^{(s,d)}}
#'   with co-variance:
#'   \deqn{\widehat{CoVar}(\hat{\mu}^{(s',d_{1},v_{1})}, \hat{\mu}^{(s',d_{2},v_{2})}) = \sum_{s \in S} \widehat{CoVar}(\hat{\mu}^{(s,d_{1},v_{1})}, \hat{\mu}^{(s,d_{2},v_{2})}) z^{(s, d_{1}, d_{2})}}
#'   where
#'   \deqn{z^{(s, d_{1}, d_{2})}=\frac{\hat{N}^{(s,d_{1})}\hat{N}^{(s,d_{2})}}{\hat{N}^{(s',d_{1})}\hat{N}^{(s',d_{2})}}}
#'   }
#'  }
#' @param AnalyticalPopulationEstimateData \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with estimates to aggregate.
#' @param RetainStrata strata that should not be included in the aggregation. Must correspond to 'Stratum' in 'AnalyticalPopulationEstimateData'
#' @param AggregateStratumName name to use for the aggregated stratum
#' @return \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with aggregated estimates.
#' @concept Analytical estimation
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}, \code{\link[RstoxFDA]{AnalyticalRatioEstimate}}, \code{\link[RstoxFDA]{ExtendAnalyticalSamplingFrameCoverage}}, \code{\link[RstoxFDA]{InterpolateAnalyticalDomainEstimates}}
#' @export
#' @md
AggregateAnalyticalEstimate <- function(AnalyticalPopulationEstimateData, RetainStrata=character(), AggregateStratumName=character()){
  
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  checkMandatory(AggregateStratumName, "AggregateStratumName")
  if (isGiven(RetainStrata)){
    if (!all(RetainStrata %in% AnalyticalPopulationEstimateData$StratificationVariables$Stratum)){
      missing <- RetainStrata[!(RetainStrata %in% AnalyticalPopulationEstimateData$StratificationVariables$Stratum)]
      stop(paste("Not all values in 'RetainStrata' are strata in 'AnalyticalPopulationEstimateData'. Missing:", truncateStringVector(missing)))
    }
  }
  else{
    RetainStrata <- c()
  }
  
  if (!is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData)){
    stop("Invalid 'AnalyticalPopulationEstimateData'")
  }
  
  collapsedStrata <- unique(AnalyticalPopulationEstimateData$StratificationVariables$Stratum[
    !(AnalyticalPopulationEstimateData$StratificationVariables$Stratum %in% RetainStrata)])

  newSampleCountTable <- AnalyticalPopulationEstimateData$SampleCount
  newSampleCountTable$Stratum[newSampleCountTable$Stratum %in% collapsedStrata] <- AggregateStratumName
  newSampleCountTable <- newSampleCountTable[,list(nPSU=sum(get("nPSU")), nIndividuals=sum(get("nIndividuals"))), by=c("Stratum", "Domain")]
  
  newAbundanceTableOldStratum <- AnalyticalPopulationEstimateData$Abundance[!(get("Stratum") %in% RetainStrata),]
  
  totalAbundance <- sum(newAbundanceTableOldStratum$Abundance)
  newAbundanceTable <- newAbundanceTableOldStratum
  newAbundanceTable$Stratum[newAbundanceTable$Stratum %in% collapsedStrata] <- AggregateStratumName
  newAbundanceTable <- newAbundanceTable[,list(Abundance=sum(get("Abundance")), 
                                               Frequency=sum(get("Abundance"))/totalAbundance
                                               ), 
                                         by=c("Stratum", "Domain")]
  
  newAbundanceVarianceTable <- AnalyticalPopulationEstimateData$AbundanceCovariance[!(get("Stratum") %in% RetainStrata),]
  newAbundanceVarianceTable$Stratum[newAbundanceVarianceTable$Stratum %in% collapsedStrata] <- AggregateStratumName
  newAbundanceVarianceTable <- newAbundanceVarianceTable[,list(AbundanceCovariance=sum(get("AbundanceCovariance")),
                                                               FrequencyCovariance=sum(get("AbundanceCovariance"))/(totalAbundance**2)),
                                                         by=c("Stratum", "Domain1", "Domain2")]
  
  newVariablesTable <- AnalyticalPopulationEstimateData$Variables[!(get("Stratum") %in% RetainStrata),]
  newVariablesTable <- merge(newVariablesTable, newAbundanceTableOldStratum, by=c("Stratum", "Domain"))
  newVariablesTable$Stratum[newVariablesTable$Stratum %in% collapsedStrata] <- AggregateStratumName
  newVariablesTable <- newVariablesTable[,list(Total=sum(get("Total")), 
                                               #NaN means correspond to zero abundance
                                               Mean=sum(get("Mean")[!is.na(get("Mean"))]*get("Abundance")[!is.nan(get("Mean"))])/sum(get("Abundance")[!is.nan(get("Mean"))])
                                               ), 
                                         by=c("Stratum", "Domain", "Variable")]
  
  newVariablesVariancetable <- AnalyticalPopulationEstimateData$VariablesCovariance[!(get("Stratum") %in% RetainStrata),]
  newVariablesVariancetable <- merge(newVariablesVariancetable, newAbundanceTableOldStratum, by.x=c("Stratum", "Domain1"), by.y=c("Stratum", "Domain"))
  newVariablesVariancetable <- merge(newVariablesVariancetable, newAbundanceTableOldStratum, by.x=c("Stratum", "Domain2"), by.y=c("Stratum", "Domain"), suffixes=c("1","2"))
  newVariablesVariancetable$Stratum[newVariablesVariancetable$Stratum %in% collapsedStrata] <- AggregateStratumName
  newVariablesVariancetable <- newVariablesVariancetable[,list(TotalCovariance=sum(get("TotalCovariance")),
                                                               MeanCovariance=sum(get("MeanCovariance")[!is.nan(get("MeanCovariance"))] * 
                                                                                    get("Abundance1")[!is.nan(get("MeanCovariance"))] *
                                                                                    get("Abundance2")[!is.nan(get("MeanCovariance"))]) /
                                                                 (sum(get("Abundance1")[!is.nan(get("MeanCovariance"))]) *
                                                                       sum(get("Abundance2")[!is.nan(get("MeanCovariance"))])
                                                                  )
                                                               ),
                                                               by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]
  
  AnalyticalPopulationEstimateData$StratificationVariables$Stratum[!(AnalyticalPopulationEstimateData$StratificationVariables$Stratum %in% RetainStrata)] <- AggregateStratumName
  AnalyticalPopulationEstimateData$Abundance <- rbind(AnalyticalPopulationEstimateData$Abundance[AnalyticalPopulationEstimateData$Abundance$Stratum %in% RetainStrata,],
                                                                                                 newAbundanceTable)
  AnalyticalPopulationEstimateData$AbundanceCovariance <- rbind(AnalyticalPopulationEstimateData$AbundanceCovariance[AnalyticalPopulationEstimateData$AbundanceCovariance$Stratum %in% RetainStrata,],
                                                                                                 newAbundanceVarianceTable)
  AnalyticalPopulationEstimateData$Variables <- rbind(AnalyticalPopulationEstimateData$Variables[AnalyticalPopulationEstimateData$Variables$Stratum %in% RetainStrata,],
                                                      newVariablesTable)
  AnalyticalPopulationEstimateData$VariablesCovariance <- rbind(AnalyticalPopulationEstimateData$VariablesCovariance[AnalyticalPopulationEstimateData$VariablesCovariance$Stratum %in% RetainStrata,],
                                                                newVariablesVariancetable)
  
  AnalyticalPopulationEstimateData$SampleCount <- newSampleCountTable
  
  return(AnalyticalPopulationEstimateData)
  
}

#' Fills in unsampled strata according to 'strict' after new domains and new strata has been inferred and added to the estimation object
#' @noRd
fillStrict <- function(extendedAnalyticalPopulationEstimateData){
  newAbundance <- data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                 Domain=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain))
  newAbundance <- merge(extendedAnalyticalPopulationEstimateData$Abundance, 
                        newAbundance, all.y=T, by=c("Stratum", "Domain"))
  
  newVariables <- data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                 Domain=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain), 
                                 Variable=unique(extendedAnalyticalPopulationEstimateData$Variables$Variable))
  newVariables <- merge(extendedAnalyticalPopulationEstimateData$Variables, newVariables, all.y=T, by=c("Stratum", "Domain", "Variable"))
  
  cross <- data.table::CJ(Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain), 
                          Domain2=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain))
  cross <- cross[cross$Domain1>=cross$Domain2,]
  cross <- merge(data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain)),
                 cross, by=c("Domain1"), allow.cartesian=T)
  newAbundanceCovariance <- merge(extendedAnalyticalPopulationEstimateData$AbundanceCovariance, 
                                  cross, by=c("Stratum", "Domain1", "Domain2"), all.y=T)
  
  cross <- data.table::CJ(Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain), 
                          Variable1=unique(extendedAnalyticalPopulationEstimateData$Variables$Variable), 
                          Domain2=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain), 
                          Variable2=unique(extendedAnalyticalPopulationEstimateData$Variables$Variable))
  cross <- cross[cross$Domain1>=cross$Domain2 & cross$Variable1 >= cross$Variable2]
  cross <- merge(data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain)),
                 cross, by=c("Domain1"), allow.cartesian=T)
  newVariableCovariance <- merge(extendedAnalyticalPopulationEstimateData$VariablesCovariance, cross, 
                                 by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2"), all.y=T)
  
  extendedAnalyticalPopulationEstimateData$Abundance <- newAbundance
  extendedAnalyticalPopulationEstimateData$Variables <- newVariables
  extendedAnalyticalPopulationEstimateData$AbundanceCovariance <- newAbundanceCovariance
  extendedAnalyticalPopulationEstimateData$VariablesCovariance <- newVariableCovariance
  
  return(extendedAnalyticalPopulationEstimateData)
}


#' Fills in unsampled strata, according to 'SetToStratum' after new domains and new strata has been inferred and added to the estimation object
#' @noRd
fillSetToStratum <- function(extendedAnalyticalPopulationEstimateData, SourceStratum, UnsampledStratum){
  
  abundance <- extendedAnalyticalPopulationEstimateData$Abundance[
    extendedAnalyticalPopulationEstimateData$Abundance$Stratum == SourceStratum,]
  abundance$Stratum <- UnsampledStratum
  abundance$Abundance <- as.numeric(NA)
  
  variables <- extendedAnalyticalPopulationEstimateData$Variables[
    extendedAnalyticalPopulationEstimateData$Variables$Stratum == SourceStratum,]
  variables$Total <- as.numeric(NA)
  variables$Stratum <- UnsampledStratum
  
  abundCovar <- extendedAnalyticalPopulationEstimateData$AbundanceCovariance[
    extendedAnalyticalPopulationEstimateData$AbundanceCovariance$Stratum == SourceStratum,]
  abundCovar$AbundanceCovariance <- as.numeric(NA)
  abundCovar$Stratum <- UnsampledStratum
  
  variableCovar <- extendedAnalyticalPopulationEstimateData$VariablesCovariance[
    extendedAnalyticalPopulationEstimateData$VariablesCovariance$Stratum == SourceStratum,]
  variableCovar$TotalCovariance <- as.numeric(NA)
  variableCovar$Stratum <- UnsampledStratum
  
  extendedAnalyticalPopulationEstimateData$Abundance <- rbind(extendedAnalyticalPopulationEstimateData$Abundance, abundance)
  extendedAnalyticalPopulationEstimateData$Variables <- rbind(variables, extendedAnalyticalPopulationEstimateData$Variables)
  extendedAnalyticalPopulationEstimateData$AbundanceCovariance <- rbind(extendedAnalyticalPopulationEstimateData$AbundanceCovariance, abundCovar)
  extendedAnalyticalPopulationEstimateData$VariablesCovariance <- rbind(variableCovar, extendedAnalyticalPopulationEstimateData$VariablesCovariance)
  
  return(extendedAnalyticalPopulationEstimateData)
}

#' Extends estimate beyond sampling frame
#' @description
#'  Infer estimates to parts of the fishery / target population that was not covered by sampling programs.
#'  That is strata not covered by the sampling frame, but that are known to be populated in census data (landing data).
#'  These are pragmatic approximations, without statistical justification.
#' @details
#'  This function only infers precence of unsampled strata from census-data, and provide options for some pragmatic approximations to substitute for estimates for these strata.
#'  Any estimates already provided is not changed. The function does not introduce landed weights, or other knowledge from landings, 
#'  except for the fact that strata are present in the landings data. All inference about unkown values are taken from the provided estimates ('AnalyticalPopulationEstimateData').
#'  Subsequent ratio-estimation may make use of this information to also make use of total-weight information from landings (see \code{link[RstoxFDA]{AnalyticalRatioEstimate}}).
#'  
#'  Inference about unkown values can be done in several ways, controled by the argument 'Method'
#'  \describe{
#'  \item{Strict}{
#'   Provide NA-values for all parameters of all domains that is not in the sampling frame.}
#'  \item{SetToStratum}{
#'   Provide NA-values for abundance and total of all domains that is not in the sampling frame.
#'   Set means, frequencies, and corresponding variance to the same values as in a sampled strata.}
#'  }
#'  
#'  For variables in 'StratificationVariables', any non-sampled landing partition
#'  will be added as one unsampled stratum with the name provided by 'UnsampledStratum'. Inferred statistics will be reported for all domains for this stratum.
#'  
#'  Inference about unsampled strata is not justified by sampling. It may be considered applicable if the following applies:
#'  \itemize{
#'    \item Other considerations indicate that frequencies and means should be similar between the same domains
#'     in the unsampeld stratum and the source stratum.
#'    \item The unsampled stratum will be agregated with sampled ones, and constitute a small volume compared to sampled strata.
#'     This can be inspected with \code{\link[RstoxFDA]{ReportFdaSampling}}.
#'  }
#'  
#' @param AnalyticalPopulationEstimateData \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with Estimates for the sampling frame
#' @param StoxLandingData Landing data for the entire fishery / target population
#' @param StratificationVariables vector of variables in StoxLandingData that should be used to partition the fishery, must be Stratification Variables in 'AnalyticalPopulationEstimateData'
#' @param Method method of inference beyond sampling frame.
#' @param UnsampledStratum name to use for unsamled stratum
#' @param SourceStratum name of the stratum to get means and frequencies for the Method 'SetToStratum'
#' @return \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with parameters for unsampled stratum, and StratificationVariables reduced to those mathced with landings
#' @md
#' @concept Analytical estimation
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}, \code{\link[RstoxFDA]{AnalyticalRatioEstimate}}, \code{\link[RstoxFDA]{AggregateAnalyticalEstimate}}, \code{\link[RstoxFDA]{InterpolateAnalyticalDomainEstimates}}
#' @export
ExtendAnalyticalSamplingFrameCoverage <- function(AnalyticalPopulationEstimateData, StoxLandingData, StratificationVariables, Method=c("Strict", "SetToStratum"), UnsampledStratum=character(), SourceStratum=character()){
  
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  checkMandatory(StoxLandingData, "StoxLandingData")
  checkLandingsNotEmpty(StoxLandingData)
  checkMandatory(StratificationVariables, "StratificationVariables")
  checkOptions(Method, "Method", c("Strict", "SetToStratum"))
  checkMandatory(UnsampledStratum, "UnsampledStratum")
  if (Method == "SetToStratum"){
    checkMandatory(SourceStratum, "SourceStratum")
    if (!(SourceStratum %in% AnalyticalPopulationEstimateData$StratificationVariables$Stratum)){
      stop(paste("SourceStratum", SourceStratum, "is not a valid Stratum in 'AnalyticalPopulationEstimateData'"))
    }
  }
  
  if (!is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData)){
    stop("Invalid 'AnalyticalPopulationEstimateData'")
  }
  
  if (!all(StratificationVariables %in% c(names(AnalyticalPopulationEstimateData$StratificationVariables)))){
    stop("StratificationVariables are not found in 'AnalyticalPopulationEstimateData'")
  }
  
  if (length(StratificationVariables)==0){
   stop("No stratification variables provided") 
  }
  
  if (UnsampledStratum %in% AnalyticalPopulationEstimateData$StratificationVariables$Stratum){
    stop("Cannot use an existing stratum name for the unsampled stratum.")
  }
  
  # check that landingpartition identify strata
  # remove other stratification variables
  AnalyticalPopulationEstimateData$StratificationVariables <- AnalyticalPopulationEstimateData$StratificationVariables[,.SD,.SDcols=c("Stratum", StratificationVariables)]
  stratacount <- AnalyticalPopulationEstimateData$StratificationVariables[,list(totalStrata=length(unique(get("Stratum")))), by=list(stratavars=apply(AnalyticalPopulationEstimateData$StratificationVariables[,.SD,.SDcols=StratificationVariables], 1, paste, collapse="/"))]
  stratacount <- stratacount[get("totalStrata")>1]
  if (nrow(stratacount)>0){
    stop(paste("Stratification variables does not identify strata. Several strata identified for:", truncateStringVector(stratacount$stratavars)))
  }
  AnalyticalPopulationEstimateData$StratificationVariables <- AnalyticalPopulationEstimateData$StratificationVariables[!duplicated(
    apply(AnalyticalPopulationEstimateData$StratificationVariables[,.SD,.SDcols=StratificationVariables], 1, paste, collapse="/")
  ),]

    extendedAnalyticalPopulationEstimateData <- AnalyticalPopulationEstimateData
  
    landedpart <- StoxLandingData$Landing[,.SD,.SDcol=StratificationVariables]
    landedpart <- landedpart[!duplicated(apply(landedpart, 1, paste, collapse=",")),]
    sampleframepart <- extendedAnalyticalPopulationEstimateData$StratificationVariables[,.SD,.SDcol=StratificationVariables]
    unsampledpart <- landedpart[!(apply(landedpart, 1, paste, collapse=",") %in% apply(sampleframepart, 1, paste, collapse=",")),]
    unsampledpart$Stratum <- UnsampledStratum
    unsampledpart <- unsampledpart[,.SD,.SDcol=c("Stratum", StratificationVariables)]
  
    extendedAnalyticalPopulationEstimateData$StratificationVariables <- rbind(extendedAnalyticalPopulationEstimateData$StratificationVariables, 
                                                                      unsampledpart)
    #set samples in these strata to 0
    sampleCount <- data.table::CJ(Stratum=unsampledpart$Stratum, Domain=AnalyticalPopulationEstimateData$DomainVariables$Domain, unique = T)
    sampleCount$nPSU <- 0
    sampleCount$nIndividuals <- 0
    
    extendedAnalyticalPopulationEstimateData$SampleCount <- rbind(extendedAnalyticalPopulationEstimateData$SampleCount, 
                                                                  sampleCount)
    
  if (Method == "Strict"){
    return(fillStrict(extendedAnalyticalPopulationEstimateData))
  }
  
  else if (Method == "SetToStratum"){
    return(fillSetToStratum(extendedAnalyticalPopulationEstimateData, SourceStratum, UnsampledStratum))
  }
  
  else{
    stop(paste("Option", Method, "for argument 'Method' is not recognized"))
  }

}

#' Fill in stratum means for unsampled domain variables
#' 
#' @noRd
fillDomainStratumMean <- function(zeroDomainEstimate, DomainVariables, epsilon){

  #
  # construct original estimate (only non-zero marginal domains)
  #
  
  abundanceTable <- merge(zeroDomainEstimate$Abundance, zeroDomainEstimate$DomainVariables, by="Domain")
  abundanceByDomain <- abundanceTable[,list(totalAbundance=sum(get("Abundance"), na.rm=T)), by=c("Stratum", DomainVariables)]
  nonzeroAbundance <- abundanceByDomain[get("totalAbundance")>epsilon,]
  nonzeroAbundanceDomains <- merge(nonzeroAbundance, zeroDomainEstimate$DomainVariables, by=DomainVariables)
  
  originalEstimate <- zeroDomainEstimate
  originalEstimate$Abundance <- originalEstimate$Abundance[paste(originalEstimate$Abundance$Stratum,
                                                                 originalEstimate$Abundance$Domain) %in%
                                                             paste(nonzeroAbundanceDomains$Stratum,
                                                                   nonzeroAbundanceDomains$Domain),]
  
  originalEstimate$Variables <- originalEstimate$Variables[paste(originalEstimate$Variables$Stratum,
                                                                 originalEstimate$Variables$Domain) %in%
                                                             paste(nonzeroAbundanceDomains$Stratum,
                                                                   nonzeroAbundanceDomains$Domain),]
  
  originalEstimate$AbundanceCovariance <- originalEstimate$AbundanceCovariance[paste(originalEstimate$AbundanceCovariance$Stratum,
                                                                 originalEstimate$AbundanceCovariance$Domain1) %in%
                                                             paste(nonzeroAbundanceDomains$Stratum,
                                                                   nonzeroAbundanceDomains$Domain) &
                                                               paste(originalEstimate$AbundanceCovariance$Stratum,
                                                                     originalEstimate$AbundanceCovariance$Domain2) %in%
                                                               paste(nonzeroAbundanceDomains$Stratum,
                                                                     nonzeroAbundanceDomains$Domain),]
  
  originalEstimate$VariablesCovariance <- originalEstimate$VariablesCovariance[paste(originalEstimate$VariablesCovariance$Stratum,
                                                                                     originalEstimate$VariablesCovariance$Domain1) %in%
                                                                                 paste(nonzeroAbundanceDomains$Stratum,
                                                                                       nonzeroAbundanceDomains$Domain) &
                                                                                 paste(originalEstimate$VariablesCovariance$Stratum,
                                                                                       originalEstimate$VariablesCovariance$Domain2) %in%
                                                                                 paste(nonzeroAbundanceDomains$Stratum,
                                                                                       nonzeroAbundanceDomains$Domain),]
  originalEstimate$DomainVariables <- originalEstimate$DomainVariables[originalEstimate$DomainVariables$Domain %in%
                                                                                       nonzeroAbundanceDomains$Domain,]
  
  originalEstimate$SampleCount <- originalEstimate$SampleCount[paste(originalEstimate$SampleCount$Stratum,
                                                                     originalEstimate$SampleCount$Domain) %in%
                                                                 paste(nonzeroAbundanceDomains$Stratum,
                                                                       nonzeroAbundanceDomains$Domain),]
  
  keepVariables<-names(originalEstimate$DomainVariables)[!(names(originalEstimate$DomainVariables) %in% c("Domain", DomainVariables))]
  
  #
  # get frequency over marginal domain variables
  #
  frequencyTable <- merge(originalEstimate$Abundance, originalEstimate$DomainVariables, by="Domain")
  frequencies <- frequencyTable[,list(StratumFrequency=sum(get("Frequency"))), by=c("Stratum", keepVariables)]
  
  #
  # get frequency covariance over marginal domain variables
  #
  frequencyCovarTable <- merge(originalEstimate$AbundanceCovariance, originalEstimate$DomainVariables, by.x="Domain1", by.y="Domain", all.x=T)
  frequencyCovarTable <- merge(frequencyCovarTable, originalEstimate$DomainVariables, by.x="Domain2", by.y="Domain", suffixes = c("1", "2"), all.x=T)
  frequencyCovar <- frequencyCovarTable[,list(StratumFrequencyCovariance=sum(get("FrequencyCovariance"))), by=c("Stratum", paste(keepVariables, "1", sep=""), paste(keepVariables, "2", sep=""))]
  
  #
  # get means over marginal domain variables
  #
  meansTable <- merge(originalEstimate$Variables, originalEstimate$Abundance, by=c("Stratum", "Domain"))
  meansTable <- merge(meansTable, originalEstimate$DomainVariables, by="Domain")
  means <- meansTable[,list(StratumMean=sum(get("Mean")[!is.nan(get("Mean"))]*get("Frequency")[!is.nan(get("Mean"))])/sum(get("Frequency")[!is.nan(get("Mean"))])), by=c("Stratum", "Variable", keepVariables)]
  
  #
  # get mean covariance over marginal domain variables
  #
  
  meanCovarTable <- merge(originalEstimate$VariablesCovariance, originalEstimate$DomainVariables, by.x=c("Domain1"), by.y=c("Domain"), all.x=T)
  meanCovarTable <- merge(meanCovarTable, originalEstimate$DomainVariables, by.x="Domain2", by.y="Domain", suffixes = c("1", "2"), all.x=T)
  meanCovarTable <- merge(meanCovarTable, originalEstimate$Abundance, by.x=c("Stratum", "Domain1"), by.y=c("Stratum", "Domain"), all.x=T)
  meanCovarTable <- merge(meanCovarTable, originalEstimate$Abundance, by.x=c("Stratum", "Domain2"), by.y=c("Stratum", "Domain"), , suffixes = c("1", "2"), all.x=T)
  meanCovarTable <- merge(meanCovarTable, meansTable[,.SD,.SDcol=c("Stratum", "Domain", "Variable", "Mean")], 
                          by.x=c("Stratum", "Domain1", "Variable1"),
                          by.y=c("Stratum", "Domain", "Variable"))
  meanCovarTable <- merge(meanCovarTable, meansTable[,.SD,.SDcol=c("Stratum", "Domain", "Variable", "Mean")], 
                          by.x=c("Stratum", "Domain2", "Variable2"),
                          by.y=c("Stratum", "Domain", "Variable"), 
                          suffixes = c("1", "2"))
  meanCovar <- meanCovarTable[,list(StratumMeanCovariance=sum(get("MeanCovariance")[!is.nan(get("Mean1")) & !is.nan(get("Mean2"))]*get("Frequency1")[!is.nan(get("Mean1")) & !is.nan(get("Mean2"))]*get("Frequency2")[!is.nan(get("Mean1")) & !is.nan(get("Mean2"))])/sum(get("Frequency1")[!is.nan(get("Mean1")) & !is.nan(get("Mean2"))]*get("Frequency2")[!is.nan(get("Mean1")) & !is.nan(get("Mean2"))])), by=c("Stratum", "Variable1", "Variable2", paste(keepVariables, "1", sep=""), paste(keepVariables, "2", sep=""))]
  
  #
  # construct new sample count table
  #
  newSampleCountTable <- zeroDomainEstimate$SampleCount
  newDomain <- !(apply(newSampleCountTable[,.SD,.SDcols = c("Stratum", "Domain")], 1, paste, collapse="/") %in%
                   apply(originalEstimate$SampleCount[,.SD,.SDcols = c("Stratum", "Domain")], 1, paste, collapse="/"))
  newSampleCountTable$nPSU[newDomain] <- 0
  newSampleCountTable$nIndividuals[newDomain] <- 0
  
  #
  # construct new abundance table
  # frequencies for unsampled domains are adjusted by a low value (epsilon)
  #
  newAbundanceTable <- merge(zeroDomainEstimate$Abundance, zeroDomainEstimate$DomainVariables, by="Domain")
  newAbundanceTable <- merge(newAbundanceTable, frequencies, by=c("Stratum", keepVariables), all.x=T)
  newDomain <- !(apply(newAbundanceTable[,.SD,.SDcols = c("Stratum", DomainVariables)], 1, paste, collapse="/") %in%
    apply(frequencyTable[,.SD,.SDcols = c("Stratum", DomainVariables)], 1, paste, collapse="/"))
  newAbundanceTable$Frequency[newDomain] <- newAbundanceTable$StratumFrequency[newDomain] * epsilon
  newAbundanceTable$Frequency[!newDomain] <- newAbundanceTable$Frequency[!newDomain] * (1-epsilon*(sum(newDomain)/sum(!newDomain)))
  newAbundanceTable$Abundance[newDomain] <- as.numeric(NA)
  newAbundanceTable <- newAbundanceTable[,.SD,.SDcols = names(originalEstimate$Abundance)]
  
  #
  # construct new abundance variance table
  # frequencies for unsampled domains are set to very low value, controlled by epsilon
  #
  frequencyCovarNewDomains <- merge(zeroDomainEstimate$DomainVariables, frequencyCovar, by.x=keepVariables, by.y=paste0(keepVariables, "1"), allow.cartesian = T)
  frequencyCovarNewDomains <- merge(zeroDomainEstimate$DomainVariables, frequencyCovarNewDomains, by.x=keepVariables, by.y=paste0(keepVariables, "2"), suffixes = c("1","2"), allow.cartesian = T)
  
  #copy the covariance table constructed by Method 'Strict'
  newAbundanceVarianceTable <- zeroDomainEstimate$AbundanceCovariance
  
  #
  # We will set covariances for any new domains that are in the same marginal domain (same value for DomainVariables)
  # cross marginal-domain covariances for new domians will be NA
  #
  newDomain1 <- !(newAbundanceVarianceTable$Domain1 %in%
    originalEstimate$DomainVariables$Domain)
  newDomain2 <- !(newAbundanceVarianceTable$Domain2 %in%
    originalEstimate$DomainVariables$Domain)
  sameMarginalDomain <- apply(zeroDomainEstimate$DomainVariables[match(newAbundanceVarianceTable$Domain1, zeroDomainEstimate$DomainVariables$Domain),.SD,.SDcols=DomainVariables], 1, paste, collapse="/") ==
    apply(zeroDomainEstimate$DomainVariables[match(newAbundanceVarianceTable$Domain2, zeroDomainEstimate$DomainVariables$Domain),.SD,.SDcols=DomainVariables], 1, paste, collapse="/")
  
  # look up Domains either way (pmin, pmax), so that Domain1=a, Domain2=b is equal to Domain1=b, Domain2=a
  newAbundanceVarianceTable$FrequencyCovariance[newDomain1 & newDomain2 & sameMarginalDomain] <- 
    frequencyCovarNewDomains$StratumFrequencyCovariance[match(
      paste(newAbundanceVarianceTable$Stratum[newDomain1 & newDomain2 & sameMarginalDomain],
            pmin(newAbundanceVarianceTable$Domain1[newDomain1 & newDomain2 & sameMarginalDomain],
                 newAbundanceVarianceTable$Domain2[newDomain1 & newDomain2 & sameMarginalDomain]),
            pmax(newAbundanceVarianceTable$Domain1[newDomain1 & newDomain2 & sameMarginalDomain],
            newAbundanceVarianceTable$Domain2[newDomain1 & newDomain2 & sameMarginalDomain])), 
      paste(frequencyCovarNewDomains$Stratum,
            pmin(frequencyCovarNewDomains$Domain1,
            frequencyCovarNewDomains$Domain2),
            pmax(frequencyCovarNewDomains$Domain1,
                 frequencyCovarNewDomains$Domain2)))] * (epsilon**2)
  newAbundanceVarianceTable$AbundanceCovariance[newDomain1 & newDomain2 & sameMarginalDomain] <- as.numeric(NA)
  newAbundanceVarianceTable$FrequencyCovariance[(newDomain1 | newDomain2) & !sameMarginalDomain] <- as.numeric(NA)
  newAbundanceVarianceTable$AbundanceCovariance[(newDomain1 | newDomain2) & !sameMarginalDomain] <- as.numeric(NA)
  newAbundanceVarianceTable$FrequencyCovariance[(!newDomain1 & !newDomain2)] <- newAbundanceVarianceTable$FrequencyCovariance[(!newDomain1 & !newDomain2)] * (1-epsilon*(sum(newDomain)/sum(!newDomain)))**2
  
  #
  # construct new variable table
  #
  newTotalTable <- merge(zeroDomainEstimate$Variables, zeroDomainEstimate$DomainVariables, by="Domain")
  newTotalTable <- merge(newTotalTable, means, by=c("Stratum", "Variable", keepVariables), all.x=T)
  newDomain <- !(apply(newTotalTable[,.SD,.SDcols = c("Stratum", "Variable", DomainVariables)], 1, paste, collapse="/") %in%
                   apply(meansTable[,.SD,.SDcols = c("Stratum", "Variable", DomainVariables)], 1, paste, collapse="/"))
  newTotalTable$Mean[newDomain] <- newTotalTable$StratumMean[newDomain]
  newTotalTable$Total[newDomain] <- as.numeric(NA)
  newTotalTable <- newTotalTable[,.SD,.SDcols = names(originalEstimate$Variables)]
  
  #
  # construct new variables variance table
  #

  meansCovarNewDomains <- merge(zeroDomainEstimate$DomainVariables, meanCovar, by.x=keepVariables, by.y=paste0(keepVariables, "1"), allow.cartesian = T)
  meansCovarNewDomains <- merge(zeroDomainEstimate$DomainVariables, meansCovarNewDomains, by.x=keepVariables, by.y=paste0(keepVariables, "2"), suffixes = c("1","2"), allow.cartesian = T)
  
  #copy the covariance table constructed by Method 'Strict'
  newVariableVarianceTable <- zeroDomainEstimate$VariablesCovariance
  
  #
  # We will set covariances for any new domains that are in the same marginal domain (same value for DomainVariables)
  # cross marginal-domain covariances for new domians will be NA
  #
  newDomain1 <- !(newVariableVarianceTable$Domain1 %in%
                    originalEstimate$DomainVariables$Domain)
  newDomain2 <- !(newVariableVarianceTable$Domain2 %in%
                    originalEstimate$DomainVariables$Domain)
  sameMarginalDomain <- apply(zeroDomainEstimate$DomainVariables[match(newVariableVarianceTable$Domain1, zeroDomainEstimate$DomainVariables$Domain),.SD,.SDcols=DomainVariables], 1, paste, collapse="/") ==
    apply(zeroDomainEstimate$DomainVariables[match(newVariableVarianceTable$Domain2, zeroDomainEstimate$DomainVariables$Domain),.SD,.SDcols=DomainVariables], 1, paste, collapse="/")
  
  # look up Domains either way (pmin, pmax), so that Domain1=a, Domain2=b is equal to Domain1=b, Domain2=a
  newVariableVarianceTable$MeanCovariance[newDomain1 & newDomain2 & sameMarginalDomain] <- 
    meansCovarNewDomains$StratumMeanCovariance[match(
      paste(newVariableVarianceTable$Stratum[newDomain1 & newDomain2 & sameMarginalDomain],
            pmin(newVariableVarianceTable$Domain1[newDomain1 & newDomain2 & sameMarginalDomain],
                 newVariableVarianceTable$Domain2[newDomain1 & newDomain2 & sameMarginalDomain]),
            pmax(newVariableVarianceTable$Domain1[newDomain1 & newDomain2 & sameMarginalDomain],
                 newVariableVarianceTable$Domain2[newDomain1 & newDomain2 & sameMarginalDomain]),
            pmin(newVariableVarianceTable$Variable1[newDomain1 & newDomain2 & sameMarginalDomain],
                 newVariableVarianceTable$Variable2[newDomain1 & newDomain2 & sameMarginalDomain]),
            pmax(newVariableVarianceTable$Variable1[newDomain1 & newDomain2 & sameMarginalDomain],
                 newVariableVarianceTable$Variable2[newDomain1 & newDomain2 & sameMarginalDomain])), 
      paste(meansCovarNewDomains$Stratum,
            pmin(meansCovarNewDomains$Domain1,
                 meansCovarNewDomains$Domain2),
            pmax(meansCovarNewDomains$Domain1,
                 meansCovarNewDomains$Domain2),
            pmin(meansCovarNewDomains$Variable1,
                 meansCovarNewDomains$Variable2),
            pmax(meansCovarNewDomains$Variable1,
                 meansCovarNewDomains$Variable2)))]
  newVariableVarianceTable$TotalCovariance[newDomain1 & newDomain2 & sameMarginalDomain] <- as.numeric(NA)
  newVariableVarianceTable$MeanCovariance[(newDomain1 | newDomain2) & !sameMarginalDomain] <- as.numeric(NA)
  newVariableVarianceTable$TotalCovariance[(newDomain1 | newDomain2) & !sameMarginalDomain] <- as.numeric(NA)
  
  #
  # Update object with new tables
  #
  
  stopifnot(nrow(zeroDomainEstimate$Abundance)==nrow(newAbundanceTable))
  stopifnot(nrow(zeroDomainEstimate$Variables)==nrow(newTotalTable))
  stopifnot(nrow(zeroDomainEstimate$AbundanceCovariance)==nrow(newAbundanceVarianceTable))
  stopifnot(nrow(zeroDomainEstimate$VariablesCovariance)==nrow(newVariableVarianceTable))
  
  zeroDomainEstimate$Abundance <- newAbundanceTable
  zeroDomainEstimate$Variables <- newTotalTable
  zeroDomainEstimate$AbundanceCovariance <- newAbundanceVarianceTable
  zeroDomainEstimate$VariablesCovariance <- newVariableVarianceTable
  zeroDomainEstimate$SampleCount <- newSampleCountTable
  return(zeroDomainEstimate)
}

#' Interpolate means and frequencies for zero-abundance domains
#' @description
#'  Interpolate means and frequences for domains with no estimated abundance (domains not sampled)
#' @details
#'  This function infers parameters for unsampled domains that are present in census data (landings).
#'  This function only infers precence of unsampled domains from census-data, and provide options for some pragmatic approximations to substitute for estimates for these domains.
#'  The function does not introduce landed weights, or other knowledge from landings, 
#'  except for the fact that domains are present in the landings data. All inference about unkown values are taken from the provided estimates ('AnalyticalPopulationEstimateData').
#'  
#'  The Domain-variables provided in the argument 'DomainMarginVariables' are compared with landings ('StoxLandingData')
#'  to detect if the census contains values and combinations for these variables that are not present in the samples.
#'  Corresponding domains are then introduced into the analytical estimates results according to the option for the argument 'Method'
#'  
#'  Subsequent ratio-estimation may make use of this information to also make use of total-weight information from landings (see \code{link[RstoxFDA]{AnalyticalRatioEstimate}}).
#'  For design based approaches, unsampled domains have estimates of zero abundance, frequencies and totals, and hence undefined means.
#'  Such zero-domains may be only implicitly encoded in 'AnalyticalPopulationEstimateData', 
#'  and this function may make that encoding explicit for by use of the option 'Strict' for Method.
#'  The 'Strict' Method introduces unsampled domains with abundance, total and frequencies of 0, and NaN means.
#'  
#'  In order to prepare ratio-estimation (see. \code{\link[RstoxFDA]{AnalyticalRatioEstimate}}), it may be desirable to infer some plausible values for means and frequencies of unsampled domains.
#'  This is facilitated by the option 'StratumMean' for 'Method'. This method will calculate aggregate statistcs for each stratum over the
#'  domain variables, and use this average for means and frequencies for the marginal domains that have zero abundance. Marginal domains are
#'  domains defined by combining statistics for all other domain variables than those identified in DomainMarginVariables.
#'  
#'  For instance, one may one to infer frequencies and means for unsampled gears, as the mean of all sampled gears in a stratum.
#'  This can be obtained by providing DomainMarginVariables='Gear', or unsampled combinations of gears and quarters could be similarly
#'  specifyed by DomainMarginVariables=c('Gear','Quarter'). The DomainMarginVariables must be domain variables in 'AnalyticalPopulationEstimateData'
#'  and are typically PSU-domains (see. \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}).
#'  
#'  Since frequencies are normalized to strata, they need to be re-normalized after inclusion of positive frequencies in these domains.
#'  In order to reflect their low abundance in the sampling frame, the frequencies are scaled to a low-value provided in the argument 'Epsilon'.
#'  Epsilon indicate the precense of these domains, as deduced from census-data, but should be chosen low enough to be considered practically zero, 
#'  to be consistent with the result of sampling-based estimation. For this reason, abundances and totals are set to NA for these domains,
#'  but frequencies need to present to capture for instance age-distributions in the domain.
#'  
#'  Subsequent ratio-estimation may provide relastic estimates of abundances and relative frequencies between PSU-domains.
#'  
#'  The domain estimates in 'AnalyticalPopulationEstimateData' are identied by a stratum \eqn{s} and a set of domain variables \eqn{D}.
#'  The domain variables can be divided into the set \eqn{M} and \eqn{N}, where \eqn{M} are the 'DomainMarginVariables'. Let \eqn{(m+n)}
#'  denote a domain defined by the combination of the variables \eqn{m} and \eqn{n}.
#'  For each \eqn{d \in N}, we define the stratum statistics:
#'  \describe{
#'    \item{frequency}{
#'    \deqn{\widehat{f}^{(s,d)}=\sum_{m \in M}\widehat{f}^{(s,d+m)}}
#'    
#'    with covariance:
#'    
#'    \deqn{\widehat{CoVar}(\widehat{f}^{(s,d_{1})}, \widehat{f}^{(s,d_{2})})=\sum_{m \in M}\widehat{CoVar}(\widehat{f}^{(s,d{1}+m)}, \widehat{f}^{(s,d_{2}+m)})}
#'    }
#'    \item{mean}{
#'    \deqn{\widehat{\mu}^{(s,d,v)}=\frac{1}{\sum_{m \in M}I(s,d+m)\widehat{f}^{(s,d+m)}}\sum_{m \in M}I(s,d+m)\widehat{f}^{(s,d+m)}\widehat{\mu}^{(s,d+m,v)}}
#'    where \eqn{v} denote the variable that the mean is estimated for, and \eqn{I(s,d)} is an indicator functon that is 1
#'    if a mean is defined for domain \eqn{d} in stratum \eqn{s}, otherwise 0.
#'    
#'    with covariance:
#'    
#'    \deqn{\widehat{CoVar}(\widehat{\mu}^{(s,d_{1},v_{1})}, \widehat{\mu}^{(s,d_{2},v_{2})})=\frac{1}{\sum_{m \in M}z^{s,d_{1},d_{2},m}}\sum_{m \in M}z^{s,d_{1},d_{2},m}\widehat{CoVar}(\widehat{\mu}^{(s,d_{1}+m, v_{1})}, \widehat{\mu}^{(s,d_{2}+m, v_{2})})}
#'    where:
#'    \deqn{z^{s,d_{1},d_{2},m}=I(s,d_{1}+m)I(s,d_{2}+m)\widehat{f}^{(s,d_{1}+m)}\widehat{f}^{(s,d_{2}+m)}}
#'    }
#'  }
#'  
#'  These stratum statistics are used to interpolate to zero-abundance domains, as follows.
#'  Let \eqn{D'} denote the combinations of 'DomainMarginVariables' that have zero estimated abundance.
#'  For each \eqn{d \in N} and \eqn{n' \in D'}, statistics are defined as:
#'  
#'  \describe{
#'    \item{frequency}{
#'    \deqn{\widehat{f}^{(s,d+n')}=\epsilon\widehat{f}^{(s,d)}}
#'    where \eqn{\epsilon} corresponds to the argument 'Epsilon'
#'    
#'    with covariance:
#'    
#'    \deqn{\widehat{CoVar}(\widehat{f}^{(s,d_{1}+n')}, \widehat{f}^{(s,d_{2}+n')})=\epsilon^{2}\widehat{CoVar}(\widehat{f}^{(s,d_{1})}, \widehat{f}^{(s,d_{2})})}
#'    }
#'    \item{mean}{
#'    \deqn{\widehat{\mu}^{(s,d+n',v)}=\widehat{\mu}^{(s,d,v)}}
#'    
#'    with covariance:
#'    
#'    \deqn{\widehat{CoVar}(\widehat{\mu}^{(s,d_{1}+n',v_{1})}, \widehat{\mu}^{(s,d_{2}+n',v_{2})})=\widehat{CoVar}(\widehat{\mu}^{(s,d_{1},v_{1})}, \widehat{\mu}^{(s,d_{2},v_{2})})}
#'    }
#'  }
#'  
#'  To renormalize frequencies, a minor adjustment is also made to the non-zero abundance domains:
#'  
#'  Let \eqn{D''} denote the combinations of 'DomainMarginVariables' that have positive estimated abundance.
#'  For each \eqn{d \in N} and \eqn{n'' \in D''}, the frequency is defined as:
#'  
#'  \deqn{\widehat{f{*}}^{(s,d+n'')}=(1-\epsilon r)\widehat{f}^{(s,d+n'')}}
#'  where \eqn{r=\frac{|D'|}{|D''|}}, the ratio of zero-abundance domains to non-zero-abundance marginal domains.
#'  
#'  Similarly, the covariance is:
#'  \deqn{\widehat{CoVar^{*}}(\widehat{f}^{(s,d_{1}+n'')}, \widehat{f}^{(s,d_{2}+n'')}=(1-\epsilon r)^{2}\widehat{CoVar}(\widehat{f}^{(s,d_{1}+n'')}, \widehat{f}^{(s,d_{2}+n'')})}
#'  
#'  
#' @param AnalyticalPopulationEstimateData \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with analytical estimates
#' @param StoxLandingData Landing data for the entire fishery / target population
#' @param Method method for inferring means and frequencies. See details.
#' @param DomainMarginVariables Domain Variables used for interpolation with the 'StratumMean' Method. Must be variables in StoxLandingData and Domain Variables in 'AnalyticalPopulationEstimateData'. See details.
#' @param Epsilon factor for representing relative frequencies for 0-abundance domains, used for interpolation with the 'StratumMean' Method. See details.
#' @return \code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}} with parameters for unsampled domains
#' @md
#' @concept Analytical estimation
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}}, \code{\link[RstoxFDA]{AnalyticalRatioEstimate}}, \code{\link[RstoxFDA]{AggregateAnalyticalEstimate}}, \code{\link[RstoxFDA]{ExtendAnalyticalSamplingFrameCoverage}}
#' @export
InterpolateAnalyticalDomainEstimates <- function(AnalyticalPopulationEstimateData, StoxLandingData, Method=c("Strict", "StratumMean"), DomainMarginVariables=character(), Epsilon=numeric()){
  
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  checkMandatory(StoxLandingData, "StoxLandingData")
  checkLandingsNotEmpty(StoxLandingData)
  Method <- checkOptions(Method, "Method", c("Strict", "StratumMean"))
  if (Method=="StratumMean"){
    checkMandatory(DomainMarginVariables, "DomainMarginVariables")
    checkMandatory(Epsilon, "Epsilon")
    if (!(Epsilon > 0 & Epsilon < 1)){
      stop("Argument 'Epsilon' must be between 0 and 1.")
    }
  }
  
  if (!is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData)){
    stop("Invalid 'AnalyticalPopulationEstimateData'")
  }
  
  if (!all(DomainMarginVariables %in% names(AnalyticalPopulationEstimateData$DomainVariables))){
    stop("DomainMarginVariables must all be Domain Variables in 'AnalyticalPopulationEstimateData'.")
  }
  
  if (length(DomainMarginVariables)==0){
    stop("'DomainMarginVariables' does not contain any variables that are domain variables in 'AnalyticalPopulationEstimateData'")
  }
  
  extendedAnalyticalPopulationEstimateData <- AnalyticalPopulationEstimateData
  
  # Add domains with 0 abundance, frequency, and total, and NA mean for each unsampled variable that is a domain variable
  
    
    #
    # Construct unsampled domains that need to be filled to match landings
    #
    landeddomains <- StoxLandingData$Landing[,.SD,.SDcol=DomainMarginVariables]
    landeddomains <- landeddomains[!duplicated(apply(landeddomains, 1, paste, collapse=",")),]
    sampledfractionaldomains <- extendedAnalyticalPopulationEstimateData$DomainVariables[,.SD,.SDcol=DomainMarginVariables]
    unsampledfractionaldomains <- landeddomains[!(apply(landeddomains, 1, paste, collapse=",") 
                                                  %in% apply(sampledfractionaldomains, 1, paste, collapse=",")),]
    unsampledfractionaldomains$Domain <- "newdomain"
    
    otherDomainVariables <- extendedAnalyticalPopulationEstimateData$DomainVariables
    otherDomainVariables <- otherDomainVariables[,.SD,.SDcol=names(otherDomainVariables)[!(names(otherDomainVariables) %in% DomainMarginVariables)],]
    otherDomainVariables$Domain <- "newdomain"
    otherDomainVariables <- otherDomainVariables[!(duplicated(apply(otherDomainVariables, 1, paste, collapse=","))),]
    
    additionalDomains <- merge(otherDomainVariables, unsampledfractionaldomains, by="Domain", allow.cartesian = T)
    additionalDomains <- additionalDomains[,.SD,.SDcol=names(extendedAnalyticalPopulationEstimateData$DomainVariables)]
    additionalDomains$Domain <- "All"
    for (domainCol in names(additionalDomains)[!(names(additionalDomains) %in% c("Domain"))]){
      additionalDomains$Domain <- paste(additionalDomains$Domain, paste(domainCol, additionalDomains[[domainCol]], sep=":"), sep="/")
    }
    additionalDomains <- additionalDomains[!duplicated(additionalDomains$Domain),]
    
    extendedAnalyticalPopulationEstimateData$DomainVariables <- rbind(extendedAnalyticalPopulationEstimateData$DomainVariables, additionalDomains)
    stopifnot(all(!duplicated(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain)))
    
    #
    # Infer zero domains for sampling frame, same for all methods
    #
    
    additionalSampleCount <- data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                            Domain=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain))
    additionalSampleCount <- additionalSampleCount[!(paste(additionalSampleCount$Stratum, additionalSampleCount$Domain) 
                                                 %in% paste(extendedAnalyticalPopulationEstimateData$SampleCount$Stratum, 
                                                            extendedAnalyticalPopulationEstimateData$SampleCount$Domain)),]
    additionalSampleCount$nPSU <- 0
    additionalSampleCount$nIndividuals <- 0
    
    additionalAbundance <- data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                          Domain=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain))
    additionalAbundance <- additionalAbundance[!(paste(additionalAbundance$Stratum, additionalAbundance$Domain) 
                                                 %in% paste(extendedAnalyticalPopulationEstimateData$Abundance$Stratum, 
                                                            extendedAnalyticalPopulationEstimateData$Abundance$Domain)),]
    additionalAbundance$Abundance <- 0
    additionalAbundance$Frequency <- 0
    
    additionalVariables <- data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                          Domain=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain), 
                                          Variable=unique(extendedAnalyticalPopulationEstimateData$Variables$Variable))
    additionalVariables <- additionalVariables[!(paste(additionalVariables$Stratum, additionalVariables$Domain) %in% 
                                                   paste(extendedAnalyticalPopulationEstimateData$Variables$Stratum, 
                                                         extendedAnalyticalPopulationEstimateData$Variables$Domain)),]
    additionalVariables$Total <- 0
    additionalVariables$Mean <- NaN
    
    cross <- data.table::CJ(Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain), 
                            Domain2=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain))
    cross <- cross[cross$Domain1>=cross$Domain2,]
    cross$AbundanceCovariance <- 0
    cross$FrequencyCovariance <- 0
    
    additionalAbundanceCovariance <- merge(data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                                          Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain)),
                                           cross, by=c("Domain1"), allow.cartesian=T)
    additionalAbundanceCovariance <- additionalAbundanceCovariance[!(paste(additionalAbundanceCovariance$Stratum, 
                                                                           additionalAbundanceCovariance$Domain1, 
                                                                           additionalAbundanceCovariance$Domain2) %in% 
                                                                       paste(extendedAnalyticalPopulationEstimateData$AbundanceCovariance$Stratum, 
                                                                             extendedAnalyticalPopulationEstimateData$AbundanceCovariance$Domain1, 
                                                                             extendedAnalyticalPopulationEstimateData$AbundanceCovariance$Domain2)),]
    
    
    
    cross <- data.table::CJ(Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain), 
                            Variable1=unique(extendedAnalyticalPopulationEstimateData$Variables$Variable), 
                            Domain2=unique(unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain)), 
                            Variable2=unique(extendedAnalyticalPopulationEstimateData$Variables$Variable))
    cross <- cross[cross$Domain1>=cross$Domain2 & cross$Variable1 >= cross$Variable2,]
    cross$TotalCovariance <- 0
    cross$MeanCovariance <- NaN
    additionalVariableCovariance <- merge(data.table::CJ(Stratum=unique(extendedAnalyticalPopulationEstimateData$StratificationVariables$Stratum), 
                                                         Domain1=unique(extendedAnalyticalPopulationEstimateData$DomainVariables$Domain)),
                                          cross, by=c("Domain1"), allow.cartesian=T)
    additionalVariableCovariance <- additionalVariableCovariance[!(paste(additionalVariableCovariance$Stratum, 
                                                                         additionalVariableCovariance$Domain1, 
                                                                         additionalVariableCovariance$Domain2, 
                                                                         additionalVariableCovariance$Variable1, 
                                                                         additionalVariableCovariance$Variable2) %in% 
                                                                     paste(extendedAnalyticalPopulationEstimateData$VariablesCovariance$Stratum, 
                                                                           extendedAnalyticalPopulationEstimateData$VariablesCovariance$Domain1, 
                                                                           extendedAnalyticalPopulationEstimateData$VariablesCovariance$Domain2, 
                                                                           extendedAnalyticalPopulationEstimateData$VariablesCovariance$Variable1, 
                                                                           extendedAnalyticalPopulationEstimateData$VariablesCovariance$Variable2)),]
    
    extendedAnalyticalPopulationEstimateData$Abundance <- rbind(extendedAnalyticalPopulationEstimateData$Abundance, additionalAbundance)
    extendedAnalyticalPopulationEstimateData$Variables <- rbind(extendedAnalyticalPopulationEstimateData$Variables, additionalVariables)
    extendedAnalyticalPopulationEstimateData$AbundanceCovariance <- rbind(extendedAnalyticalPopulationEstimateData$AbundanceCovariance, 
                                                                          additionalAbundanceCovariance)
    extendedAnalyticalPopulationEstimateData$VariablesCovariance <- rbind(extendedAnalyticalPopulationEstimateData$VariablesCovariance, 
                                                                          additionalVariableCovariance)
    extendedAnalyticalPopulationEstimateData$SampleCount <- rbind(extendedAnalyticalPopulationEstimateData$SampleCount,
                                                                  additionalSampleCount)
  if (Method == "Strict"){
    return(extendedAnalyticalPopulationEstimateData)
  }
  
  if (Method == "StratumMean"){
    return(fillDomainStratumMean(extendedAnalyticalPopulationEstimateData, DomainMarginVariables, Epsilon))    
  }

  else{
    stop(paste("The option", Method, "for the argument 'Method' is not recognized."))
  }
    
}

#' Add length group to StoxBioticData
#' @description
#'  Adds a variable that groups individuals based on the variable 'IndividualTotalLength'.
#'  This allows length-groups to be defined as domains in analytical estimation.
#'  
#'  The groups are defined by the argument 'LengthInterval', which specify consecutive length groups of
#'  equal length range ('LengthInterval'), starting with length 0. The argument 'LeftOpen' specifies
#'  whether the intervals are open to the left (lower value) or to the right (higher value).
#'  
#'  For example LengthInterval=5, and LeftOpen=FALSE, specifies length-groups [0,5>,[5,10>,...
#'  
#'  Note that even though the length-groups are formatted as a character, they have a strict format that
#'  carry numerical information for downstream functions, such as \code{\link[RstoxFDA]{ReportAnalyticalCatchAtLength}}
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} with individuals to be grouped by length
#' @param LengthInterval The 'bin-size', length in cm between length-groups
#' @param LengthGroupVariable Name to use for the length group variable
#' @param LeftOpen logical, specifying whether intervals are left-open, or right-open.
#' @md
#' @examples
#'  StoxBioticWLengthGroup <- RstoxFDA:::AddLengthGroupStoxBiotic(RstoxFDA::CatchLotteryExample, 
#'        LengthInterval=5, LengthGroupVariable="LengthGroup", LeftOpen=TRUE)
#'  table(StoxBioticWLengthGroup$Individual$LengthGroup)
#' @concept Analytical estimation
#' @seealso \code{\link[RstoxFDA]{AnalyticalPSUEstimate}}, \code{\link[RstoxFDA]{ReportAnalyticalCatchAtLength}}
#' @export
AddLengthGroupStoxBiotic <- function(StoxBioticData, LengthInterval=numeric(), LengthGroupVariable=character(), LeftOpen=TRUE){
  checkMandatory(StoxBioticData, "StoxBioticData")
  checkMandatory(LengthInterval, "LengthInterval")
  checkMandatory(LengthGroupVariable, "LengthGroupVariable")
  checkMandatory(LeftOpen, "LeftOpen")
  
  for (tab in names(StoxBioticData)){
    if (LengthGroupVariable %in% names(StoxBioticData[[tab]])){
      stop(paste("The argument 'LengthGroupVariable' cannot be a variable already existing in 'StoxBioticData'.", LengthGroupVariable,"was found on the table:", tab))
    }
  }
  StoxBioticData$Individual[[LengthGroupVariable]] <- as.character(
    cut(StoxBioticData$Individual$IndividualTotalLength, 
        seq(0, max(StoxBioticData$Individual$IndividualTotalLength,na.rm=T)+LengthInterval, LengthInterval), 
        right=LeftOpen))
  StoxBioticData$Individual[[LengthGroupVariable]][is.na(StoxBioticData$Individual$IndividualTotalLength)] <- as.character(NA)
  return(StoxBioticData)
}