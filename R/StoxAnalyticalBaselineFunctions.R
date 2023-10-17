#' Construct design parameters assuming FSWOR, non-finite, equal prob, potentially stratified
#' @noRd
assumeDesignParametersStoxBiotic <- function(StoxBioticData, SamplingUnitId, StratificationColumns=c(), OrderColumn=NULL){
  stop("Not Implemented")
}

#' parse design parameters from tab delimited file
#' @noRd
parseDesignParameters <- function(filename){
  
  colClasses <- c(Stratum="character", N="numeric", n="numeric", SelectionMethod="character", Finite="logical", FrameDescription="character", Order="numeric", SamplingUnitId="character", InclusionProbability="numeric", SelectionProbability="numeric", SelectionDescription="character")
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

  selectionTable <- designParameters[,.SD,.SDcol=c("Stratum", "Order", "SamplingUnitId", "InclusionProbability", "SelectionProbability", "SelectionDescription")]
  sampleTable <- designParameters[,.SD,.SDcol=c("Stratum", names(designParameters)[!(names(designParameters) %in% names(selectionTable))])]
  stratificationTable <- data.table::data.table(StratificationVariables=c(stratificationColumns))

  if (any(is.na(sampleTable$Stratum)) | any(is.na(selectionTable$Stratum))){
    stop("Invalid design specification. The mandatory column 'Stratum' may not contain missing values (NA).")
  }
  if (any(is.na(sampleTable$Finite))){
    stop("Invalid design specification. The mandatory column 'Finite' may not contain missing values (NA).")
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
    stratificationVariableStrings <- apply(sampleTable[,.SD, .SDcol=stratificationColumns], 1, paste, collapse="/")
    duplicatedStrata <- sampleTable$Stratum[duplicated(stratificationVariableStrings)]
    
    if (length(duplicatedStrata)>0){
      stop(paste("Invalid design spesification. The stratification variables must uniquely identify a stratum. Duplicates found for:", paste(duplicatedStrata, collapse=",")))
    }
  }
  
  sampleTable <- sampleTable[!duplicated(sampleTable$Stratum),]

  validSelectionMethod <- c("Poisson", "FSWR", "FSWOR")
  if (!all(sampleTable$SelectionMethod %in% validSelectionMethod)){
    invalid <- sampleTable$SelectionMethod[!(sampleTable$SelectionMethod %in% validSelectionMethod)]
    stop(paste("Invalid design specification. Unkown selection method:", paste(invalid, collapse=",")))
  }
  
  designParameters <- list()
  designParameters$sampleTable <- sampleTable
  designParameters$selectionTable <- selectionTable
  designParameters$stratificationVariables <- stratificationTable
  
  return(designParameters)
}

#' Define Sampling Design Parameters
#' @description 
#'  Define sampling design parameters for use in analytical estimation.
#' @details 
#'  The DefintionMethod 'ResourceFile' reads design parameters from a tab delimited file with headers corresponding to those listed in 
#'  \code{\link[RstoxFDA]{SamplingDesignParametersData}}. The data is provided as one table, so that the information in 'sampleTable' is repeated for each entry in 'selectionTable'.
#'  Any columns not named in \code{\link[RstoxFDA]{SamplingDesignParametersData}} are assumed to be stratification variables.
#'  The conditions listed for the variables in \code{\link[RstoxFDA]{SamplingDesignParametersData}} are checked upon reading the data, and
#'  execution halts with error if any are violated.
#'  
#'  The DefinitionMethod 'AdHocStoxBiotic' constructs Sampling Design Parameters from data, 
#'  assuming equal probability non-finite sampling with fixed sample size, selection without replacement and complete response.
#' @param processData \code{\link[RstoxFDA]{SamplingDesignParametersData}} as returned from this function.
#' @param DefinitionMethod 'ResourceFile' or 'AdHocStoxBiotic'
#' @param FileName path to resource file
#' @param StoxBioticData
#' @param SamplingUnitId
#' @param StratificationColumns
#' @param OrderColumn
#' @param UseProcessData If TRUE, bypasses execution of function and returns existing 'processData'
#' @return \code{\link[RstoxFDA]{SamplingDesignParametersData}}
#' @export
#' @concept StoX-functions
#' @concept Analytical estimation
#' @md
DefineSamplingDesignParameters <- function(processData, DefinitionMethod=c("ResourceFile", "AdHocStoxBiotic"), FileName=character(), StoxBioticData, SamplingUnitId, StratificationColumns, OrderColumn, UseProcessData=F){

  if (UseProcessData){
    return(processData)
  }
  
  DefinitionMethod <- checkOptions(DefinitionMethod, "DefinitionMethod", c("ResourceFile", "AdHocStoxBiotic"))
  
  if (DefinitionMethod == "ResourceFile"){
    return(parseDesignParameters(FileName))
  }
  if (DefinitionMethod == "AdHocStoxBiotic"){
    return(assumeDesignParametersStoxBiotic(StoxBioticData, SamplingUnitId, StratificationColumns, OrderColumn))
  }
}




#' @noRd
AssignIndividualDesignParameters <- function(){}

#' @noRd
AssignPSUDesignParameters <- function(){}

#' @noRd
DefinePSUCoInclusionProbabilities <- function(){}

#' @noRd
ProbabilisticSuperIndividuals <- function(){}