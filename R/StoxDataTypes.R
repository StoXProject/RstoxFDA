#' @noRd
is.POSIXct <- function(date){
  if (length(date) > 1 & "POSIXct" %in% class(date)){
    return(TRUE)
  }
  if (length(date) == 1 & class(date) == "POSIXct"){
    return(TRUE)
  }
  
  return(FALSE)
}

#' @noRd
is.Date <- function(date){
  if (length(date) > 1 & "Date" %in% class(date)){
    return(TRUE)
  }
  if (length(date) == 1 & class(date) == "Date"){
    return(TRUE)
  }
  
  return(FALSE)
}

#' Checks if argument is \code{\link[RstoxData]{Translation}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxData]{Translation}}
#' @param Translation argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxData]{Translation}}
#' @export
is.Translation <- function(Translation){
  if (!data.table::is.data.table(Translation)){
    return(FALSE)
  }
  if (!all(c("VariableName", "Value",	"NewValue") %in% names(Translation))){
    return(FALSE)
  }
  return(TRUE)
}

#' Weight Conversion Table (WeightConversionTable)
#' 
#' @description 
#'  \code{\link[data.table]{data.table}} with factors for approximating the weight of a 
#'  desired product type (e.g. round fish)
#'  from weights of other fish products. Contains the columns:
#'  \describe{
#'  \item{'Description'}{Free-text description of the product type}
#'  \item{'Species'}{Identifier for the species that the conversion applies to}
#'  \item{'ProductType'}{Identifier for the type of product that the conversion applies to}
#'  \item{'WeightFactor'}{scalar value that weights for the given 'ProductType' can be multiplied with to approximate desired product type (e.g. round fish).}
#'  }
#'  NA is allowed for 'WeightFactor', which will result in NA for weights after conversion
#'  
#' @name WeightConversionTable
#' 
NULL

#' Check if argument is WeightConversionTable
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{WeightConversionTable}}
#' @param WeightConversionTable argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{WeightConversionTable}}
#' @export
is.WeightConversionTable <- function(WeightConversionTable){
  if (!data.table::is.data.table(WeightConversionTable)){
    return(FALSE)
  }
  if (!all(c("Description", "Species", "ProductType", "WeightFactor") %in% names(WeightConversionTable))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Sampling Report data (ReportFdaSamplingData)
#' 
#' @description 
#'  list with tow members:
#'  \describe{
#'   \item{AggregationVariables}{a \code{\link[data.table]{data.table}} with the variables used for aggregation in 'FishereisSampling' stored in the column 'AggregationVariables'}
#'   \item{FisheriesSampling}{a \code{\link[data.table]{data.table}} described below.}
#'  }
#'  
#'  FisheriesSampling is a report of sampling against total landings for partitions of a fishery.
#'  The report is a \code{\link[data.table]{data.table}} with columns:
#'  \describe{
#'   \item{...}{A column for each of the provided Aggregation variables}
#'   \item{LandedRoundWeight}{Total landings in kg}
#'   \item{Catches}{Number of catches sampled}
#'   \item{Vessels}{Number of vessels sampled}
#'   \item{WeightMeasurments}{Number of fished measured for weight}
#'   \item{LengthMeasurments}{Number of fished measured for length}
#'   \item{AgeReadings}{Number of fished with age determined}
#'   \item{WeightOfSampledCatches}{Total weight of the sampled catches}
#'  }
#' 
#' @name ReportFdaSamplingData
#' 
NULL

#' Check if argument is ReportFdaSamplingData
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @param ReportFdaSamplingData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @export
is.ReportFdaSamplingData <- function(ReportFdaSamplingData){
  if (!is.list(ReportFdaSamplingData)){
    return(FALSE)
  }
  if (!all(c("AggregationVariables", "FisheriesSampling") %in% names(ReportFdaSamplingData))){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaSamplingData$FisheriesSampling)){
    return(FALSE)
  }
  
  if (!all(c("LandedRoundWeight", "Catches", "Vessels", "WeightMeasurments", "LengthMeasurments", "AgeReadings", "WeightOfSampledCatches") %in% names(ReportFdaSamplingData$FisheriesSampling))){
    return(FALSE)
  }
  
  return(TRUE)
  
}

#' Reca Data (RecaData)
#'
#' Data and some data parameters prepared for running
#' \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' via \code{\link[RstoxFDA]{RunRecaEstimate}}.
#'
#' @details
#' \describe{
#'  \item{AgeLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{WeightLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{Landings}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{GlobalParameters}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. see details}
#'  \item{CovariateMaps}{Mapping of values for each covariate in landings and samples (including non-configurable catchId) to integer value used in R-ECA.}
#' }
#'
#' @name RecaData
#'
NULL

#' Check if argument is RecaData
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaData}}
#' @param RecaData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaData}}
#' @export
is.RecaData <- function(RecaData){
  if (!is.list(RecaData)){
    return(FALSE)
  }
  if (!all(c("AgeLength", "WeightLength", "Landings", "GlobalParameters", "CovariateMaps") %in% names(RecaData))){
    return(FALSE)
  }
  if (!is.list(RecaData$AgeLength)){
    return(FALSE)
  }
  if (!is.list(RecaData$WeightLength)){
    return(FALSE)
  }
  if (!is.list(RecaData$Landings)){
    return(FALSE)
  }
  if (!is.list(RecaData$GlobalParameters)){
    return(FALSE)
  }
  if (!is.list(RecaData$CovariateMaps)){
    return(FALSE)
  }
  
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaData$AgeLength))){
    return(FALSE)
  }
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaData$WeightLength))){
    return(FALSE)
  }
  if (!all(c("AgeLengthCov", "WeightLengthCov", "LiveWeightKG") %in% names(RecaData$Landings))){
    return(FALSE)
  }
  
  return(TRUE)
}


#' Reca Parameter Data (RecaParameterData)
#'
#' @description 
#' Data and some data parameters prepared for running
#' various report functions that invoke \code{\link[Reca]{eca.predict}}.
#'
#' @section model fit:
#'  For inspection or analysis of model fit, the lists 'FitProportionAtAge', 
#'  'FitLengthGivenAge' and 'FitWeightGivenLength' is of interest. 
#'  These lists correspond to the three Reca-models and contain:
#'  \describe{
#'  \item{LogLikeliehood}{A \code{\link[data.table]{data.table}} 
#'    tabulating the logarithm of the likeliehood of the parameter set for each iteration}
#'  \item{...}{A \code{\link[data.table]{data.table}} for each of the model effects (e.g. covariates).}
#'  }
#'  
#'  In addition to configurable covariates, the models always contain a constant effect (named 'constant'),
#'  a catch or haul effect (named 'catchSample') and effects for fish measurments (named 'fish'). 
#'  Where relevant the following parameters may be tabulated for each effect:
#'  \describe{
#'  \item{Age}{Identifying the age the effect applies to}
#'  \item{Level}{Identifying the value or level of the covariate the effect applies to}
#'  \item{Iteration}{Identifying the iteration the fit is provided for}
#'  \item{AgeIndex}{Age identifier used internally in Reca}
#'  \item{LevelIndex}{Level identifier used internally in Reca}
#'  \item{Slope}{The value of the regression slope}
#'  \item{tau_Slope}{The value of tau parameter for the regression slope}
#'  \item{ar_Slope}{The value of regression slope of a the autoregressive coefficient associated with the effect}
#'  \item{Intercept}{The value of the regression intercept}
#'  \item{tau_Intercept}{The value of tau parameter for the regression intercept}
#'  \item{ar_Intercept}{The value of the regression intercept of a autoregressive coefficient associated with the effect}
#'  }
#'  Consult Hirst et.al. 2005 for description of the parameters
#'  
#'  @section other data:
#'  The lists 'AgeLength', 'WeightLength', 'Landings', 'GlobalParameters' and 'CovariateMaps'
#'  may be passed to \code{\link[Reca]{eca.predict}} in functions consuming output from this function. All in all
#'  the following lists can be accessed on RecaParameterData objects:
#'  \describe{
#'  \item{FitProportionAtAge}{list of data tables with parameters for for the Proportion-at-age model}
#'  \item{FitLengthGivenAge}{list of data tables with parameters for for the Length-given-age model}
#'  \item{FitWeightGivenLength}{list of data tables with parameters for for the Weight-given-length model}
#'  \item{AgeLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{WeightLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{Landings}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{GlobalParameters}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. see details}
#'  \item{CovariateMaps}{Mapping of values for each covariate in landings and samples (including non-configurable catchId) to integer value used in R-ECA.}
#' }
#'
#' @name RecaParameterData
#'
NULL

#' Check if argument is RecaParameterData
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaParameterData}}
#' @param RecaParameterData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaParameterData}}
#' @export
is.RecaParameterData <- function(RecaParameterData){
  
  if (!is.list(RecaParameterData)){
    return(FALSE)
  }
  if (!all(c("FitProportionAtAge", "FitLengthGivenAge", "FitWeightGivenLength", "AgeLength", "WeightLength", "Landings", "GlobalParameters", "CovariateMaps") %in% names(RecaParameterData))){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$AgeLength)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$WeightLength)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$Landings)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$GlobalParameters)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$CovariateMaps)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$FitProportionAtAge)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$FitLengthGivenAge)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$FitWeightGivenLength)){
    return(FALSE)
  }
  
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaParameterData$AgeLength))){
    return(FALSE)
  }
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaParameterData$WeightLength))){
    return(FALSE)
  }
  if (!all(c("AgeLengthCov", "WeightLengthCov", "LiveWeightKG") %in% names(RecaParameterData$Landings))){
    return(FALSE)
  }
  if (!all(c("LogLikelihood") %in% names(RecaParameterData$FitProportionAtAge))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaParameterData$FitProportionAtAge$LogLikelihood)){
    return(FALSE)
  }
  if (!all(c("LogLikelihood") %in% names(RecaParameterData$FitLengthGivenAge))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaParameterData$FitLengthGivenAge$LogLikelihood)){
    return(FALSE)
  }
  if (!all(c("LogLikelihood") %in% names(RecaParameterData$FitWeightGivenLength))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaParameterData$FitWeightGivenLength$LogLikelihood)){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Reca Results (RecaCatchAtAge)
#' 
#' @description
#'  Posterior distribution of total catch at age and weight and length parameters.
#'
#' @details
#' a list of data tables:
#' \describe{
#'  \item{CatchAtAge}{\code{\link[data.table]{data.table}} tabulating the estimated catch-at-age by length group for each Reca iteration (MCMC sample)}
#'  \item{MeanLength}{\code{\link[data.table]{data.table}} tabulating the mean length in cm by age for each Reca iteration (MCMC sample)}
#'  \item{MeanWeight}{\code{\link[data.table]{data.table}} tabulating the mean weight in g by age for each Reca iteration (MCMC sample)}
#'  \item{AggregationVariables}{\code{\link[data.table]{data.table}} with any variables that catch-at-age estimates are partitioned on in the column 'AggregationVariables'.}
#' }
#' In addition to columns for the variables in 'AggregationVariables', column names in the data tables should be interpreted as:
#' \describe{
#'  \item{Length}{Upper limit of length group in cm}
#'  \item{Age}{Age in number of years}
#'  \item{Iteration}{The Reca iteration (MCMC sample) that estimates are calculated for}
#'  \item{CatchAtAge}{The total catch at age in numbers}
#'  \item{MeanIndividualLength}{Mean Length at age in cm}
#'  \item{MeanIndividualWeight}{Mean weight at age in g}
#' }
#' 
#'
#' @name RecaCatchAtAge
#'
NULL

#' Check if argument is RecaCatchAtAge
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaCatchAtAge}}
#' @param RecaCatchAtAge argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaCatchAtAge}}
#' @export
is.RecaCatchAtAge <- function(RecaCatchAtAge){
  if (!is.list(RecaCatchAtAge)){
    return(FALSE)
  }
  if (!all(c("CatchAtAge", "MeanLength", "MeanWeight") %in% names(RecaCatchAtAge))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaCatchAtAge$CatchAtAge)){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaCatchAtAge$MeanLength)){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaCatchAtAge$MeanWeight)){
    return(FALSE)
  }
  if (!all(c("Length", "Age", "Iteration", "CatchAtAge") %in% names(RecaCatchAtAge$CatchAtAge))){
    return(FALSE)
  }
  if (!all(c("MeanIndividualLength", "Age", "Iteration") %in% names(RecaCatchAtAge$MeanLength))){
    return(FALSE)
  }
  if (!all(c("MeanIndividualWeight", "Age", "Iteration") %in% names(RecaCatchAtAge$MeanWeight))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Reca Results (RecaResult)
#'
#' Results from running
#' \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' via \code{\link[RstoxFDA]{RunRecaEstimate}}.
#'
#' @details
#'
#' \describe{
#'  \item{input}{All input data and parameters provided to \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{fit}{as returned by \code{\link[Reca]{eca.estimate}}}
#'  \item{prediction}{as returned by \code{\link[Reca]{eca.predict}}}
#'  \item{covariateMaps}{list() mapping from Reca covariate encoding to values fed to \code{\link[RstoxFDA]{PrepareRecaEstimate}}. As in \code{\link[RstoxFDA]{RecaData}}}
#' }
#'
#' @name RecaResult
#'
NULL

#' @noRd
is.RecaPrediction <- function(prediction){
  if (!is.list(prediction)){
    return(FALSE)
  }
  if (!all(c("TotalCount", "MeanLength", "MeanWeight", "AgeCategories", "LengthIntervalsLog") %in% names(prediction))){
    return(FALSE)
  }
  if (!is.array(prediction$TotalCount)){
    return(FALSE)
  }
  if (!is.array(prediction$MeanLength)){
    return(FALSE)
  }
  if (!is.array(prediction$MeanWeight)){
    return(FALSE)
  }
  return(TRUE)
}

#' Check if argument is RecaResult
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaResult}}
#' @param RecaResult argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaResult}}
#' @export
is.RecaResult <- function(RecaResult){
  if (!is.list(RecaResult)){
    return(FALSE)
  }
  if (!all(c("input", "fit", "prediction", "covariateMaps") %in% names(RecaResult))){
    return(FALSE)
  }
  if (!is.RecaData(RecaResult$input)){
    return(FALSE)
  }
  if (!is.RecaPrediction(RecaResult$prediction)){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Unified Variable Definition (UnifiedVariableDefinition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a unified variable for different data formats
#'
#' @details
#'  \describe{
#'   \item{UnifiedVariable}{Unified code}
#'   \item{Source}{Format for which the unified variable has corresponding codes}
#'   \item{Definition}{The codes defining the unified code in the 'source'. Comma-separated list of codes.}
#'  }
#'
#' @name UnifiedVariableDefinition
#'
NULL

#' Check if argument is UnifiedVariableDefinition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{UnifiedVariableDefinition}}
#' @param UnifiedVariableDefinition argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{UnifiedVariableDefinition}}
#' @export
is.UnifiedVariableDefinition <- function(UnifiedVariableDefinition){
  if (!data.table::is.data.table(UnifiedVariableDefinition)){
    return(FALSE)
  }
  if (!all(c("UnifiedVariable", "Source", "Definition") %in% names(UnifiedVariableDefinition))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Temporal Categories (TemporalDefinition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a categorical variable for grouping data based on date.
#'
#' @details
#'  \describe{
#'   \item{TemporalCategory}{character() Value of the temporal category}
#'   \item{StartDay}{integer() Day of month for first day in the temporal category (1-based)}
#'   \item{StartMonth}{integer() Month for first day in the temporal category (1-based)}
#'   \item{StartYear}{integer() Year for which the category is defined, NA for seasonal definitions.}
#'  }
#'
#'  Start and end of year is not implied as category delimitations when not included.
#'  If 1st of January is not definied as the start of a category,
#'  it is taken to be included in the last category of the preceding year.
#'
#' @name TemporalDefinition
#'
NULL

#' Check if argument is TemporalDefinition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{TemporalDefinition}}
#' @param TemporalDefinition argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{TemporalDefinition}}
#' @export
is.TemporalDefinition <- function(TemporalDefinition){
  if (!data.table::is.data.table(TemporalDefinition)){
    return(FALSE)
  }
  if (!all(c("Period", "StartDay", "StartMonth", "StartYear") %in% names(TemporalDefinition))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Area Code Positions (AreaPosition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a position for area codes.
#'
#' @details
#'  \describe{
#'   \item{Area}{Area code. (key)}
#'   \item{Location}{optional subdivision of 'Area'}
#'   \item{Latitude}{WGS84 Latitude, decimal degrees}
#'   \item{Longitude}{WGS84 Longitude, decimal degrees}
#'  }
#'  If location is provided, the case for missing location is also encoded.
#'
#' @name AreaPosition
#'
NULL

#' Check if argument is AreaPosition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{AreaPosition}}
#' @param AreaPosition argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{AreaPosition}}
#' @export
is.AreaPosition <- function(AreaPosition){
  if (!data.table::is.data.table(AreaPosition)){
    return(FALSE)
  }
  if (!all(c("Area", "Location", "Latitude", "Longitude") %in% names(AreaPosition))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Area Neighbour Definition (CarNeighbours)
#'
#' Table (\code{\link[data.table]{data.table}})
#' defining neighbours for a CAR-variable (Conditional autoregressive variable).
#'
#' @details
#'  \describe{
#'   \item{CarVariable}{Values for a variable used as CAR-variable}
#'   \item{Neighbours}{Comma-separated list of neighbours}
#'  }
#'
#'  The table is symmetric, so that if b is a neighbour of a. a is also a neighbour of b.
#'
#' @name CarNeighbours
#'
NULL

#' Check if argument is CarNeighbours
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{CarNeighbours}}
#' @param CarNeighbours argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{CarNeighbours}}
#' @export
is.CarNeighbours <- function(CarNeighbours){
  if (!data.table::is.data.table(CarNeighbours)){
    return(FALSE)
  }
  if (!all(c("CarVariable", "Neighbours") %in% names(CarNeighbours))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Age Error Matrix (AgeErrorMatrix)
#'
#' Table (\code{\link[data.table]{data.table}})
#' defining probabilities of misreading age.
#'
#' @details
#'  \describe{
#'   \item{columns 1..n}{numeric() [0,1]. Probability of reading read age, given that true age is as column name.}
#'   \item{ReadAge}{The read age.}
#'  }
#'
#'  Columns sum to 1.
#'
#' @name AgeErrorMatrix
#'
NULL

#' Check if argument is AgeErrorMatrix
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{AgeErrorMatrix}}
#' @param AgeErrorMatrix argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{AgeErrorMatrix}}
#' @export
is.AgeErrorMatrix <- function(AgeErrorMatrix){
  if (!data.table::is.data.table(AgeErrorMatrix)){
    return(FALSE)
  }
  if (!("ReadAge" %in% names(AgeErrorMatrix))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Stock splitting parameters (StockSplittingParamteres)
#'
#' @description
#'  Table (\code{\link[data.table]{data.table}})
#'  
#'  Defining parameters for the stock-splitting analysis in Reca, including
#'  parameters for the probability of misclassifying when determining stock membership of a specimen.
#'
#'  The stock splitting analysis allows catch at age to be estimated for two domains that partition all individuals,
#'  and that are observed for all age-determined specimens. It was developed for disciminating Coastal Cod and North East Arctic Cod,
#'  based on otholith growth patterns, and naming conventions are derived from that. It could be adapted to
#'  other stocks and in principle to other bipartite domain definitions (such as Sex).
#'  
#'  Two otolith patterns are defined for each of the two stocks 'CC' and 'S'. Otolith type 1 and 2 identifies
#'  that a specimen belongs to the stock 'CC', and are interpreted by otoloith readers as 'certain' and 'uncertain' CC, respectively.
#'  Otolith type 4 and 5 identifies that a specimen belongs to the stock 'S', and are interpreted as 'uncertain' and 'certain' S, respectively.
#'  
#'  \describe{
#'   \item{StockNameCC}{Name of the stock identified as CC}
#'   \item{StockNameS}{Name of the stock identified as S}
#'   \item{ProbabilityType1As1}{Probability of classifying a type 1 specimen as type 1 (certain CC).}
#'   \item{ProbabilityType5As1}{Probability of classifying a type 5 (certain S) specimen as type 1 (certain CC).}
#'   \item{ProbabilityType2As2}{Probability of classifying a type 2 (uncertain CC) specimen as type 2 (uncertain CC).}
#'   \item{ProbabilityType4As2}{Probability of classifying a type 4 (uncertain S) specimen as type 2 (uncertain CC).}
#'   \item{ProbabilityType2As4}{Probability of classifying a type 2 (uncertain CC) specimen as type 4 (uncertain S).}
#'   \item{ProbabilityType4As4}{Probability of classifying a type 4 (uncertain S) specimen as type 4 (uncertain S).}
#'   \item{ProbabilityType1As5}{Probability of classifying a type 1 (certain CC) specimen as type 5 (certain S).}
#'   \item{ProbabilityType5As5}{Probability of classifying a type 5 (certain S) specimen as type 5 (certain S).}
#'  }
#'
#'  The probabilities for different ways to classify a type must sum to 1.
#'  E.g.: ProbabilityType1As1 + ProbabilityType1As5 = 1.
#'
#'  The data table contains only one row.
#'
#' @name StockSplittingParamteres
#'
NULL

#' Check if argument is StockSplittingParamteres
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @param StockSplittingParamteres argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{StockSplittingParamteres}}
#' @export
is.StockSplittingParamteres <- function(StockSplittingParamteres){
  if (!data.table::is.data.table(StockSplittingParamteres)){
    return(FALSE)
  }
  if (!all(c("StockNameCC", "StockNameS", "ProbabilityType1As1",
             "ProbabilityType1As5", "ProbabilityType2As2",
             "ProbabilityType2As4",	"ProbabilityType4As2",
             "ProbabilityType4As4",	"ProbabilityType5As1",
             "ProbabilityType5As5") %in% names(StockSplittingParamteres))){
    return(FALSE)
  }
  if (nrow(StockSplittingParamteres) != 1){
    return(FALSE)
  }
  
  prob <- function(arg){
    if (arg<0 | arg > 1){
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (!prob(StockSplittingParamteres$ProbabilityType1As1)){
    return(FALSE)
  }
  if (!prob(StockSplittingParamteres$ProbabilityType1As5)){
    return(FALSE)
  }
  if (!prob(StockSplittingParamteres$ProbabilityType2As2)){
    return(FALSE)
  }
  if (!prob(StockSplittingParamteres$ProbabilityType2As4)){
    return(FALSE)
  }
  if (!prob(StockSplittingParamteres$ProbabilityType4As2)){
    return(FALSE)
  }
  if (!prob(StockSplittingParamteres$ProbabilityType4As4)){
    return(FALSE)
  }
  if (!prob(StockSplittingParamteres$ProbabilityType5As1)){
    return(FALSE)
  }
  if (!prob(StockSplittingParamteres$ProbabilityType5As5)){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Function specification for inclusion in StoX UI
#' @export
stoxFunctionAttributes <- list(
  
  DefineStockSplittingParamteres = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StockSplittingParamteres",
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      ),
      StockNameCC=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      StockNameS=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType1As1=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType5As1=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType2As2=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType4As2=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType2As4=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType4As4=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType1As5=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType5As5=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineCarNeighbours = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "CarNeighbours",
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      StratumPolygon = list(
        DefinitionMethod = "StratumPolygon", 
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineAgeErrorMatrix = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "AgeErrorMatrix",
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineAreaPosition = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "AreaPosition", 
    functionParameterFormat = list(
      FileName = "filePath"
    ), 
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      StratumPolygon = list(
        DefinitionMethod = "StratumPolygon", 
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefinePeriod = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "TemporalDefinition", 
    functionParameterFormat = list(
      CustomPeriods = "periodvector"
    ), 
    functionArgumentHierarchy = list(
      TemporalCategory = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      CustomPeriods = list(
        TemporalCategory = "Custom", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineWeightConversionFactor = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "WeightConversionTable", 
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      )
    )
  ),
  
  ConvertWeightsBiotic =list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData" 
  ),
  
  SetTimeBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  
  SetStartDateBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  
  SetAreaPositionsBiotic =list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  
  AddAreaPositionStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddGearGroupStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddGearGroupStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  
  AddStratumStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddStratumStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  
  AddPeriodStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddPeriodStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  
  PrepareRecaEstimate = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaData",
    functionParameterFormat = list(
      RandomEffects = "randomcovariates",
      FixedEffects = "fixedcovariates"),
    functionArgumentHierarchy = list(
      AgeErrorMatrix = list(
        UseAgingError = TRUE
      ),
      CarNeighbours = list(
        UseCarEffect = TRUE
      ),
      CarEffect = list(
        UseCarEffect = TRUE
      )
    )
  ),
  ParameterizeRecaModels = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaParameterData"
    #doesnt work for directory
    #functionParameterFormat = list(
    #  ResultDirectory = "filePath"
    #)
  ),
  RunRecaModels = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaCatchAtAge",
    functionParameterFormat = list(
      AggregationVariables = "aggregationvariables"
    )
  ),
  ReportFdaSampling = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaSamplingData",
    functionParameterFormat = list(
      AggregationVariables = "samplereportvariables"
    )
  )
)

#' Define the process property formats for inclusion in stox UI
#' 
#' @export
#' 
processPropertyFormats <- list(
  filePath = list(
    class = "single", 
    title = "The path to a single file"
  ),
  periodvector = list(
    class = "vector", 
    title = "Period defintinions. Start date on the form \"DD-MM\" or \"DD-MM-YYYY\"", 
    variableTypes = "character"
  ),
  
  randomcovariates = list(
    class = "vector", 
    title = "One or more variables to use as covariates in Reca", 
    possibleValues = function(StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[!(possibleValues %in% c("CruiseKey", "StationKey", "HaulKey", "SpeciesCategoryKey", "SampleKey"))]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  aggregationvariables = list(
    class = "vector", 
    title = "One or more variables to use as aggregation variables.", 
    possibleValues = function(StoxLandingData) {
      possibleValues <- names(StoxLandingData$Landing)[!(names(StoxLandingData$Landing) %in% c("RoundWeight"))]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  samplereportvariables = list(
    class = "vector", 
    title = "One or more variables to use as aggregation variables.", 
    possibleValues = function(StoxLandingData, StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[possibleValues %in% names(StoxLandingData$Landing)]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  fixedcovariates = list(
    class = "vector", 
    title = "One or more variables to use as covariates in Reca", 
    possibleValues = function(StoxLandingData, StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[possibleValues %in% names(StoxLandingData$Landing)]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  carcovariate = list(
    class = "single",
    possibleValues = function(StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[!(possibleValues %in% c("CruiseKey", "StationKey", "HaulKey", "SpeciesCategoryKey", "SampleKey"))]
      return(sort(possibleValues))
    }
  )
)

