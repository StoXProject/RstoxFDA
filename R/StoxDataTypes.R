#' @noRd
is.POSIXct <- function(date){
  if ("POSIXct" %in% class(date)){
    return(TRUE)
  }

  return(FALSE)
}

#' @noRd
is.Date <- function(date){
  if ("Date" %in% class(date)){
    return(TRUE)
  }

  return(FALSE)
}


#' Age group statistics (ReportFdaByAgeData)
#' 
#' @description 
#'  Results from catch at age estimations. The results may be presented
#'  decomposed on combinations of aggregation variables, such as gear, area, stock etc.
#'  
#'  list with two members 'FdaReport' and 'GroupingVariables'.
#'  'FdaReport' is a \code{\link[data.table]{data.table}} with the columns:
#'  \describe{
#'   \item{AgeGroup}{character. The age group the estimate is reported for. May be age or plus group}
#'   \item{Age}{integer. The lower age the estimate is reported for. May be an age or lower limit of plus group (inclusive)}
#'   \item{<Statistic>}{A reported statistic}
#'   \item{SD}{Standard deviation for the reported statistic.}
#'   \item{Low}{The lower limit of the estimated interval for the reported statistic.}
#'   \item{High}{The higher limit of the estimated interval for the reported statistic.}
#'   \item{...}{Any aggregation variables. The names of these are listed in 'GroupingVariables'}
#'  }
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#' @name ReportFdaByAgeData
#' 
NULL


#' Total catch statistics (ReportFdaSummaryData)
#' @description 
#'  Results from catch estimations. The results may be presented
#'  decomposed on combinations of aggregation variables, such as gear, area, stock etc.
#'  
#'  list with six members 'MeanAge', 'MeanWeight', 'MeanLength', 'TotalWeight',
#'  'TotalNumber', and 'GroupingVariables'.
#'  'MeanAge', 'MeanWeight', 'MeanLength', 'TotalWeight',
#'  'TotalNumber' are \code{\link[data.table]{data.table}}s with the columns:
#'  \describe{
#'   \item{<Statistic>}{The reported statistic, either 'MeanIndividualAge', 'MeanIndividualWeight', 'MeanIndividualLength', 'TotalWeight', or 'TotalNumber'}
#'   \item{SD}{Standard deviation for the reported statistic.}
#'   \item{Low}{The lower limit of the estimated interval for the reported statistic.}
#'   \item{High}{The higher limit of the estimated interval for the reported statistic.}
#'   \item{...}{Any aggregation variables. The names of these are listed in 'GroupingVariables'}
#'  }
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#' @name ReportFdaSummaryData
NULL

#' Fisheries dependent Catch At Age Report (ReportFdaCatchAtAgeData)
#' 
#' @description 
#'  A \code{\link[RstoxFDA]{ReportFdaByAgeData}} object with the reported <Statistic> being:
#'  
#'  \describe{
#'   \item{CacthAtAge}{The total catch at age in numbers.}
#'  }
#' 
#' @name ReportFdaCatchAtAgeData
#' 
NULL

#' Fisheries dependent Length At Age Report (ReportFdaLengthAtAgeData)
#' 
#' @description 
#'  Results from Reca catch at age estimations. A \code{\link[RstoxFDA]{ReportFdaByAgeData}} object
#'  with the reported <Statistic> being:
#'  
#'  \describe{
#'   \item{MeanIndividualLength}{The mean length at age in cm.}
#'  }
#'  
#'  Note that the summary statistics are reported for summaries of mean lengths, 
#'  so that e.g. SD report the standard deviation of the means,
#'  and does not characterize the length distribution of fish.
#' 
#' @name ReportFdaLengthAtAgeData
#' 
NULL

#' Reca Weight At Age Report (ReportFdaWeightAtAgeData)
#' 
#' @description 
#'  Results from Reca catch at age estimations. A \code{\link[RstoxFDA]{ReportFdaByAgeData}} object
#'  with the reported <Statistic> being:
#'  
#'  \describe{
#'   \item{MeanIndividualWeight}{The mean weight at age in kg}
#'  }
#'  
#'  Note that the summary statistics are reported for summaries
#'  of mean weights, so that e.g. SD report the standard deviation of the means,
#'  and does not characterize the weight distribution of fish.
#' 
#' @name ReportFdaWeightAtAgeData
#' 
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ReportFdaByAgeData}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaByAgeData}}
#' @param ReportFdaByAgeData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaByAgeData}}
#' @export
is.ReportFdaByAgeData <- function(ReportFdaByAgeData){
  
  if (!is.list(ReportFdaByAgeData)){
    return(FALSE)
  }
  if (!all(c("GroupingVariables", "FdaReport") %in% names(ReportFdaByAgeData))){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaByAgeData$FdaReport)){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaByAgeData$GroupingVariables)){
    return(FALSE)
  }
  if (!all(c("Age", "Low", "High", "SD") %in% names(ReportFdaByAgeData$FdaReport))){
    return(FALSE)
  }
  if (!all(c("GroupingVariables") %in% names(ReportFdaByAgeData$GroupingVariables))){
    return(FALSE)
  }
  return(TRUE)
}

#' Sum of Products report (ReportFdaSopData)
#' 
#' @description 
#'  Sum of Products report (SOP-report), comparing the total landed weight of fish
#'  with the product of mean weight at age estimates and total number 
#'  at age estimates 
#'  
#'  list with two members 'SopReport' and 'GroupingVariables'.
#'  'SopReport' is a \code{\link[data.table]{data.table}} with the columns:
#'  \describe{
#'   \item{TotalWeightEstimated}{Total round weight (kg) estimated}
#'   \item{LandedWeight}{Landed round weight (kg) reported}
#'   \item{Difference}{The difference between estimated and reported landed weight}
#'   \item{RelativeDifference}{The difference between estimated and reported landed weight relative to reported weight}
#'   \item{...}{Any aggregation variables. The names of these are listed in 'GroupingVariables'}
#'  }
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#' @name ReportFdaSopData
#' 
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ReportFdaSOP}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSOP}}
#' @param ReportFdaSOP argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSOP}}
#' @export
is.ReportFdaSOP <- function(ReportFdaSOP){
  
  if (!is.list(ReportFdaSOP)){
    return(FALSE)
  }
  if (!all(c("GroupingVariables", "SopReport") %in% names(ReportFdaSOP))){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaSOP$SopReport)){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaSOP$GroupingVariables)){
    return(FALSE)
  }
  if (!all(c("TotalWeightEstimated", "LandedWeight", "Difference", "RelativeDifference") %in% names(ReportFdaSOP$SopReport))){
    return(FALSE)
  }
  if (!all(c("GroupingVariables") %in% names(ReportFdaSOP$GroupingVariables))){
    return(FALSE)
  }
  return(TRUE)
  
}


#' Summary statistics for simulated parameters (ParameterizationSummaryData)
#' 
#' @description 
#'  Summary statistics for (potentially multi-chained) simulated parameters, 
#'  such as MCMC simulations with Reca.
#'  'chains' in this respect refers to statistically independent simulations.
#'  
#'  list with two members 'ParameterSummary' and 'RunParameters',
#'  which both all \code{\link[data.table]{data.table}}s. 'ParameterSummary'
#'  contains parameter statistics for simulations and have the following columns:
#'  \describe{
#'   \item{Parameter}{Identifies the parameter that is summarized.}
#'   \item{Mean}{Mean of the parameter}
#'   \item{Variance}{Variance of the parameter}
#'   \item{chainId}{Identifies the parameterization chain.}
#'  }
#'  
#'  RunParameters summarizes global control parameters for each chain and has columns:
#'  \describe{
#'   \item{chainId}{Identifies the parameterization chain.}
#'   \item{Iterations}{The number of iterations for parameter simulation}
#'  }
#'  
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @seealso \code{\link[RstoxFDA]{ReportRecaParameterStatistics}} 
#'  for creating ParameterizationSummaryData from Reca-simulations.
#' 
#' @name ParameterizationSummaryData
#' 
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ParameterizationSummaryData}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ParameterizationSummaryData}}
#' @param ParameterizationSummaryData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ParameterizationSummaryData}}
#' @export
is.ParameterizationSummaryData <- function(ParameterizationSummaryData){
  
  if (!is.list(ParameterizationSummaryData)){
    return(FALSE)
  }
  
  if (!all(c("ParameterSummary", "RunParameters") %in% names(ParameterizationSummaryData))){
    return(FALSE)
  }
  
  if (!data.table::is.data.table(ParameterizationSummaryData$ParameterSummary)){
    return(FALSE)
  }
  
  if (!data.table::is.data.table(ParameterizationSummaryData$RunParameters)){
    return(FALSE)
  }
  
  if (!all(c("chainId","Iterations") %in% names(ParameterizationSummaryData$RunParameters))){
    return(FALSE)
  }
  
  if (!all(c("Parameter", "Mean", "Variance", "chainId") %in% names(ParameterizationSummaryData$ParameterSummary))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Convergence Report for simulated parameters (ParameterConvergenceData)
#' 
#' @description 
#'  Convergence Report for multi-chained simulated parameters, 
#'  such as MCMC simulations with Reca.
#'  'chains' in this respect refers to statistically independent simulations.
#'  
#'  list with one members 'ConvergenceReport',
#'  which is a \code{\link[data.table]{data.table}} containing the following columns:
#'  \describe{
#'   \item{Parameter}{Identifies the parameter that is summarized.}
#'   \item{InterVariance}{The Mean-Squared-Deviation the means of the parameter in each chain, to the mean across all chains}
#'   \item{IntraVariance}{The mean of the within-chain variances of the parameter}
#'   \item{GelmanRubinR}{Gelman-Rubins R}
#'  }
#'  
#' @details 
#'  Gelman-Rubins R is described by Gelman and Rubin (Statistical Science, 1992):
#'  DOI: https://doi.org/10.1214/ss/1177011136
#'  
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @seealso \code{\link[RstoxFDA]{ReportParameterConvergence}} 
#'  for creating ParameterConvergenceData.
#' 
#' @name ParameterConvergenceData
#' 
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @param ParameterConvergenceData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @export
is.ParameterConvergenceData <- function(ParameterConvergenceData){
  
  if (!is.list(ParameterConvergenceData)){
    return(FALSE)
  }
  if (!all(c("ConvergenceReport") %in% names(ParameterConvergenceData))){
    return(FALSE)
  }
  cnames <- c("Parameter", "InterVariance", "IntraVariance", "GelmanRubinR")
  if (!all(cnames %in% names(ParameterConvergenceData$ConvergenceReport))){
    return(FALSE)
  }
  if (!data.table::is.data.table(ParameterConvergenceData$ConvergenceReport)){
    return(FALSE)
  }

  return(TRUE)
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

#' Length Conversion Table (LengthConversionTable)
#' 
#' @description
#'  Length conversion parameters realting different length measurements.
#' @details 
#'  Length conversion factors relating different length measurements, such as 'standard length' and 'fork length'
#'  based on a linear regression fit between length measurements:
#'  L1 = alpha + beta \* L2,
#'  where L1 and L2 are different length measurements
#'  and 'alpha' and 'beta' are species-specific coefficients.
#'  
#'  \code{\link[data.table]{data.table}} with columns:
#'  \describe{
#'  \item{'Description'}{character: Free-text description of the product}
#'  \item{'Species'}{character: Identifier for the species that the conversion applies to}
#'  \item{'MeasurmentType'}{character: Identifier for the type of length measurement for the independent variable (L2 above)}
#'  \item{'Alpha'}{numeric: scalar value representing the intercept (in cm) of a linear regression fit between length measurements.}
#'  \item{'Beta'}{numeric: scalar value representing the slope of a linear regression fit between length measurements.}
#'  }
#'  
#' @name LengthConversionTable
#' 
NULL

#' Check if argument is LengthConversionTable
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{LengthConversionTable}}
#' @param LengthConversionTable argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{LengthConversionTable}}
#' @export
is.LengthConversionTable <- function(LengthConversionTable){
  if (!data.table::is.data.table(LengthConversionTable)){
    return(FALSE)
  }
  if (!all(c("Description", "Species", "MeasurementType", "Alpha", "Beta") %in% names(LengthConversionTable))){
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
#'   \item{GroupingVariables}{a \code{\link[data.table]{data.table}} with the variables used for aggregation in 'FishereisSampling' stored in the column 'GroupingVariables'}
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
  if (!all(c("GroupingVariables", "FisheriesSampling") %in% names(ReportFdaSamplingData))){
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
  if (!all(c("AgeLength", "WeightLength", "Landings", "GlobalParameters") %in% names(RecaData))){
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
#'  'FitLengthGivenAge' and 'FitWeightGivenLength' is of interest. For stock-splitting analysis,
#'  the lists FitLengthGivenAgeCC and FitWeightGivenLengthCC will be added as well, corresponding to one of the stocks.
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
  if (!all(c("FitProportionAtAge", "FitLengthGivenAge", "FitWeightGivenLength", "AgeLength", "WeightLength", "Landings", "GlobalParameters") %in% names(RecaParameterData))){
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
#'  \item{GroupingVariables}{\code{\link[data.table]{data.table}} with any variables that catch-at-age estimates are partitioned on 
#'            in the column 'GroupingVariables'. These may correspond to variables in the landings, or maye be the variable 'Stock' if
#'            stock-splitting analysis have been perfomred.}
#' }
#' In addition to columns for the variables in 'GroupingVariables', the data tables 'CatchAtAge', 'MeanLength', and 'MeanWeight' have the following variables:
#' \describe{
#'  \item{Age}{Age in number of years.}
#'  \item{Iteration}{The Reca iteration (MCMC sample) that estimates are calculated for}
#' }
#' 
#' 'CatchAtAge' also have the variables
#' \describe{
#'  \item{Length}{Upper limit of length group in cm}
#'  \item{CatchAtAge}{The total catch at age in numbers}
#' }
#' 
#' 'MeanLength' has the variable:
#' \describe{
#'  \item{MeanIndividualLength}{Mean Length at age in cm}
#' }
#' 
#' 'MeanWeight' has the variable:
#' \describe{
#'  \item{MeanIndividualWeight}{Mean weight at age in g}
#' }
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
#'   \item{CarValue}{Values for a variable used as CAR-variable}
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
  if (!all(c("CarValue", "Neighbours") %in% names(CarNeighbours))){
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

#' Stock splitting parameters (StockSplittingParameters)
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
#' @name StockSplittingParameters
#'
NULL

#' Check if argument is StockSplittingParameters
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param StockSplittingParameters argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @export
is.StockSplittingParameters <- function(StockSplittingParameters){
  if (!data.table::is.data.table(StockSplittingParameters)){
    return(FALSE)
  }
  if (!all(c("StockNameCC", "StockNameS", "ProbabilityType1As1",
             "ProbabilityType1As5", "ProbabilityType2As2",
             "ProbabilityType2As4",	"ProbabilityType4As2",
             "ProbabilityType4As4",	"ProbabilityType5As1",
             "ProbabilityType5As5") %in% names(StockSplittingParameters))){
    return(FALSE)
  }
  if (nrow(StockSplittingParameters) != 1){
    return(FALSE)
  }
  
  prob <- function(arg){
    if (arg<0 | arg > 1){
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (!prob(StockSplittingParameters$ProbabilityType1As1)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType1As5)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType2As2)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType2As4)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType4As2)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType4As4)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType5As1)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType5As5)){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Function specification for inclusion in StoX UI
#' @export
stoxFunctionAttributes <- list(
  
  DefineStockSplittingParameters = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StockSplittingParameters",
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
  
  DefineLengthConversionParameters = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "LengthConversionTable", 
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
  
  ConvertWeightBiotic =list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData" 
  ),
  
  ConvertLengthBiotic =list(
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
      CarEffect = "randomcovariates",
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
      ),
      StockSplittingParameters = list(
        UseStockSplitting = TRUE
      ),
      UseStockSplittingError = list(
        UseStockSplitting = TRUE
      )
    )
  ),
  ParameterizeRecaModels = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaParameterData"
    #doesnt work for directory ?
    #functionParameterFormat = list(
    #  ResultDirectory = "filePath"
    #)
  ),
  RunRecaModels = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaCatchAtAge",
    functionParameterFormat = list(
      GroupingVariables = "GroupingVariables"
    )
  ),
  ReportFdaSampling = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaSamplingData",
    functionParameterFormat = list(
      GroupingVariables = "samplereportvariables"
    )
  ),
  ReportRecaCatchAtAge = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaCatchAtAgeData"
  ),
  ReportRecaLengthAtAge = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaLengthAtAgeData"
  ),
  ReportRecaWeightAtAge = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaWeightAtAgeData"
  ),
  ReportRecaCatchStatistics = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaCatchSummaryData",
    functionArgumentHierarchy = list(
      DecimalTotalNumber = list(
        DecimalOptions = TRUE
      ),
      DecimalTotalWeight = list(
        DecimalOptions = TRUE
      ),
      DecimalMeanAge = list(
        DecimalOptions = TRUE
      ),
      DecimalMeanWeight = list(
        DecimalOptions = TRUE
      ),
      DecimalMeanLength = list(
        DecimalOptions = TRUE
      )
    )
  ),
  ReportFdaSOP = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaSopData",
    functionParameterFormat = list(
      GroupingVariables = "GroupingVariablesSop"
    )
  ),
  ReportRecaParameterStatistics = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ParameterizationSummaryData",
    functionArgumentHierarchy = list(
      ParameterizationSummaryData = list(
        AppendReport = TRUE
      )
    )
  ),
  ReportParameterConvergence = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ParameterConvergenceData"
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
  GroupingVariablesSop = list(
    class = "vector", 
    title = "One or more variables to use as aggregation variables.", 
    possibleValues = function(ReportFdaCatchAtAgeData, ReportFdaWeightAtAgeData, StoxLandingData) {
      possibleValues <- names(StoxLandingData$Landing)[(names(StoxLandingData$Landing) %in% ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables) &
                                                         (names(StoxLandingData$Landing) %in% ReportFdaWeightAtAgeData$GroupingVariables$GroupingVariables)]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  GroupingVariables = list(
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

