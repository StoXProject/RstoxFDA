#' checks that intervalwidth is in the range <0,1>
#' @noRd
check_intervalWidth <- function(intervalwidth){
  if (intervalwidth <= 0 || intervalwidth >= 1){
    stop("'IntervalWidth' must be larger than 0 and smaller than 1.")
  }
}

#' Report FDA sampling
#' @description 
#'  Report sampling of fisheries against landings in partitions of the fisheries.
#' @details 
#'  Sampling is reported partitioned on the provided 'GroupingVariables', which must be present in both samples (StoxBioticData) and landings (StoxLandingData).
#'  If samples are encoded in partitions of the fisheries with no landings. 'LandedRoundWeight' will be NA.
#'  This may be due to recording errors or filtering errors, but it may also be due to comparison of similar
#'  but unequal category-definitions. For instance area are coded in landings as dominant area for a fishing trip,
#'  while at-sea sampling will record area of fishing operation, and the catch from that area by subsequently be landed
#'  with another area listed as dominant area.
#'  
#'  Note that the columns Catches and Vessels summarize the number of unique catches and vessels in each partition / cell.
#'  So depending on which grouping variables are added, the sum of vessels does not have to correspond to the total sum of vessels in the fleet.
#'  In principle the same applies to Catches, but in practice that situation does less commonly arise.
#'  
#'  In addition sampling may be reported partitioned on the provided 'SamplingVariables'. For instance the variable 'IndividualSex' may be provided
#'  to see how many samples are collected for each sex. NAs will be treated as a separate category.
#'  When 'SamplingVariables' are provided, each partition specified by 'GroupingVariables' will be reported several times,
#'  so that the sum of the column 'LandedRoudnWeight' will not be the total landings.
#'  'SamplingVariables' must be columns that are only present in samples (StoxBioticData), not in landings (StoxLandingData).
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'
#'  The units considered valid for weights are those listed for quantity 'mass' in \code{\link[RstoxData]{StoxUnits}}
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries
#'  and approriate columns added for identifying corresponding landings.
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns added for identifying corresponding samples
#' @param GroupingVariables Columns of 'StoxBioticData' and 'StoxLandingData' that partitions the fisheries. If not provided, a single row for all landings will be produced.
#' @param Decimals integer specifying the number of decimals to report for 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportFdaSampling$functionParameterDefaults$Decimals`.
#' @param Unit unit for the weights 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportFdaSampling$functionParameterDefaults$Unit`
#' @param SamplingVariables Columns of 'StoxBioticData' identifying sampling variables to be use to partition the report. See details.
#' @return \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @concept landings functions
#' @concept StoX-functions
#' @examples 
#'   samplingreport <- RstoxFDA::ReportFdaSampling(RstoxFDA::StoxBioticDataExample, 
#'       RstoxFDA::StoxLandingDataExample, GroupingVariables = c("GearGroup"), Unit = "ton")
#'   samplingreport$FisheriesSampling
#' @export
#' @md
ReportFdaSampling <- function(StoxBioticData, StoxLandingData, GroupingVariables=character(), Decimals=integer(), Unit=RstoxData::getUnitOptions("mass", conversionRange=c(1,1e12)), SamplingVariables=character()){
  
  checkMandatory(StoxBioticData, "StoxBioticData")
  checkMandatory(StoxLandingData, "StoxLandingData")
  Decimals <- getDefault(Decimals, "Decimals", F, RstoxFDA::stoxFunctionAttributes$ReportFdaSampling$functionParameterDefaults$Decimals)

  Unit <- getDefault(Unit, "Unit", F, RstoxFDA::stoxFunctionAttributes$ReportFdaSampling$functionParameterDefaults$Unit)
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("mass", conversionRange=c(1,1e12)))
  
  flatlandings <- StoxLandingData$Landing
  flatbiotic <- RstoxData::MergeStoxBiotic(StoxBioticData)
  
  # flattening may introduce hard to trace NAs if any higher levels lack children.
  
  vars <- c(GroupingVariables, SamplingVariables)
  if (any(is.na(flatbiotic$StationKey))){
    stationVars <- vars[!(vars %in% names(StoxBioticData$Cruise))]
    if (length(stationVars)>0){
      stoxWarning(paste("There are some missions with no stations This may introduce NAs in ", paste(stationVars, collapse=","), ". Consider filtering with argument 'FilterUpwards'", sep="")) 
    }
  }
  if (any(!is.na(flatbiotic$StationKey) & is.na(flatbiotic$HaulKey))){
    haulVars <- vars[!(vars %in% c(names(StoxBioticData$Station), names(StoxBioticData$Cruise)))]
    if (length(haulVars)>0){
      stoxWarning(paste("There are some stations with no hauls. This may introduce NAs in ", paste(haulVars, collapse=","), ". Consider filtering with argument 'FilterUpwards'", sep=""))  
    }
  }
  if (any(!is.na(flatbiotic$HaulKey) & is.na(flatbiotic$SpeciesCategoryKey))){
    scVars <- vars[!(vars %in% c(names(StoxBioticData$Station), names(StoxBioticData$Cruise), names(StoxBioticData$Haul)))]
    if (length(scVars)>0){
      stoxWarning(paste("There are some hauls with no SpeciesCategory This may introduce NAs in ", paste(scVars, collapse=","), ". Consider filtering with argument 'FilterUpwards'", sep="")) 
    }
  }
  if (any(!is.na(flatbiotic$SpeciesCategoryKey) & is.na(flatbiotic$SampleKey))){
    sampleVars <- vars[!(vars %in% c(names(StoxBioticData$Station), names(StoxBioticData$Cruise), names(StoxBioticData$Haul), names(StoxBioticData$SpeciesCategory)))]
    if (length(sampleVars)>0){
      stoxWarning(paste("There are some SpeciesCategory with no samples. This may introduce NAs in ", paste(sampleVars, collapse=","), ". Consider filtering with argument 'FilterUpwards'", sep=""))   
    }
  }
  if (any(!is.na(flatbiotic$SampleKey) & is.na(flatbiotic$Individual))){
    indVars <- vars[!(vars %in% c(names(StoxBioticData$Station), names(StoxBioticData$Cruise), names(StoxBioticData$Haul), names(StoxBioticData$SpeciesCategory), names(StoxBioticData$Sample)))]
    if (length(indVars)>0){
      stoxWarning(paste("There are some Samples with no individuals. This may introduce NAs in ", paste(indVars, collapse=","), ". Consider filtering with argument 'FilterUpwards'", sep="")) 
    }
  }
  
  if (!isGiven(GroupingVariables)){
    GroupingVariables <- c("Segment")
    flatlandings$Segment <- "All landings"
    flatbiotic$Segment <- "All landings"
  }
  
  if (isGiven(SamplingVariables)){
    if (any(SamplingVariables %in% names(flatlandings))){
      stop("'SamplingVariables' cannot be variables in 'StoxLandingData', consider using 'GroupingVariables' instead.")
    }
    if (!all(SamplingVariables %in% names(flatbiotic))){
      stop("All 'SamplingVariables' must be variables in 'StoxBioticData'")
    }
  }
  
  if (length(GroupingVariables) == 0){
    stop("No variables to stats::aggregate and compare. Provide parameter 'GroupingVariables'")
  }
  
  if (!all(GroupingVariables %in% names(flatlandings))){
    missing <- GroupingVariables[!(GroupingVariables %in% names(flatlandings))]
    stop(paste("All 'GroupingVariables' must be present in 'StoxLandingData'. Missing:", paste(missing, sep=",")))
  }
  if (!all(GroupingVariables %in% names(flatbiotic))){
    missing <- GroupingVariables[!(GroupingVariables %in% names(flatbiotic))]
    stop(paste("All 'GroupingVariables' must be present in 'StoxBioticData'. Missing:", paste(missing, sep=",")))
  }

  samples <- flatbiotic[,c(GroupingVariables, SamplingVariables, "IndividualRoundWeight", "IndividualAge", "IndividualTotalLength", "CatchFractionWeight", "CatchPlatform", "Haul", "Sample"), with=F]
  
  #prep with some reserved names, to allow counted variables to also be aggregation variables (e.g. IndividualAge)
  if (any(c("Catches", "Vessels", "WeightMeasurements", "LengthMeasurements", "AgeReadings") %in% names(samples))){
    stop("The follwing variable names are reserved for this report and cannot occur in StoxBioticData or StoxLandingData: 'Catches', 'Vessels', 'WeightMeasurements', 'LengthMeasurements', 'AgeReadings'")
  }
  samples$Catches <- samples$Haul
  samples$Vessels <- samples$CatchPlatform
  samples$WeightMeasurements <- !is.na(samples$IndividualRoundWeight)
  samples$LengthMeasurements <- !is.na(samples$IndividualTotalLength)
  samples$AgeReadings <- !is.na(samples$IndividualAge)
  
  sampledTab <- samples[,list(Catches=length(unique(get("Catches"))), 
                              Vessels=length(unique(get("Vessels"))),
                              WeightMeasurements=sum(get("WeightMeasurements")),
                              LengthMeasurements=sum(get("LengthMeasurements")),
                              AgeReadings=sum(get("AgeReadings"))
                              ), by=c(GroupingVariables, SamplingVariables)]
  
  sampledWeights <- samples[,list(WeightOfSampledCatches=sum(get("CatchFractionWeight")[!duplicated(get("Sample"))], na.rm=T)), by=c(GroupingVariables, SamplingVariables)]
  sampledTab <- merge(sampledTab, sampledWeights, by=c(GroupingVariables, SamplingVariables))
  
  landings <- flatlandings[,c(GroupingVariables, "RoundWeight"), with=F]
  landingsTab <- landings[,list(LandedRoundWeight=sum(get("RoundWeight"), na.rm=T)), by=GroupingVariables]
  
  tab <- merge(landingsTab, sampledTab, by=GroupingVariables, all=T)
  tab <- tab[order(tab$LandedRoundWeight, decreasing = T),]
  tab <- setUnits(tab, c("WeightOfSampledCatches", "LandedRoundWeight"), "kg", "mass")
  if (isGiven(Unit)){
    tab <- setUnits(tab, c("WeightOfSampledCatches", "LandedRoundWeight"), Unit, "mass")
  }
  
  if (isGiven(Decimals)){
    tab <- setDecimals(tab, c("WeightOfSampledCatches", "LandedRoundWeight"), Decimals)
  }
  
  output <- list()
  output$FisheriesSampling <- tab
  output$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  output$SamplingVariables <- data.table::data.table(SamplingVariables=SamplingVariables)
  
  return(output)
}

#' Report FDA landings
#' @description 
#'  Report landings in partitions of the fisheries.
#' @details 
#'  Landings are reported partitioned on the provided 'GroupingVariables'.
#'  
#'  Landings are sorted by decreasing weight, except if the column 'CatchDate' is used as GroupingVariable.
#'  In that case Landings are sorted by increasing date, after sorting on other Grouping variables.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'
#'  The units considered valid for weights are those listed for quantity 'mass' in \code{\link[RstoxData]{StoxUnits}}
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns added for identifying corresponding samples
#' @param GroupingVariables Columns of 'StoxBioticData' and 'StoxLandingData' that partitions the fisheries. If not provided, a single row for all landings will be produced.
#' @param Decimals integer specifying the number of decimals to report for 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportFdaLandings$functionParameterDefaults$Decimals`.
#' @param Unit unit for the weights 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportFdaLandings$functionParameterDefaults$Unit`. 
#' @return \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @concept landings functions
#' @concept StoX-functions
#' @examples 
#'  landingsreport <- RstoxFDA::ReportFdaLandings(RstoxFDA::StoxLandingDataExample, 
#'     GroupingVariables = c("Area"), Unit="ton")
#'  landingsreport$FisheriesLandings
#' @export
#' @md
ReportFdaLandings <- function(StoxLandingData, GroupingVariables=character(), Decimals=integer(), Unit=RstoxData::getUnitOptions("mass", conversionRange=c(1,1e12))){
  
  checkMandatory(StoxLandingData, "StoxLandingData")

  Decimals <- getDefault(Decimals, "Decimals", F, RstoxFDA::stoxFunctionAttributes$ReportFdaLandings$functionParameterDefaults$Decimals)
  Unit <- getDefault(Unit, "Unit", F, RstoxFDA::stoxFunctionAttributes$ReportFdaLandings$functionParameterDefaults$Unit)
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("mass", conversionRange=c(1,1e12)))
  
  flatlandings <- StoxLandingData$Landing
  
  if (!isGiven(GroupingVariables)){
    GroupingVariables <- c("Segment")
    flatlandings$Segment <- "All landings"
  }
  
  if (length(GroupingVariables) == 0){
    stop("No variables to stats::aggregate and compare. Provide parameter 'GroupingVariables'")
  }
  
  if (!all(GroupingVariables %in% names(flatlandings))){
    missing <- GroupingVariables[!(GroupingVariables %in% names(flatlandings))]
    stop(paste("All 'GroupingVariables' must be present in 'StoxLandingData'. Missing:", paste(missing, sep=",")))
  }
  
  landingsAggList <- list()
  for (v in GroupingVariables){
    landingsAggList[[v]] <- flatlandings[[v]]
  }
  landings <- flatlandings[,c(GroupingVariables, "RoundWeight"), with=F]
  landingsTab <- data.table::data.table(stats::aggregate(list(LandedRoundWeight=landings$RoundWeight), by=landingsAggList, FUN=function(x){sum(x, na.rm=T)}))
  
  if (!("CatchDate" %in% GroupingVariables)){
    landingsTab <- landingsTab[order(landingsTab$LandedRoundWeight, decreasing = T),]    
  }
  else{
    orderCols <- c(GroupingVariables[GroupingVariables!="CatchDate"], "CatchDate")
    data.table::setorderv(landingsTab, cols = orderCols)
  }

  
  landingsTab <- setUnits(landingsTab, c("LandedRoundWeight"), "kg", "mass")
  if (isGiven(Unit)){
    landingsTab <- setUnits(landingsTab, c("LandedRoundWeight"), Unit, "mass")
  }
  
  if (isGiven(Decimals)){
    landingsTab <- setDecimals(landingsTab, c("LandedRoundWeight"), Decimals)
  }
  
  output <- list()
  output$FisheriesLandings <- landingsTab
  output$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  
  return(output)
}

#' @noRd
reportParameterAtAge <- function(table, aggVariables, parameter, alpha=.1){
  
  stopifnot(all(c("Age", "AgeGroup") %in% names(table)))
  
  output <- list()
  
  aggNames <- aggVariables
  aggNames <- c("Age", "AgeGroup", aggNames)
  stopifnot(length(aggNames) == (ncol(table)-2))

  # NAs may occur fore mean parameters when CatchAtAge is 0 for certain iterations.
  # consider reporting number of iterations used as column in result (sum(!is.na(parameter)))
  result <- table[,list(par=mean(get(parameter), na.rm=T), SD=stats::sd(get(parameter), na.rm=T), Low=stats::quantile(get(parameter), probs = alpha/2.0, na.rm=T), High=stats::quantile(get(parameter), probs = 1-(alpha/2.0), na.rm=T)), by=aggNames]
  
  data.table::setcolorder(result ,c("AgeGroup", "Age", "par", "SD", "Low", "High", aggVariables))
  names(result) <- c("AgeGroup", "Age", parameter, "SD", "Low", "High", aggVariables)
  data.table::setorderv(result, c(aggVariables, "Age"))
  
  output <- list()
  output$FdaReport <- result
  
  output$GroupingVariables <- data.table::data.table(GroupingVariables=aggVariables)
  
  return(output)
}

#' @noRd
#' @importFrom data.table .SD
reportCovarianceAtAge <- function(table, aggVariables, parameter){
  
  stopifnot(all(c("Age", "AgeGroup") %in% names(table)))
  
  output <- list()
  
  aggNames <- aggVariables
  aggNames <- c("Age", "AgeGroup", aggNames)
  stopifnot(length(aggNames) == (ncol(table)-2))
  
  iterations <- length(unique(table$Iteration))
  result <- table[,list(mean=mean(get(parameter))), by=aggNames]
  DevTab <- merge(table, result)
  DevTab$Dev <- DevTab[[parameter]] - DevTab$mean
  DevTab$VariableId <- DevTab[["AgeGroup"]]
  if (length(aggVariables)>0){
    for (var in aggVariables){
      DevTab$VariableId <- paste(DevTab$VariableId, DevTab[[var]], sep="/") 
    }
  }
  
  nameTab <- DevTab[!duplicated(DevTab[["VariableId"]]),.SD,.SDcols=c("VariableId", aggNames)]
  covarTab <- DevTab[,.SD,.SDcols=c("VariableId", "Dev", "Iteration")]
  covarTab2 <- covarTab[,list(VariableId2=get("VariableId"), Dev2=get("Dev")), by=list(Iteration=get("Iteration"))]
  covarTab <- covarTab[covarTab2, on="Iteration", allow.cartesian=TRUE]
  covarTab <- covarTab[,list(Covariance=sum(get("Dev")*get("Dev2"))/(iterations-1)), by=list(VariableId1=get("VariableId"), VariableId2=get("VariableId2"))]
  
  output <- list()
  output$FdaCovariances <- covarTab
  output$Variables <- nameTab
  
  return(output)
}

#' @noRd
reportParameterAtLength <- function(table, aggVariables, parameter, alpha=.1){
  
  stopifnot(all(c("Length", "LengthGroup") %in% names(table)))
  
  output <- list()
  
  aggNames <- aggVariables
  aggNames <- c("Length", "LengthGroup", aggNames)
  stopifnot(length(aggNames) == ncol(table)-2)
  
  result <- table[,list(par=mean(get(parameter)), SD=stats::sd(get(parameter)), Low=stats::quantile(get(parameter), probs = alpha/2.0), High=stats::quantile(get(parameter), probs = 1-(alpha/2.0))), by=aggNames]
  
  data.table::setcolorder(result ,c("LengthGroup", "Length", "par", "SD", "Low", "High", aggVariables))
  names(result) <- c("LengthGroup", "Length", parameter, "SD", "Low", "High", aggVariables)
  data.table::setorderv(result, c(aggVariables, "Length"))
  
  output <- list()
  output$FdaReport <- result
  
  output$GroupingVariables <- data.table::data.table(GroupingVariables=aggVariables)
  
  return(output)
}

#' @noRd
reportParameterAtAgeLength <- function(table, aggVariables, parameter, alpha=.1){
  
  stopifnot(all(c("Length", "LengthGroup", "Age", "AgeGroup") %in% names(table)))
  
  output <- list()
  
  aggNames <- aggVariables
  aggNames <- c("Length", "LengthGroup", "Age", "AgeGroup", aggNames)
  stopifnot(length(aggNames) == ncol(table)-2)
  
  result <- table[,list(par=mean(get(parameter)), SD=stats::sd(get(parameter)), Low=stats::quantile(get(parameter), probs = alpha/2.0), High=stats::quantile(get(parameter), probs = 1-(alpha/2.0))), by=aggNames]
  
  data.table::setcolorder(result ,c("AgeGroup", "Age", "LengthGroup", "Length", "par", "SD", "Low", "High", aggVariables))
  names(result) <- c("AgeGroup", "Age", "LengthGroup", "Length", parameter, "SD", "Low", "High", aggVariables)
  data.table::setorderv(result, c(aggVariables, "Age", "Length"))
  
  output <- list()
  output$FdaReport <- result
  
  output$GroupingVariables <- data.table::data.table(GroupingVariables=aggVariables)
  
  return(output)
}



#' Add character description of Age groups
#' @noRd
setAgeGroup <- function(AgeReport){
  stopifnot(!("AgeGroup" %in% names(AgeReport)))
  stopifnot(("Age" %in% names(AgeReport)))
  
  AgeReport$AgeGroup <- paste("Age", AgeReport$Age)
  
  return(AgeReport)
}

#' Add character description of Length groups
#' @noRd
setLengthGroup <- function(LengthReport, interval){
  stopifnot(!("LengthGroup" %in% names(LengthReport)))
  stopifnot(("Length" %in% names(LengthReport)))
  
  if (isGiven(interval)){
    l <- sort(unique(LengthReport$Length))
    
    #Handle collapsed length groups
    if (length(l)==1){
      diffs <- l
    }
    else{
      diffs <- unique(l[2:length(l)] - l[1:(length(l)-1)])  
    }
    
    if (interval < max(diffs)){
      stoxWarning("Length interval is specified lower than the available resolution. Check if options 'LengthResolution' or 'CollapseLength' are set as intended.")
    }
    
    groups <- seq(0,max(LengthReport$Length)+interval,interval)
    names <- seq(interval,max(LengthReport$Length)+interval,interval)
    LengthReport$Length <- as.numeric(as.character(cut(LengthReport$Length, groups, labels=names)))
  }
  
  l <- sort(unique(LengthReport$Length))
  #Handle collapsed length groups
  if (length(l)==1){
    diffs <- l
  }
  else{
    diffs <- unique(round(l[2:length(l)] - l[1:(length(l)-1)], digits = 10))    
  }
  
  if (length(diffs)==1){
    LengthReport$LengthGroup <- sprintf("\u2329%.1f, %.1f\uFF3D", LengthReport$Length - diffs, LengthReport$Length)  
  }
  else{
    LengthReport$LengthGroup <- sprintf("\u2329 , %.1f\uFF3D", LengthReport$Length)  
  }
  
  return(LengthReport)
}

#' Report catch at age
#' @description 
#'  Tabulates summary statistics for total catch (number) at age from MCMC simulations using Reca.
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  The units considered valid for catch at age in numbers are those listed for quantity 'cardinaltiy' in \code{\link[RstoxData]{StoxUnits}}
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported credible interval. A value of 0.9 gives 90 per cent credible intervals. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchAtAge$functionParameterDefaults$IntervalWidth`
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtAge', 'SD', 'Low' and 'High'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchAtAge$functionParameterDefaults$Decimals`.
#' @param Unit unit for 'CatchAtAge', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtLength}} for reporting length composition.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @examples 
#'   catchAtAgeReport <- RstoxFDA::ReportRecaCatchAtAge(RstoxFDA::RecaCatchAtAgeExample, 
#'         PlusGroup = 13)
#'   catchAtAgeReport$NbyAge
#' @export
#' @md
ReportRecaCatchAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  
  checkMandatory(RecaCatchAtAge, "RecaCatchAtAge")
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  Decimals <- getDefault(Decimals, "Decimals", F, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtAge$functionParameterDefaults$Decimals)
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12)))
  IntervalWidth <- getDefault(IntervalWidth, "IntervalWidth", F, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtAge$functionParameterDefaults$IntervalWidth)
  check_intervalWidth(IntervalWidth)
  
  aggNames <- c("Iteration", "Age", RecaCatchAtAge$GroupingVariables$GroupingVariables)
  stopifnot(length(aggNames) == (ncol(RecaCatchAtAge$CatchAtAge)-2))
  totalOverLength <- RecaCatchAtAge$CatchAtAge[,list(CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  
  totalOverLength <- setAgeGroup(totalOverLength)
  
  if (isGiven(PlusGroup)){
    if (PlusGroup > max(totalOverLength$Age)){
      stop("'PlusGroup' is larger than the oldest age in the model.")
    }
    if (PlusGroup < min(totalOverLength$Age)){
      stop("'PlusGroup' is smaller than the smallest age in the model.")
    }
    
    aggNames <- c("Iteration", "AgeGroup", RecaCatchAtAge$GroupingVariables$GroupingVariables)
    totalOverLength$AgeGroup[totalOverLength$Age>=PlusGroup] <- paste("Age ", PlusGroup, "+", sep="")
    totalOverLength <- totalOverLength[, list(Age=min(get("Age")), CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  }
  
  aggNames <- c(RecaCatchAtAge$GroupingVariables$GroupingVariables)
  
  caa <- reportParameterAtAge(totalOverLength, aggNames, "CatchAtAge", alpha = 1-IntervalWidth)
  
  caa$NbyAge <- caa$FdaReport
  caa$FdaReport <- NULL
  
  caa$NbyAge <- setUnits(caa$NbyAge, "Age", "year", "age")
  caa$NbyAge <- setUnits(caa$NbyAge, c("CatchAtAge", "SD", "Low", "High"), "individuals", "cardinality")
  if (isGiven(Unit)){
    caa$NbyAge <- setUnits(caa$NbyAge, c("CatchAtAge", "SD", "Low", "High"), Unit, "cardinality")  
  }
  
  if (isGiven(Decimals)){
    caa$NbyAge <- setDecimals(caa$NbyAge, c("CatchAtAge", "SD", "Low", "High"), Decimals)
  }
  
  return(caa[c("NbyAge", "GroupingVariables")])
  
}

#' Report covariance of catch at age
#' @description 
#'  Tabulates the covariance between age groups of (number) at age from MCMC simulations using Reca.
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Covariances are obtained from the posterior distribution.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., covariances are calculated between age groups for each of these aggregation variables.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  The units considered valid for catch at age in numbers are those listed for quantity 'cardinaltiy' in \code{\link[RstoxData]{StoxUnits}}
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param Decimals integer specifying the number of decimals to report for 'Covariance'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchAtAgeCovariance$functionParameterDefaults$Decimals`.
#' @param Unit unit for 'CatchAtAge'. Covariance will be provided as the square of this unit.
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeCovarianceData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtLength}} for reporting length composition.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @examples
#'  covariances <- RstoxFDA::ReportRecaCatchAtAgeCovariance(RstoxFDA::RecaCatchAtAgeExample, 
#'         PlusGroup = 13)
#'  covariances$CovarianceNbyAge
#' @export
#' @md
ReportRecaCatchAtAgeCovariance <- function(RecaCatchAtAge, PlusGroup=integer(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  
  checkMandatory(RecaCatchAtAge, "RecaCatchAtAge")
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  Decimals <- getDefault(Decimals, "Decimals", F, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtAgeCovariance$functionParameterDefaults$Decimals)
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12)))

  aggNames <- c("Iteration", "Age", RecaCatchAtAge$GroupingVariables$GroupingVariables)
  stopifnot(length(aggNames) == (ncol(RecaCatchAtAge$CatchAtAge)-2))
  totalOverLength <- RecaCatchAtAge$CatchAtAge[,list(CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  
  totalOverLength <- setAgeGroup(totalOverLength)
  
  if (isGiven(PlusGroup)){
    if (PlusGroup > max(totalOverLength$Age)){
      stop("'PlusGroup' is larger than the oldest age in the model.")
    }
    if (PlusGroup < min(totalOverLength$Age)){
      stop("'PlusGroup' is smaller than the smallest age in the model.")
    }
    
    aggNames <- c("Iteration", "AgeGroup", RecaCatchAtAge$GroupingVariables$GroupingVariables)
    totalOverLength$AgeGroup[totalOverLength$Age>=PlusGroup] <- paste("Age ", PlusGroup, "+", sep="")
    totalOverLength <- totalOverLength[, list(Age=min(get("Age")), CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  }
  
  aggNames <- c(RecaCatchAtAge$GroupingVariables$GroupingVariables)
  
  totalOverLength <- setUnits(totalOverLength, c("CatchAtAge"), "individuals", "cardinality")
  if (isGiven(Unit)){
    totalOverLength <- setUnits(totalOverLength, c("CatchAtAge"), Unit, "cardinality")  
  }
  
  cov <- reportCovarianceAtAge(totalOverLength, aggNames, "CatchAtAge")
  
  cov$CovarianceNbyAge <- cov$FdaCovariances
  cov$FdaCovariances <- NULL
  cov$Variables <- setUnits(cov$Variables, c("Age"), "year", "age")
  
  if (isGiven(Decimals)){
    cov$CovarianceNbyAge <- setDecimals(cov$CovarianceNbyAge, c("Covariance"), Decimals)
  }
  
  return(cov[c("CovarianceNbyAge", "Variables")])
  
}

#' Report catch at length
#' @description 
#'  Tabulates summary statistics for total catch (number) at length from MCMC simulations using Reca.
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals.
#'  
#'  Different length groups than the ones reported in the argument 'RecaCatchAtAge'
#'  may be specified with the argument 'IntervalWidth'. This will specify equi-intervalled
#'  lengthgroups with the smallest lengthgroup starting at 0. If it does not align with the
#'  length groups reported in 'RecaCatchAtAge' length group assignment is done to
#'  the highest overlapping length group.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  The units considered valid for catch at length in numbers are those listed for quantity 'cardinaltiy' in \code{\link[RstoxData]{StoxUnits}}
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param IntervalWidth The width of the reported credible interval. A value of 0.9 gives 90 per cent credible intervals. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchAtLength$functionParameterDefaults$IntervalWidth`.
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtLength', 'SD', 'Low' and 'High'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchAtLength$functionParameterDefaults$Decimals`.
#' @param Unit unit for 'CatchAtLength', 'SD', 'Low' and 'High'
#' @param LengthInterval width of length bins in cm. If not provided, the interval in 'RecaCatchAtAge' will be used.
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtLengthData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for reporting age composition
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @examples
#'  catchAtLength <- RstoxFDA::ReportRecaCatchAtLength(RstoxFDA::RecaCatchAtAgeExample,
#'           LengthInterval = 10)
#'  catchAtLength$NbyLength
#'  
#' @export
#' @md
ReportRecaCatchAtLength <- function(RecaCatchAtAge, IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12)), LengthInterval=numeric()){
  
  checkMandatory(RecaCatchAtAge, "RecaCatchAtAge")
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  Decimals <- getDefault(Decimals, "Decimals", F, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtLength$functionParameterDefaults$Decimals)
  
  if (!isGiven(LengthInterval)){
    LengthInterval <- NULL
  }
  
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12)))
  
  IntervalWidth <- getDefault(IntervalWidth, "IntervalWidth", F, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchAtLength$functionParameterDefaults$IntervalWidth)
  check_intervalWidth(IntervalWidth)
  
  aggNames <- c("Iteration", "Length", RecaCatchAtAge$GroupingVariables$GroupingVariables)
  stopifnot(length(aggNames) == (ncol(RecaCatchAtAge$CatchAtAge)-2))
  totalOverLength <- RecaCatchAtAge$CatchAtAge[,list(CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  
  totalOverLength <- setLengthGroup(totalOverLength, LengthInterval)    

  #aggregate length groups
  aggNames <- c("Iteration", "LengthGroup", RecaCatchAtAge$GroupingVariables$GroupingVariables)
  totalOverLength <- totalOverLength[, list(Length=max(get("Length")), CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  
  aggNames <- c(RecaCatchAtAge$GroupingVariables$GroupingVariables)
  
  caa <- reportParameterAtLength(totalOverLength, aggNames, "CatchAtAge", alpha = 1-IntervalWidth)
  caa$FdaReport$CatchAtLength <- caa$FdaReport$CatchAtAge
  caa$FdaReport$CatchAtAge <- NULL
  
  caa$NbyLength <- caa$FdaReport
  caa$FdaReport <- NULL

  #set units
  caa$NbyLength <- setUnits(caa$NbyLength, "Length", "cm", "length")
  caa$NbyLength <- setUnits(caa$NbyLength, c("CatchAtLength", "SD", "Low", "High"), "individuals", "cardinality")

  #convert units
  caa$NbyLength <- setUnits(caa$NbyLength, c("CatchAtLength", "SD", "Low", "High"), Unit, "cardinality")  
  caa$NbyLength <- setDecimals(caa$NbyLength, c("CatchAtLength", "SD", "Low", "High"), Decimals)

  return(caa[c("NbyLength", "GroupingVariables")])
  
}

#' Report catch at length and age
#' @description 
#'  Tabulates summary statistics for total catch (number) at all age-length combinations from MCMC simulations using Reca.
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals.
#'  
#'  Different length groups than the ones reported in the argument 'RecaCatchAtAge'
#'  may be specified with the argument 'IntervalWidth'. This will specify equi-intervalled
#'  lengthgroups with the smallest lengthgroup starting at 0. If it does not align with the
#'  length groups reported in 'RecaCatchAtAge' length group assignment is done to
#'  the highest overlapping length group.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  The units considered valid for catch at length in numbers are those listed for quantity 'cardinaltiy' in \code{\link[RstoxData]{StoxUnits}}
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param LengthInterval width of length bins in cm. If not provided, the interval in 'RecaCatchAtAge' will be used.
#' @param IntervalWidth The width of the reported credible interval. A value of 0.9 gives 90 per cent credible intervals. Defaults to `r stoxFunctionAttributes$ReportRecaCatchAtLengthAndAge$functionParameterDefaults$IntervalWidth`
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtAge', 'SD', 'Low' and 'High'. Defaults to `r stoxFunctionAttributes$ReportRecaCatchAtLengthAndAge$functionParameterDefaults$Decimals`.
#' @param Unit unit for 'CatchAtAge', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtLengthAndAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for reporting age composition
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @examples 
#'   lengthAge <- ReportRecaCatchAtLengthAndAge(RstoxFDA::RecaCatchAtAgeExample, 13, 10)
#'   lengthAge$NbyLengthAge
#' @export
#' @md
ReportRecaCatchAtLengthAndAge <- function(RecaCatchAtAge, 
                                          PlusGroup=integer(), 
                                          LengthInterval=numeric(), 
                                          IntervalWidth=numeric(), 
                                          Decimals=integer(), 
                                          Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  
  checkMandatory(RecaCatchAtAge, "RecaCatchAtAge")
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  Decimals <- getDefault(Decimals, "Decimals", F, stoxFunctionAttributes$ReportRecaCatchAtLengthAndAge$functionParameterDefaults$Decimals)
  IntervalWidth <- getDefault(IntervalWidth, "IntervalWidth", F, stoxFunctionAttributes$ReportRecaCatchAtLengthAndAge$functionParameterDefaults$IntervalWidth)
  check_intervalWidth(IntervalWidth)
  
  if (!isGiven(LengthInterval)){
    LengthInterval <- NULL
  }
  
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12)))
  
  aggNames <- c("Iteration", "Length", "Age", RecaCatchAtAge$GroupingVariables$GroupingVariables)
  stopifnot(length(aggNames) == (ncol(RecaCatchAtAge$CatchAtAge)-1))
  totalOverLength <- RecaCatchAtAge$CatchAtAge[,list(CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  
  totalOverLength <- setLengthGroup(totalOverLength, LengthInterval)
  totalOverLength <- setAgeGroup(totalOverLength)
  
  if (isGiven(PlusGroup)){
    if (PlusGroup > max(totalOverLength$Age)){
      stop("'PlusGroup' is larger than the oldest age in the model.")
    }
    if (PlusGroup < min(totalOverLength$Age)){
      stop("'PlusGroup' is smaller than the smallest age in the model.")
    }
    
    aggNames <- c("Iteration", "AgeGroup", "LengthGroup", RecaCatchAtAge$GroupingVariables$GroupingVariables)
    totalOverLength$AgeGroup[totalOverLength$Age>=PlusGroup] <- paste("Age ", PlusGroup, "+", sep="")
    totalOverLength <- totalOverLength[, list(Age=min(get("Age")), Length=max(get("Length")), CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
  }
  
  aggNames <- c(RecaCatchAtAge$GroupingVariables$GroupingVariables)
  
  caa <- reportParameterAtAgeLength(totalOverLength, aggNames, "CatchAtAge", alpha = 1-IntervalWidth)
  caa$FdaReport$CatchAtAgeLength <- caa$FdaReport$CatchAtAge
  caa$FdaReport$CatchAtAge <- NULL
  caa$FdaReport <- setUnits(caa$FdaReport, "Length", "cm", "length")
  caa$FdaReport <- setUnits(caa$FdaReport, "Age", "year", "age")
  caa$FdaReport <- setUnits(caa$FdaReport, c("CatchAtAgeLength", "SD", "Low", "High"), "individuals", "cardinality")
  
  #convert units
  caa$FdaReport <- setUnits(caa$FdaReport, c("CatchAtAgeLength", "SD", "Low", "High"), Unit, "cardinality")  
  caa$FdaReport <- setDecimals(caa$FdaReport, c("CatchAtAgeLength", "SD", "Low", "High"), Decimals)
  
  caa$NbyLengthAge <- caa$FdaReport
  caa$FdaReport <- NULL
  
  return(caa[c("NbyLengthAge", "GroupingVariables")])
  
}

#' Calcualtes means for RecaCatchAtAge with plusgroups
#' @noRd
getPlusGroupMeans <- function(RecaCatchAtAge, table, parameter, PlusGroup=integer()){
  mw <- setAgeGroup(RecaCatchAtAge[[table]])
  
  # handle plusggroup
  if (isGiven(PlusGroup)){
    
    if (PlusGroup > max(mw$Age)){
      stop("'PlusGroup' is larger than the oldest age in the model.")
    }
    if (PlusGroup < min(mw$Age)){
      stop("'PlusGroup' is smaller than the smallest age in the model.")
    }
    
    #total over length, all ages
    aggNames <- c("Iteration", "Age", RecaCatchAtAge$GroupingVariables$GroupingVariables)
    stopifnot(length(aggNames) == (ncol(RecaCatchAtAge$CatchAtAge)-2))
    totalOverLength <- RecaCatchAtAge$CatchAtAge[,list(CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
    totalOverLength <- setAgeGroup(totalOverLength)
    
    # mean weights, all ages
    mw <- merge(mw, totalOverLength)
    
    # add plusgroup and aggregate
    aggNames <- c("Iteration", "AgeGroup", RecaCatchAtAge$GroupingVariables$GroupingVariables)
    mw$AgeGroup[mw$Age>=PlusGroup] <- paste("Age ", PlusGroup, "+", sep="")
    
    mw <- mw[, list(Age=min(get("Age")), Total=sum(get(parameter)*get("CatchAtAge")), CatchAtAge=sum(get("CatchAtAge")), meanparam=get(parameter)), by=aggNames]
    
    # in order to avoid uneccessary problems with caa estimated to zero, 
    # and to keep calculation consistent with the case when PlusGroup is not set:
    # set to the mean returned from ECA for non-plusgroups
    
    mw[[parameter]][mw$Age>=PlusGroup] <- mw$Total[mw$Age>=PlusGroup] / mw$CatchAtAge[mw$Age>=PlusGroup]
    mw[[parameter]][mw$Age>=PlusGroup & mw$CatchAtAge==0] <- NA #set mean of parameter to NA, when CatchAtAge for PlusGroup is 0
    mw[[parameter]][mw$Age<PlusGroup] <- mw$meanparam[mw$Age<PlusGroup]
    mw$Total <- NULL
    mw$CatchAtAge <- NULL
    mw$meanparam <- NULL
    
  }
  
  return(mw)
}

#' Report weight at age
#' @description 
#'  Tabulates summary statistics for mean weights at age from MCMC simulations using Reca.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Mean weight for plus-groups are a weighted by the relative catch-at-age in each composite age group.
#'  For iterations where all of the plus-group ages have a zero catch at age, this weight is not defined,
#'  and summary statistics are obtained from the remaining iterations.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  The units considered valid for mean weights are those listed for quantity 'mass' in \code{\link[RstoxData]{StoxUnits}}
#'  
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as 90% equal-tailed credible intervals.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported credible interval. A value of 0.9 gives 90 per cent credible intervals. Defaults to `r stoxFunctionAttributes$ReportRecaWeightAtAge$functionParameterDefaults$IntervalWidth`.
#' @param Decimals integer specifying the number of decimals to report for 'MeanIndividualWeight', 'SD', 'Low' and 'High'. Defaults to `r stoxFunctionAttributes$ReportRecaWeightAtAge$functionParameterDefaults$Decimals`.
#' @param Threshold threshold for reporting mean weight. Rows with an estimated Catch At Age (number of individuals) lower than this will have NA reported for their mean weight. Defaults to `r stoxFunctionAttributes$ReportRecaWeightAtAge$functionParameterDefaults$Threshold`.
#' @param Unit unit for 'MeanIndividualWeight', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @examples 
#'   weightAtAge <- RstoxFDA::ReportRecaWeightAtAge(RstoxFDA::RecaCatchAtAgeExample, 
#'        PlusGroup = 13, Threshold = 1000, Decimals = 0, Unit = "g")
#'   weightAtAge
#' @export
#' @md
ReportRecaWeightAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Threshold=numeric(), Unit=RstoxData::getUnitOptions("mass", conversionRange=c(1e-4, 10))){
  
  checkMandatory(RecaCatchAtAge, "RecaCatchAtAge")
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  if (!isGiven(PlusGroup)){
    PlusGroup <- NULL
  }
  Decimals <- getDefault(Decimals, "Decimals", F, stoxFunctionAttributes$ReportRecaWeightAtAge$functionParameterDefaults$Decimals)
  IntervalWidth <- getDefault(IntervalWidth, "IntervalWidth", F, stoxFunctionAttributes$ReportRecaWeightAtAge$functionParameterDefaults$IntervalWidth)
  check_intervalWidth(IntervalWidth)
  Threshold <- getDefault(Threshold, "Threshold", F, stoxFunctionAttributes$ReportRecaWeightAtAge$functionParameterDefaults$Threshold)
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("mass", conversionRange=c(1e-4, 10)))
  
    
  meanWeightAtAge <- getPlusGroupMeans(RecaCatchAtAge, "MeanWeight", "MeanIndividualWeight", PlusGroup)
  mwaa <-reportParameterAtAge(meanWeightAtAge, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualWeight", alpha = 1 - IntervalWidth)
  
  
  #set age groups with catches below threshold to NA
  caa<-ReportRecaCatchAtAge(RecaCatchAtAge, PlusGroup = PlusGroup)
  caa$NbyAge$SD <- NULL
  caa$NbyAge$Low <- NULL
  caa$NbyAge$High <- NULL
  caa$NbyAge$include <- caa$NbyAge$CatchAtAge >= Threshold
  caa$NbyAge$order <- 1:nrow(caa$NbyAge)
  caa$NbyAge$CatchAtAge <- NULL
  
  mwaa$FdaReport <- merge(mwaa$FdaReport, caa$NbyAge)
  stopifnot(nrow(mwaa$FdaReport) == nrow(caa$FdaReport))
  mwaa$FdaReport$MeanIndividualWeight[!mwaa$FdaReport$include] <- NA
  mwaa$FdaReport$SD[!mwaa$FdaReport$include] <- NA
  mwaa$FdaReport$Low[!mwaa$FdaReport$include] <- NA
  mwaa$FdaReport$High[!mwaa$FdaReport$include] <- NA
  mwaa$FdaReport$include <- NULL
  mwaa$FdaReport <- mwaa$FdaReport[order(mwaa$FdaReport$order),]
  mwaa$FdaReport$order <- NULL
  
  mwaa$FdaReport <- setUnits(mwaa$FdaReport, "Age", "year", "age")
  mwaa$FdaReport <- setUnits(mwaa$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), "kg", "mass")
  
  #convert unit
  mwaa$FdaReport <- setUnits(mwaa$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), Unit, "mass")  
  mwaa$FdaReport <- setDecimals(mwaa$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), Decimals)
  
  mwaa$MeanWeightByAge <- mwaa$FdaReport
  mwaa$FdaReport <- NULL
  
  return(mwaa[c("MeanWeightByAge", "GroupingVariables")])

}

#' Report length at age
#' @description 
#'  Tabulates summary statistics for mean length at age from MCMC simulations using Reca.
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals are reported.
#'  
#'  Mean length for plus-groups are a weighted by the relative catch-at-age in each composite age group.
#'  For iterations where all of the plus-group ages have a zero catch at age, this weight is not defined,
#'  and summary statistics are obtained from the remaining iterations.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  The units considered valid for mean lengths are those listed for quantity 'length' in \code{\link[RstoxData]{StoxUnits}}
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported credible interval. A value of 0.9 gives 90 per cent credible intervals. Defaults to `r stoxFunctionAttributes$ReportRecaLengthAtAge$functionParameterDefaults$IntervalWidth`.
#' @param Decimals integer specifying the number of decimals to report for 'MeanIndividualLength', 'SD', 'Low' and 'High'. Defaults to `r stoxFunctionAttributes$ReportRecaLengthAtAge$functionParameterDefaults$Decimals`.
#' @param Threshold threshold for reporting mean weight. Rows with an estimated Catch At Age (number of individuals) lower than this will have NA reported for their mean length Defaults to `r stoxFunctionAttributes$ReportRecaLengthAtAge$functionParameterDefaults$Threshold`.
#' @param Unit unit for 'MeanIndividualLength', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaLengthAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @examples
#'   lengthAtAge <- ReportRecaLengthAtAge(RstoxFDA::RecaCatchAtAgeExample, 
#'         PlusGroup = 13, Unit="cm", Decimals = 0)
#'   lengthAtAge$MeanLengthByAge
#' @export
ReportRecaLengthAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Threshold=numeric(), Unit=RstoxData::getUnitOptions("length", conversionRange=c(1e-7, 10))){
  checkMandatory(RecaCatchAtAge, "RecaCatchAtAge")
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  if (!isGiven(PlusGroup)){
    PlusGroup <- NULL
  }
  
  Decimals <- getDefault(Decimals, "Decimals", F, stoxFunctionAttributes$ReportRecaLengthAtAge$functionParameterDefaults$Decimals)
  IntervalWidth <- getDefault(IntervalWidth, "IntervalWidth", F, stoxFunctionAttributes$ReportRecaLengthAtAge$functionParameterDefaults$IntervalWidth)
  check_intervalWidth(IntervalWidth)
  Threshold <- getDefault(Threshold, "Threshold", F, stoxFunctionAttributes$ReportRecaLengthAtAge$functionParameterDefaults$Threshold)
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("length", conversionRange=c(1e-7, 10)))
  
  meanLengthAtAge <- getPlusGroupMeans(RecaCatchAtAge, "MeanLength", "MeanIndividualLength", PlusGroup)
  mla <- reportParameterAtAge(meanLengthAtAge, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualLength", alpha = 1 - IntervalWidth)
  
  #set age groups with catches below threshold to NA
  caa<-ReportRecaCatchAtAge(RecaCatchAtAge, PlusGroup = PlusGroup)
  caa$NbyAge$SD <- NULL
  caa$NbyAge$Low <- NULL
  caa$NbyAge$High <- NULL
  caa$NbyAge$include <- caa$NbyAge$CatchAtAge >= Threshold
  caa$NbyAge$order <- 1:nrow(caa$NbyAge)
  caa$NbyAge$CatchAtAge <- NULL
  
  mla$FdaReport <- merge(mla$FdaReport, caa$NbyAge)
  stopifnot(nrow(mla$FdaReport) == nrow(caa$FdaReport))
  mla$FdaReport$MeanIndividualLength[!mla$FdaReport$include] <- NA
  mla$FdaReport$SD[!mla$FdaReport$include] <- NA
  mla$FdaReport$Low[!mla$FdaReport$include] <- NA
  mla$FdaReport$High[!mla$FdaReport$include] <- NA
  mla$FdaReport$include <- NULL
  mla$FdaReport <- mla$FdaReport[order(mla$FdaReport$order),]
  mla$FdaReport$order <- NULL
  
  mla$FdaReport <- setUnits(mla$FdaReport, "Age", "year", "age")
  mla$FdaReport <- setUnits(mla$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), "cm", "length")
  
  # convert units
  mla$FdaReport <- setUnits(mla$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), Unit, "length")  
  mla$FdaReport <- setDecimals(mla$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), Decimals)
  
  mla$MeanLengthByAge <- mla$FdaReport
  mla$FdaReport <- NULL
  
  return(mla[c("MeanLengthByAge","GroupingVariables")])
}

#' Report catch statistics
#' @description 
#'  Report summary statistics for landed catches from MCMC simulations using Reca.
#'  This function reports estimated total catch, mean length, mean weight, and mean age.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  The units considered valid for mean lengths are those listed for quantity 'length' in \code{\link[RstoxData]{StoxUnits}}
#'  The units considered valid for weights are those listed for quantity 'mass' in \code{\link[RstoxData]{StoxUnits}}
#'  The units considered valid for total catch in numbers are those listed for quantity 'cardinality' in \code{\link[RstoxData]{StoxUnits}}
#'  
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals are reported.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param IntervalWidth The width of the reported credible interval. A value to 0.9 gives 90 per cent credible intervals. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$IntervalWidth`.
#' @param UseDefaultDecimalOptions logical determining whether to use default decimal options.
#' @param DecimalTotalNumber integer specifying the number of decimals to report for 'TotalNumber', and the corresponding 'SD', 'Low' and 'High'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalTotalNumber`.
#' @param DecimalTotalWeight integer specifying the number of decimals to report for 'TotalWeightDefaults', and the corresponding 'SD', 'Low' and 'High'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalTotalWeight`.
#' @param DecimalMeanAge integer specifying the number of decimals to report for 'MeanIndividualAge', and the corresponding 'SD', 'Low' and 'High'. `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanAge`.
#' @param DecimalMeanWeight integer specifying the number of decimals to report for 'MeanIndividualWeight', and the corresponding 'SD', 'Low' and 'High'. `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanWeight`.
#' @param DecimalMeanLength integer specifying the number of decimals to report for 'MeanIndividualLength', and the corresponding 'SD', 'Low' and 'High'.`r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanLength`.
#' @param UseDefaultUnitOptions logical determining whether to use default unit options.
#' @param UnitTotalNumber unit for total catch in numbers. `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitTotalNumber`.
#' @param UnitTotalWeight unit for weight of total catch. `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitTotalWeight`.
#' @param UnitMeanWeight unit for mean weight. `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitMeanWeight`.
#' @param UnitMeanLength unit for mean length. `r RstoxFDA:::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitMeanLength`.
#' @return \code{\link[RstoxFDA]{ReportFdaSummaryData}}
#' @seealso 
#'  \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for reporting catch, mean length and mean weight by age groups
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtLength}} for reporting length distributions
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtLengthAndAge}} for reporting catch by age-group and length-group combinations.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @examples 
#'  catchStats <- ReportRecaCatchStatistics(RstoxFDA::RecaCatchAtAgeExample)
#'  catchStats$MeanAge
#'  catchStats$MeanWeight
#'  catchStats$MeanLength
#'  
#'  #Note that there is no error on the total weight estimate, when there are no grouping variables
#'  catchStats$TotalWeight
#'  catchStats$TotalNumber
#' @export
#' @md
ReportRecaCatchStatistics <- function(RecaCatchAtAge, IntervalWidth=numeric(), 
                                      UseDefaultDecimalOptions=TRUE, 
                                      DecimalTotalNumber=integer(), 
                                      DecimalTotalWeight=integer(), 
                                      DecimalMeanAge=integer(), 
                                      DecimalMeanWeight=integer(), 
                                      DecimalMeanLength=integer(), 
                                      UseDefaultUnitOptions=TRUE, 
                                      UnitTotalNumber=RstoxData::getUnitOptions("cardinality", conversionRange=c(1, 1e12)), 
                                      UnitTotalWeight=RstoxData::getUnitOptions("mass", conversionRange=c(1, 1e12)), 
                                      UnitMeanWeight=RstoxData::getUnitOptions("mass", conversionRange=c(1e-4, 10)), 
                                      UnitMeanLength=RstoxData::getUnitOptions("length", conversionRange=c(1e-4, 10))){
  
  checkMandatory(RecaCatchAtAge, "RecaCatchAtAge")
  IntervalWidth <- getDefault(IntervalWidth, "IntervalWidth", F, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$IntervalWidth)
  check_intervalWidth(IntervalWidth)
  DecimalTotalNumber <- getDefault(DecimalTotalNumber, "DecimalTotalNumber", UseDefaultDecimalOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalTotalNumber)
  DecimalTotalWeight <- getDefault(DecimalTotalWeight, "DecimalTotalWeight", UseDefaultDecimalOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalTotalWeight)
  DecimalMeanAge <- getDefault(DecimalMeanAge, "DecimalMeanAge", UseDefaultDecimalOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanAge)
  DecimalMeanWeight <- getDefault(DecimalMeanWeight, "DecimalMeanWeight", UseDefaultDecimalOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanWeight)
  DecimalMeanLength <- getDefault(DecimalMeanLength, "DecimalMeanLength", UseDefaultDecimalOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$DecimalMeanLength)
  
  UnitMeanLength <- getDefault(UnitMeanLength, "UnitMeanLength", UseDefaultUnitOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitMeanLength)
  UnitMeanLength <- checkOptions(UnitMeanLength, "UnitMeanLength", RstoxData::getUnitOptions("length", conversionRange=c(1e-4, 10)))

  UnitMeanWeight <- getDefault(UnitMeanWeight, "UnitMeanWeight", UseDefaultUnitOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitMeanWeight)
  UnitMeanWeight <- checkOptions(UnitMeanWeight, "UnitMeanWeight", RstoxData::getUnitOptions("mass", conversionRange=c(1e-4, 10)))
  
  UnitTotalWeight <- getDefault(UnitTotalWeight, "UnitTotalWeight", UseDefaultUnitOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitTotalWeight)
  UnitTotalWeight <- checkOptions(UnitTotalWeight, "UnitTotalWeight", RstoxData::getUnitOptions("mass", conversionRange=c(1, 1e12)))
  
  UnitTotalNumber <- getDefault(UnitTotalNumber, "UnitTotalNumber", UseDefaultUnitOptions, RstoxFDA::stoxFunctionAttributes$ReportRecaCatchStatistics$functionParameterDefaults$UnitTotalNumber)
  UnitTotalNumber <- checkOptions(UnitTotalNumber, "UnitTotalNumber", RstoxData::getUnitOptions("cardinality", conversionRange=c(1, 1e12)))

  UnitAge <- "year"
  
  # get mean catch statistics by collapsing to a singe plusgroup
  
  # mean age
  # hack caa table so that it fits getPLusGroupMeans
  RecaCatchAtAge$MeanAge <- RecaCatchAtAge$CatchAtAge
  RecaCatchAtAge$MeanAge$MeanIndividualAge <- RecaCatchAtAge$CatchAtAge$Age
  RecaCatchAtAge$MeanAge$CatchAtAge <- NULL
  mca <- getPlusGroupMeans(RecaCatchAtAge, "MeanAge", "MeanIndividualAge",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  
  meanAge <- reportParameterAtAge(mca, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualAge", alpha = 1 - IntervalWidth)
  meanAge$FdaReport$AgeGroup <- NULL
  meanAge$FdaReport$Age <- NULL
  
  meanAge$FdaReport <- setUnits(meanAge$FdaReport, c("MeanIndividualAge", "SD", "Low", "High"), UnitAge, "age")
  meanAge$FdaReport <- setDecimals(meanAge$FdaReport, c("MeanIndividualAge", "SD", "Low", "High"), DecimalMeanAge)

  # mean weight
  mcw <- getPlusGroupMeans(RecaCatchAtAge, "MeanWeight", "MeanIndividualWeight",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  meanWeight <- reportParameterAtAge(mcw, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualWeight", alpha = 1 - IntervalWidth)
  meanWeight$FdaReport$AgeGroup <- NULL
  meanWeight$FdaReport$Age <- NULL
  

  meanWeight$FdaReport <- setDecimals(meanWeight$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), DecimalMeanWeight)
  
  meanWeight$FdaReport <- setUnits(meanWeight$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), "kg", "mass")
  #convert unit
  meanWeight$FdaReport <- setUnits(meanWeight$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), UnitMeanWeight, "mass")
  
  
  # mean length
  mlw <- getPlusGroupMeans(RecaCatchAtAge, "MeanLength", "MeanIndividualLength",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  meanLength <- reportParameterAtAge(mlw, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualLength", alpha = 1 - IntervalWidth)
  meanLength$FdaReport$AgeGroup <- NULL
  meanLength$FdaReport$Age <- NULL
  
  
  meanLength$FdaReport <- setDecimals(meanLength$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), DecimalMeanLength)
  meanLength$FdaReport <- setUnits(meanLength$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), "cm", "length")
  
  #convert unit
  meanLength$FdaReport <- setUnits(meanLength$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), UnitMeanLength, "length")
  
  # total weight and total number
  # hack to use ReportRecaCatchAtAge
  # ReportRecaCatchAtAge has already set units.
  #

  mm <- merge(RecaCatchAtAge$CatchAtAge, RecaCatchAtAge$MeanWeight)
  mm$CatchAtAge <- mm$CatchAtAge*mm$MeanIndividualWeight
  mm$MeanIndividualWeight <- NULL
  ss <- RecaCatchAtAge
  ss$CatchAtAge <- mm
  TotalWeight<-ReportRecaCatchAtAge(ss, PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  names(TotalWeight$NbyAge)[names(TotalWeight$NbyAge)=="CatchAtAge"] <- "TotalWeight"
  
  TotalWeight$NbyAge$TotalWeight <- RstoxData::setUnit(TotalWeight$NbyAge$TotalWeight, NA)
  TotalWeight$NbyAge$TotalWeight <- RstoxData::setUnit(TotalWeight$NbyAge$TotalWeight, RstoxData::findUnit("mass", "kg"), assertNew = T)
  TotalWeight$NbyAge$SD <- RstoxData::setUnit(TotalWeight$NbyAge$SD, NA)
  TotalWeight$NbyAge$SD <- RstoxData::setUnit(TotalWeight$NbyAge$SD, RstoxData::findUnit("mass", "kg"), assertNew = T)
  TotalWeight$NbyAge$Low <- RstoxData::setUnit(TotalWeight$NbyAge$Low, NA)
  TotalWeight$NbyAge$Low <- RstoxData::setUnit(TotalWeight$NbyAge$Low, RstoxData::findUnit("mass", "kg"), assertNew = T)
  TotalWeight$NbyAge$High <- RstoxData::setUnit(TotalWeight$NbyAge$High, NA)
  TotalWeight$NbyAge$High <- RstoxData::setUnit(TotalWeight$NbyAge$High, RstoxData::findUnit("mass", "kg"), assertNew = T)
  
  TotalWeight$NbyAge$AgeGroup <- NULL
  TotalWeight$NbyAge$Age <- NULL
  
  #convert unit
  TotalWeight$NbyAge <- setUnits(TotalWeight$NbyAge, c("TotalWeight", "SD", "Low", "High"), UnitTotalWeight, "mass")
  TotalWeight$NbyAge <- setDecimals(TotalWeight$NbyAge, c("TotalWeight", "SD", "Low", "High"), DecimalTotalWeight)
  
  # total number
  TotalNumber<-ReportRecaCatchAtAge(RecaCatchAtAge, PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age), Unit = "individuals")
  names(TotalNumber$NbyAge)[names(TotalNumber$NbyAge)=="CatchAtAge"] <- "TotalNumber"
  TotalNumber$NbyAge$AgeGroup <- NULL
  TotalNumber$NbyAge$Age <- NULL
  
  #convert unit
  TotalNumber$NbyAge <- setUnits(TotalNumber$NbyAge, c("TotalNumber", "SD", "Low", "High"), UnitTotalNumber, "cardinality")
  TotalNumber$NbyAge <- setDecimals(TotalNumber$NbyAge, c("TotalNumber", "SD", "Low", "High"), DecimalTotalNumber)
  
  
  # combine
  output <- list()
  output$MeanAge <- meanAge$FdaReport
  output$MeanWeight <- meanWeight$FdaReport
  output$MeanLength <- meanLength$FdaReport
  output$TotalWeight <- TotalWeight$NbyAge
  output$TotalNumber <- TotalNumber$NbyAge
  output$GroupingVariables <- meanAge$GroupingVariables
  
  return(output)
}

#' Report SOP test
#' @description 
#'  Report sum-of-product test (SOP-test) for catch estimates.
#' 
#'  Mean weight at age and estimated catch (numbers) at age is used to compute total catches
#'  and the relative difference to reported landings are reported. Missing values (NAs) are ignored
#'  (exlcuded from sums).
#'  
#'  The report will be generated for landings decomposed on the provided 'GroupingVariables'
#'  which must be available in both 'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData'
#'  and 'StoxLandingData'.
#'  
#'  'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData' must be decomposed on the same
#'  'GroupingVariables' and must be reported for the same age groups
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  The units considered valid for mean lengths are those listed for quantity 'fraction' in \code{\link[RstoxData]{StoxUnits}}
#' @param ReportFdaCatchAtAgeData \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}} with estimates of total catch at age
#' @param ReportFdaWeightAtAgeData \code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}} with estimates of mean weight at age for individual fish
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#' @param GroupingVariables Columns of 'StoxLandingData' that partitions the landings into groups SOP tests should be reported for.
#' @param DecimalWeight integer specifying the number of decimals to report for weights: 'TotalWeightEstimated', 'LandedWeight', and 'Difference'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportFdaSOP$functionParameterDefaults$DecimalWeight`.
#' @param DecimalFraction integer specifying the number of decimals to report for 'RelativeDifference'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportFdaSOP$functionParameterDefaults$DecimalFraction`.
#' @param UnitFraction unit for the RelativeDifference. E.g. '0.' for decimal notation or '\%' for percent.
#' @return \code{\link[RstoxFDA]{ReportFdaSopData}}
#' @seealso 
#'  \code{\link[RstoxFDA]{ReportRecaWeightAtAge}} and \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for some ways of preparing 'ReportFdaWeightAtAgeData' and 'ReportFdaCatchAtAgeData'.
#'  \code{\link[RstoxData]{StoxLanding}} and \code{\link[RstoxData]{FilterStoxLanding}} for ways of preparing 'StoxLandingData'.
#' @concept data QA functions
#' @concept StoX-functions
#' @examples 
#'  catchAtAge <- RstoxFDA::ReportRecaCatchAtAge(RstoxFDA::RecaCatchAtAgeExample)
#'  weightAtAge <- RstoxFDA::ReportRecaWeightAtAge(RstoxFDA::RecaCatchAtAgeExample)
#'  sop <- ReportFdaSOP(catchAtAge, weightAtAge, RstoxFDA::StoxLandingDataExample, 
#'       DecimalFraction = 6)
#'  sop$SopReport
#' @export
ReportFdaSOP <- function(ReportFdaCatchAtAgeData, ReportFdaWeightAtAgeData, StoxLandingData, 
                         GroupingVariables=character(), 
                         DecimalWeight=integer(), 
                         DecimalFraction=integer(), 
                         UnitFraction=RstoxData::getUnitOptions("fraction")){
  
  checkMandatory(ReportFdaCatchAtAgeData, "ReportFdaCatchAtAgeData")
  checkMandatory(ReportFdaWeightAtAgeData, "ReportFdaWeightAtAgeData")
  checkMandatory(StoxLandingData, "StoxLandingData")

  ReportFdaCatchAtAgeData$NbyAge$CatchAtAge <- RstoxData::setUnit(ReportFdaCatchAtAgeData$NbyAge$CatchAtAge, "cardinality-N")
  ReportFdaWeightAtAgeData$MeanWeightByAge$MeanIndividualWeight <- RstoxData::setUnit(ReportFdaWeightAtAgeData$MeanWeightByAge$MeanIndividualWeight, "mass-kg")
  
  DecimalWeight <- getDefault(DecimalWeight, "DecimalWeight", F, RstoxFDA::stoxFunctionAttributes$ReportFdaSOP$functionParameterDefaults$DecimalWeight)
  DecimalFraction <- getDefault(DecimalFraction, "DecimalFraction", F, RstoxFDA::stoxFunctionAttributes$ReportFdaSOP$functionParameterDefaults$DecimalFraction)
  UnitFraction <- checkOptions(UnitFraction, "UnitFraction", RstoxData::getUnitOptions("fraction"))
  
  if (length(GroupingVariables)==0){
    GroupingVariables <- NULL
  }
  
  aggVars <- GroupingVariables
  
  # set a temporary aggregationvariable if none is requested.
  if (is.null(aggVars)){
    aggVars <- c("DummyAgg")
    stopifnot(!("DummyAgg" %in% names(ReportFdaWeightAtAgeData$MeanWeightByAge)))
    stopifnot(!("DummyAgg" %in% names(ReportFdaCatchAtAgeData$NbyAge)))
    stopifnot(!("DummyAgg" %in% names(StoxLandingData$Landing)))
    ReportFdaWeightAtAgeData$MeanWeightByAge$DummyAgg <- "1"
    ReportFdaWeightAtAgeData$GroupingVariables <- data.table::data.table(GroupingVariables=c("DummyAgg", ReportFdaWeightAtAgeData$GroupingVariables$GroupingVariables))
    ReportFdaCatchAtAgeData$NbyAge$DummyAgg <- "1"
    ReportFdaCatchAtAgeData$GroupingVariables <- data.table::data.table(GroupingVariables=c("DummyAgg", ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables))
    StoxLandingData$Landing$DummyAgg <- "1"
  }
  
  msg <- "All 'GroupingVariables' must be present in both 'ReportFdaCatchAtAgeData', 'ReportFdaWeightAtAgeData' and 'StoxLandingData'"
  if (!all(aggVars %in% names(ReportFdaCatchAtAgeData$NbyAge))){
    stop(msg)
  }
  if (!all(aggVars %in% names(ReportFdaWeightAtAgeData$MeanWeightByAge))){
    stop(msg)
  }
  if (!all(aggVars %in% names(StoxLandingData$Landing))){
    stop(msg)
  }
  
  msg <- "'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData' must be decomposed on the same 'GroupingVariables'"
  
  if (!all(ReportFdaWeightAtAgeData$GroupingVariables$GroupingVariables %in% ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables)){
    stop(msg)
  }
  if (!all(ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables %in% ReportFdaWeightAtAgeData$GroupingVariables$GroupingVariables)){
    stop(msg)
  }
  
  msg <- "'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData' must be decomposed on the same age groups"
  
  if (!all(ReportFdaWeightAtAgeData$MeanWeightByAge$AgeGroup %in% ReportFdaCatchAtAgeData$NbyAge$AgeGroup)){
    stop(msg)
  }
  if (!all(ReportFdaCatchAtAgeData$NbyAge$AgeGroup %in% ReportFdaWeightAtAgeData$MeanWeightByAge$AgeGroup)){
    stop(msg)
  }

  #merge reports and estimate total for their aggragtion variables
  jointTab <- merge(ReportFdaCatchAtAgeData$NbyAge, ReportFdaWeightAtAgeData$MeanWeightByAge, by=c("Age", "AgeGroup", ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables), suffixes = c("mw", "caa"))
  stopifnot(nrow(jointTab) == nrow(ReportFdaCatchAtAgeData$NbyAge))
  
  jointTab$TotalWeightEstimated <- jointTab$CatchAtAge*jointTab$MeanIndividualWeight
  
  # aggregate on requested variables and merge with landings
  estTab <- jointTab[, list(TotalWeightEstimated=sum(get("TotalWeightEstimated"), na.rm=T)), by=aggVars]
  landTab <- StoxLandingData$Landing[, list(LandedWeight=sum(get("RoundWeight"), na.rm=T)), by=aggVars]
  
  # calculate differences
  reportTab <- merge(estTab, landTab, all=T)
  reportTab$Difference <- reportTab$TotalWeightEstimated - reportTab$LandedWeight
  reportTab$RelativeDifference <- reportTab$Difference / reportTab$LandedWeight
  
  reportTab$TotalWeightEstimated <- desimals(reportTab$TotalWeightEstimated, DecimalWeight)
  reportTab$LandedWeight <- desimals(reportTab$LandedWeight, DecimalWeight)
  reportTab$Difference <- desimals(reportTab$Difference, DecimalWeight)
  reportTab$RelativeDifference <- desimals(reportTab$RelativeDifference, DecimalFraction)
  
  reportTab$TotalWeightEstimated <- RstoxData::setUnit(reportTab$TotalWeightEstimated, "mass-kg", assertNew=T)
  reportTab$LandedWeight <- RstoxData::setUnit(reportTab$LandedWeight, "mass-kg", assertNew=T)
  reportTab$Difference <- RstoxData::setUnit(reportTab$Difference, "mass-kg", assertNew=T)
  reportTab$RelativeDifference <- RstoxData::setUnit(reportTab$RelativeDifference, "fraction-decimal", assertNew=T)
  reportTab$RelativeDifference <- RstoxData::setUnit(reportTab$RelativeDifference, RstoxData::findUnit("fraction", UnitFraction))
  
  #remove dummy aggregation variable
  if (is.null(GroupingVariables)){
    reportTab$DummyAgg <- NULL
    GroupingVariables <- character()
  }
  
  output <- list()
  output$SopReport <- reportTab
  output$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  
  return(output)
}

#' @noRd
summaryLgaPar <- function(modelFit, modelname="LengthGivenAgeModel"){
  
  summary <- NULL
  for (n in names(modelFit)){
    if (n == "LogLikelihood"){
      # include LogLikelihood ?
    }
    else {
      if (!all(names(modelFit[[n]]) %in% c("Age", "Level", "Iteration", "AgeIndex", "LevelIndex", "Slope", "tau_Slope", "ar_Slope", "Intercept", "tau_Intercept", "ar_Intercept"))){
        stop(paste("Cannot handle parameters for variable", n))
      }
      
      if (length(unique(modelFit[[n]]$AgeIndex))<=1 & length(unique(modelFit[[n]]$LevelIndex))<=1){
        modelFit[[n]]$varname <- paste(modelname, n, sep="-")
      }
      else if (length(unique(modelFit[[n]]$AgeIndex))>1 & length(unique(modelFit[[n]]$LevelIndex))<=1){
        modelFit[[n]]$varname <- paste(modelname, n, paste("Age", modelFit[[n]]$Age, sep=":"), sep="-")
      }
      else if (length(unique(modelFit[[n]]$AgeIndex))>1 & length(unique(modelFit[[n]]$LevelIndex))>1){
        modelFit[[n]]$varname <- paste(modelname, paste(n, modelFit[[n]]$Level, "Age", modelFit[[n]]$Age, sep=":"), sep="-")
      }
      else if (length(unique(modelFit[[n]]$AgeIndex))<=1 & length(unique(modelFit[[n]]$LevelIndex))>1){
        modelFit[[n]]$varname <- paste(modelname, paste(n, modelFit[[n]]$Level, sep=":"), sep="-")
      }
      else{
        stop()
      }

      if ("Intercept" %in% names(modelFit[[n]])){
        modelFit[[n]]$interceptname <- paste(modelFit[[n]]$varname, "Intercept")        
        intercept <- modelFit[[n]][, list(Mean=mean(get("Intercept")), Variance=stats::var(get("Intercept"))), by=list(Parameter=get("interceptname"))]
        summary <- rbind(summary, intercept)
      }
      if ("tau_Intercept" %in% names(modelFit[[n]])){
        modelFit[[n]]$tauinterceptname <- paste(modelFit[[n]]$varname, "tau_Intercept")   
        tau_intercept <- modelFit[[n]][, list(Mean=mean(get("tau_Intercept")), Variance=stats::var(get("tau_Intercept"))), by=list(Parameter=get("tauinterceptname"))]
        summary <- rbind(summary, tau_intercept)
      }
      if ("ar_Intercept" %in% names(modelFit[[n]])){
        modelFit[[n]]$arinterceptname <- paste(modelFit[[n]]$varname, "ar_Intercept")        
        ar_intercept <- modelFit[[n]][, list(Mean=mean(get("ar_Intercept")), Variance=stats::var(get("ar_Intercept"))), by=list(Parameter=get("arinterceptname"))]
        summary <- rbind(summary, ar_intercept)
      }
      if ("Slope" %in% names(modelFit[[n]])){
        modelFit[[n]]$slopename <- paste(modelFit[[n]]$varname, "Slope")        
        slope <- modelFit[[n]][, list(Mean=mean(get("Slope")), Variance=stats::var(get("Slope"))), by=list(Parameter=get("slopename"))]
        summary <- rbind(summary, slope)
      }
      if ("tau_Slope" %in% names(modelFit[[n]])){
        modelFit[[n]]$tauslopename <- paste(modelFit[[n]]$varname, "tau_Slope")  
        tau_slope <- modelFit[[n]][, list(Mean=mean(get("tau_Slope")), Variance=stats::var(get("tau_Slope"))), by=list(Parameter=get("tauslopename"))]
        summary <- rbind(summary, tau_slope)
      }
      if ("ar_Slope" %in% names(modelFit[[n]])){
        modelFit[[n]]$arslopename <- paste(modelFit[[n]]$varname, "ar_Slope")        
        ar_slope <- modelFit[[n]][, list(Mean=mean(get("ar_Slope")), Variance=stats::var(get("ar_Slope"))), by=list(Parameter=get("arslopename"))]
        summary <- rbind(summary, ar_slope)
      }
      
    }    
  }
  
  return(summary)
}
#' @noRd
summaryLgaCCPar <- function(modelFit){
  return(summaryLgaPar(modelFit, "LengthGivenAgeModelCC"))
}
#' @noRd
summaryWglPar <- function(modelFit){
  return(summaryLgaPar(modelFit, "WeightGivenLengthModel"))
}
#' @noRd
summaryWglCCPar <- function(modelFit){
  return(summaryLgaPar(modelFit, "WeightGivenLengthModelCC"))
}
#' @noRd
summaryPaaPar <- function(modelFit){
  return(summaryLgaPar(modelFit, "ProportionAtAgeModel"))
}


#' Report summary statistics for Reca paramters
#' @description 
#'  Reports means and variances over iterations run for Reca parameterization, which may be used as input to convergence checks.
#' @details
#'  Multiple chains may be aggregated into one summary table, by repeatedly applying this
#'  function with the aggregated result provided as the argument 'ParameterizationSummaryData'.
#'  This requires that chains are different, and that they are run for the same number of iterations.
#'  
#'  Parameters in the summary are identified with the following notation:
#'  \<model name\>-\<covariate name\>-\<any covariate value/level\>:\<any Age group\> \<parameter type\>,
#'  e.g: 'ProportionAtAgeModel-Area:47:Age:2 Intercept ' for
#'  the intercept of age 2 in area 47 in the Proportion-At-Age model.
#'  
#' @param RecaParameterData Simulated Reca parameters 
#' @param ParameterizationSummaryData summary of Reca parameters that the results should be appended to. Optional.
#' @param AppendReport if true, the results are appended to another report provided by 'ParameterizationSummaryData' 
#' @return \code{\link[RstoxFDA]{ParameterizationSummaryData}}
#' @seealso \code{\link[RstoxFDA]{ParameterizeRecaModels}} for model parameterisation
#'   \code{\link[RstoxFDA]{ReportParameterConvergence}} for convergence checks.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @concept convergence-checks
#' @export
#' @md
ReportRecaParameterStatistics <- function(RecaParameterData, ParameterizationSummaryData, AppendReport=FALSE){
  
  checkMandatory(RecaParameterData, "RecaParameterData")

  if (AppendReport){
    if (!isGiven(ParameterizationSummaryData)){
      stop("Need to provide 'ParameterizationSummaryData' when 'AppendReport' is TRUE")
    }
  }
  
  if (!AppendReport){
    ParameterizationSummaryData <- NULL
  }
  chainId <- RecaParameterData$GlobalParameters$GlobalParameters$resultdir
  iterations <- nrow(RecaParameterData$FitProportionAtAge$LogLikelihood)
  
  output <- list()
  output$ParameterSummary <- summaryPaaPar(RecaParameterData$FitProportionAtAge)
  output$ParameterSummary$chainId <- chainId
  
  LengthGivenAge <- summaryLgaPar(RecaParameterData$FitLengthGivenAge)
  LengthGivenAge$chainId <- chainId
  output$ParameterSummary <- rbind(output$ParameterSummary, LengthGivenAge)
  
  WeightGivenLength <- summaryWglPar(RecaParameterData$FitWeightGivenLength)
  WeightGivenLength$chainId <- chainId
  output$ParameterSummary <- rbind(output$ParameterSummary, WeightGivenLength)
  
  if ("FitWeightGivenLengthCC" %in% names(RecaParameterData)){
    WeightGivenLengthCC <- summaryWglCCPar(RecaParameterData$FitWeightGivenLengthCC)
    WeightGivenLengthCC$chainId <- chainId
    output$ParameterSummary <- rbind(output$ParameterSummary, WeightGivenLengthCC)
  }
  if ("FitLengthGivenAgeCC" %in% names(RecaParameterData)){
    LengthGivenAgeCC <- summaryLgaCCPar(RecaParameterData$FitLengthGivenAgeCC)
    LengthGivenAgeCC$chainId <- chainId
    output$ParameterSummary <- rbind(output$ParameterSummary, LengthGivenAgeCC)
  }
  
  output$RunParameters <- data.table::data.table(chainId=chainId, Iterations=iterations)
  
  if (!is.null(ParameterizationSummaryData)){
    if (any(chainId %in% ParameterizationSummaryData$ProportionAtAge$chainId) |
            any(chainId %in% ParameterizationSummaryData$LengthGivenAge$chainId) |
            any(chainId %in% ParameterizationSummaryData$WeightGivenLength$chainId)){
      stop("Cannot append summary statistic to parameterizations with the same chain ID. Different chains should be run in different directories (argument 'ResultDirectory' in 'ParameterizeRecaModels')")
    }
    
    output$ParameterSummary <- rbind(output$ParameterSummary, ParameterizationSummaryData$ParameterSummary)
    output$RunParameters <- rbind(output$RunParameters, ParameterizationSummaryData$RunParameters)
    
  }
  return(output)
}

#' @noRd
crossChainConvergence <- function(modelSummary, iterations, tolerance){
  nchains <- length(unique(modelSummary$chainId))
  if (nchains<2){
    stop("Need at least three chains to compute convergence statistics.")
  }
  
  allChainMean <- modelSummary[,list(AllChainMain=mean(get("Mean"))), by=list(Parameter=get("Parameter"))]
  modelSummary <- merge(modelSummary, allChainMean, all.x=T)
  #squared diff of chain means to joint mean
  modelSummary$sqDiff <- (modelSummary$Mean - modelSummary$AllChainMain)**2
  
  #B/n
  betweenChainVar <- modelSummary[,list(InterVariance=sum(get("sqDiff"))/(nchains-1)), by=list(Parameter=get("Parameter"))]
  #W
  withinChainVar <- modelSummary[,list(IntraVariance=mean(get("Variance"))), by=list(Parameter=get("Parameter"))]
  
  tab <- merge(betweenChainVar, withinChainVar)
  
  #sqrt(W(n-1)/n + B(m+1)/mn)
  tab$GelmanRubinR <- sqrt((tab$IntraVariance * (iterations-1) / iterations +  tab$InterVariance * (nchains+1)/nchains) / tab$IntraVariance)
  
  tab <- tab[abs(tab$GelmanRubinR - 1) >= tolerance]
  
  tab <- tab[order(abs(tab$GelmanRubinR - 1), decreasing = T),]
  
  return(tab)
}

#' Report convergence Statistics for parameter simulations
#' @description 
#'  For multi-chain parameter simulations, intra-chain variance, inter-chain variance
#'  and the Gelman-Rubins R statistic is reported.
#'  
#'  'chains' in this respect refers to statistically independent simulations.
#' @details 
#'  Only parameters with a Gelman-Rubins R statistic that deviates sufficiently from 1
#'  is reported. The accepted deviation is controlled by the argument 'Tolerance'.
#'  Parameters are reported if they differ from 1 by 'Tolerance' or more,
#'  so that a Tolerance of 0 reports all parameters.
#'  If not provided a default Tolerance of 0.1 is used.
#'  
#'  Gelman-Rubins R is described in Gelman and Rubin (Statistical Science, 1992):
#'  DOI: https://doi.org/10.1214/ss/1177011136
#'  
#'  The Gelman-Rubins R reported here is approximately that specified in 
#'  Gelman and Rubins Eq. 20, ignoring the contribution from the factor (df/(df-2)).
#'  
#'  In the report InterVariance correspond to their B/n and
#'  IntraVariance correspond to their W.
#' @param ParameterizationSummaryData summary statistics for Reca parameters
#' @param Tolerance threshold for reporting parameters. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportParameterConvergence$functionParameterDefaults$Tolerance`. See details
#' @param Decimals integer specifying the number of decimals to report for 'GelmanRubinR'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportParameterConvergence$functionParameterDefaults$Decimals`.
#' @return \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @export
#' @concept StoX-functions
#' @concept convergence-checks
#' @md
ReportParameterConvergence <- function(ParameterizationSummaryData, Tolerance=numeric(), Decimals=integer()){
  
  checkMandatory(ParameterizationSummaryData, "ParameterizationSummaryData")

  Tolerance <- getDefault(Tolerance, "Tolerance", F, RstoxFDA::stoxFunctionAttributes$ReportParameterConvergence$functionParameterDefaults$Tolerance)
  Decimals <- getDefault(Decimals, "Decimals", F, RstoxFDA::stoxFunctionAttributes$ReportParameterConvergence$functionParameterDefaults$Decimals)
  
  
  if (length(unique(ParameterizationSummaryData$RunParameters$Iterations))!=1){
    stop("All chains must be run for the same number of iterations")
  }
  iterations <- ParameterizationSummaryData$RunParameters$Iterations[1]
  
  output <- list()
  output$ConvergenceReport <- crossChainConvergence(ParameterizationSummaryData$ParameterSummary, iterations, Tolerance)

  output$ConvergenceReport$GelmanRubinR <- desimals(output$ConvergenceReport$GelmanRubinR, Decimals)
  
  return(output)
}
