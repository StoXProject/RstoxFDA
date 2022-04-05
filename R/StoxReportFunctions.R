#' Rounds the specified number of decimals
#' @noRd
desimals <- function(x, Decimals=integer()){
  
  if (isGiven(Decimals)){
    return(round(x, digits = Decimals))
  }
  
  return(x)
  
}

#' @noRd
setDecimals <- function(table, columns, decimals){
  for (co in columns){
    table[[co]] <- desimals(table[[co]], decimals)
  }
  return(table)
}

#' modifies unit by reference (note: no return value)
#' @noRd
setUnits <- function(table, columns, unit, quantity){
  for (co in columns){
    table[[co]] <- RstoxData::setUnit(table[[co]], RstoxData::findUnit(quantity, unit))
  }
  return(table)
}

#' Report FDA sampling
#' @description 
#'  Report sampling of fisheries against landings in partitions of the fisheries.
#' @details 
#'  Sampling is reported partitioned on the provided 'GroupingVariables'.
#'  If samples are encoded in partitions of the fisheries with no landings. 'LandedRoundWeight' will be NA.
#'  This may be due to recording errors or filtering errors, but it may also be due to comparison of similar
#'  but unequal category-definitios. For instance area are coded in landings as dominant area for a fishing trip,
#'  while at-sea sampling will record area of fishing operation, and the catch from that area by subsequently be landed
#'  with another area listed as dominant area.
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
#' @param Decimals integer specifying the number of decimals to report for 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to zero.
#' @param Unit unit for the weights 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to 'kg'
#' @return \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @export
#' @md
ReportFdaSampling <- function(StoxBioticData, StoxLandingData, GroupingVariables=character(), Decimals=integer(), Unit=RstoxData::getUnitOptions("mass", conversionRange=c(1,1e12))){
  
  if (!isGiven(StoxBioticData)){
    stop("Parameter 'StoxBioticData' must be provided")
  }
  if (!isGiven(StoxLandingData)){
    stop("Parameter 'StoxLandingData' must be provided")
  }
  
  if (!isGiven(Decimals)){
    Decimals=0
  }

  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("mass"))){
      stop(paste(Unit, "is not a recognized unit for mass / weight."))
    }
  }
  
  flatlandings <- StoxLandingData$Landing
  flatbiotic <- RstoxData::MergeStoxBiotic(StoxBioticData)
  
  if (!isGiven(GroupingVariables)){
    GroupingVariables <- c("Segment")
    flatlandings$Segment <- "All landings"
    flatbiotic$Segment <- "All landings"
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

  samples <- flatbiotic[,c(GroupingVariables, "IndividualRoundWeight", "IndividualAge", "IndividualTotalLength", "CatchFractionWeight", "CatchPlatform", "StationKey", "IndividualKey", "Sample"), with=F]
  
  sampledTab <- samples[,list(Catches=length(unique(get("StationKey")))), by=GroupingVariables]
  vessels <- samples[,list(Vessels=length(unique(get("CatchPlatform")))), by=GroupingVariables]
  sampledTab <- merge(sampledTab, vessels, by=GroupingVariables)
  weights <- samples[,list(WeightMeasurments=sum(!is.na(get("IndividualRoundWeight")))),by=GroupingVariables]
  sampledTab <- merge(sampledTab, weights, by=GroupingVariables)
  lengths <- samples[,list(LengthMeasurments=sum(!is.na(get("IndividualTotalLength")))),by=GroupingVariables]
  sampledTab <- merge(sampledTab, lengths, by=GroupingVariables)
  ages <- samples[,list(AgeReadings=sum(!is.na(get("IndividualAge")))),by=GroupingVariables]
  sampledTab <- merge(sampledTab, ages, by=GroupingVariables)
  sampledWeights <- samples[,list(WeightOfSampledCatches=sum(get("CatchFractionWeight")[!duplicated(get("Sample"))], na.rm=T)), by=GroupingVariables]
  sampledTab <- merge(sampledTab, sampledWeights, by=GroupingVariables)
  
  landingsAggList <- list()
  for (v in GroupingVariables){
    landingsAggList[[v]] <- flatlandings[[v]]
  }
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
#' @param Decimals integer specifying the number of decimals to report for 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to zero.
#' @param Unit unit for the weights 'LandedRoundWeight' and 'WeightOfSampledCatches'. Defaults to 'kg'
#' @return \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @export
#' @md
ReportFdaLandings <- function(StoxLandingData, GroupingVariables=character(), Decimals=integer(), Unit=RstoxData::getUnitOptions("mass", conversionRange=c(1,1e12))){
  
  if (!isGiven(StoxLandingData)){
    stop("Parameter 'StoxLandingData' must be provided")
  }
  
  if (!isGiven(Decimals)){
    Decimals=0
  }
  
  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("mass"))){
      stop(paste(Unit, "is not a recognized unit for mass / weight."))
    }
  }
  
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
      stoxWarning("Length interval is specified lower than the available resolution.")
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
#'  The units considered valid for catch at age in nnumbers are those listed for quantity 'cardinaltiy' in \code{\link[RstoxData]{StoxUnits}}
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtAge', 'SD', 'Low' and 'High'. Defaults to zero.
#' @param Unit unit for 'CatchAtAge', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtLength}} for reporting length composition.
#' @export
#' @md
ReportRecaCatchAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  
  if (!isGiven(RecaCatchAtAge)){
    stop("Parameter 'RecaCatchAtAge' must be provided")
  }
  
  if (!isGiven(Decimals)){
    Decimals=0
  }
  
  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("cardinality"))){
      stop(paste(Unit, "is not a recognized unit catch in numbers."))
    }
  }
  
  
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }
  
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
#' @param Decimals integer specifying the number of decimals to report for 'Covariance'. Defaults to zero.
#' @param Unit unit for 'CatchAtAge'. Covariance will be provided as the square of this unit.
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeCovarianceData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtLength}} for reporting length composition.
#' @export
#' @md
ReportRecaCatchAtAgeCovariance <- function(RecaCatchAtAge, PlusGroup=integer(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  
  if (!isGiven(RecaCatchAtAge)){
    stop("Parameter 'RecaCatchAtAge' must be provided")
  }
  
  
  if (!isGiven(Decimals)){
    Decimals=0
  }
  
  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("cardinality"))){
      stop(paste(Unit, "is not a recognized unit catch in numbers."))
    }
  }
  
  
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
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
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtLength', 'SD', 'Low' and 'High'. Defaults to zero.
#' @param Unit unit for 'CatchAtLength', 'SD', 'Low' and 'High'
#' @param LengthInterval width of length bins in cm. If not provided, the interval in 'RecaCatchAtAge' will be used.
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for reporting age composition
#' @export
#' @md
ReportRecaCatchAtLength <- function(RecaCatchAtAge, IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12)), LengthInterval=numeric()){
  
  if (!isGiven(RecaCatchAtAge)){
    stop("Parameter 'RecaCatchAtAge' must be provided")
  }
  if (!isGiven(LengthInterval)){
    LengthInterval <- NULL
  }
  
  if (!isGiven(Decimals)){
    Decimals=0
  }
  
  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("cardinality"))){
      stop(paste(Unit, "is not a recognized unit catch in numbers."))
    }
  }
  
  
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }
  
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

  caa$NbyLength <- setUnits(caa$NbyLength, "Length", "cm", "length")
  caa$NbyLength <- setUnits(caa$NbyLength, c("CatchAtLength", "SD", "Low", "High"), "individuals", "cardinality")
  if (isGiven(Unit)){
    caa$NbyLength <- setUnits(caa$NbyLength, c("CatchAtLength", "SD", "Low", "High"), Unit, "cardinality")  
  }
  
  if (isGiven(Decimals)){
    caa$NbyLength <- setDecimals(caa$NbyLength, c("CatchAtLength", "SD", "Low", "High"), Decimals)
  }
  
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
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtAge', 'SD', 'Low' and 'High'. Defaults to zero.
#' @param Unit unit for 'CatchAtAge', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtLengthAndAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis and \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for reporting age composition
#' @export
#' @md
ReportRecaCatchAtLengthAndAge <- function(RecaCatchAtAge, 
                                          PlusGroup=integer(), 
                                          LengthInterval=numeric(), 
                                          IntervalWidth=numeric(), 
                                          Decimals=integer(), 
                                          Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  
  if (!isGiven(RecaCatchAtAge)){
    stop("Parameter 'RecaCatchAtAge' must be provided")
  }
  if (!isGiven(LengthInterval)){
    LengthInterval <- NULL
  }
  
  if (!isGiven(Decimals)){
    Decimals=0
  }
  
  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("cardinality"))){
      stop(paste(Unit, "is not a recognized unit catch in numbers."))
    }
  }
  
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }
  
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
  if (isGiven(Unit)){
    caa$FdaReport <- setUnits(caa$FdaReport, c("CatchAtAgeLength", "SD", "Low", "High"), Unit, "cardinality")  
  }
  
  if (isGiven(Decimals)){
    caa$FdaReport <- setDecimals(caa$FdaReport, c("CatchAtAgeLength", "SD", "Low", "High"), Decimals)
  }
  
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
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @param Decimals integer specifying the number of decimals to report for 'MeanIndividualWeight', 'SD', 'Low' and 'High'. Defaults to 2.
#' @param Threshold threshold for reporting mean weight. Rows with an estimated Catch At Age (number of individuals) lower than this will have NA reported for their mean weight. Defaults to 0.
#' @param Unit unit for 'MeanIndividualWeight', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @export
#' @md
ReportRecaWeightAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Threshold=numeric(), Unit=RstoxData::getUnitOptions("mass", conversionRange=c(1e-4, 10))){
  
  if (!isGiven(RecaCatchAtAge)){
    stop("Parameter 'RecaCatchAtAge' must be provided")
  }
  if (!isGiven(PlusGroup)){
    PlusGroup <- NULL
  }
  
  
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  if (!isGiven(Decimals)){
    Decimals=2
  }
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }

  if (!isGiven(Threshold)){
    Threshold = 0
  }

  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("mass"))){
      stop(paste(Unit, "is not a recognized unit for mass / weight."))
    }
  }
    
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
  if (isGiven(Unit)){
    mwaa$FdaReport <- setUnits(mwaa$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), Unit, "mass")  
  }
  
  if (isGiven(Decimals)){
    mwaa$FdaReport <- setDecimals(mwaa$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), Decimals)
  }
  
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
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @param Decimals integer specifying the number of decimals to report for 'MeanIndividualLength', 'SD', 'Low' and 'High'. Defaults to 1.
#' @param Threshold threshold for reporting mean weight. Rows with an estimated Catch At Age (number of individuals) lower than this will have NA reported for their mean length Defaults to 0.
#' @param Unit unit for 'MeanIndividualLength', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaLengthAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @export
ReportRecaLengthAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Threshold=numeric(), Unit=RstoxData::getUnitOptions("length", conversionRange=c(1e-7, 10))){
  if (!isGiven(RecaCatchAtAge)){
    stop("Parameter 'RecaCatchAtAge' must be provided")
  }
  if (!isGiven(PlusGroup)){
    PlusGroup <- NULL
  }
  
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  if (!isGiven(Decimals)){
    Decimals=1
  }
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }
  
  if (!isGiven(Threshold)){
    Threshold = 0
  }
  
  if (isGiven(Unit)){
    Unit <- Unit[1]
    if (!(Unit %in% RstoxData::getUnitOptions("length"))){
      stop(paste(Unit, "is not a recognized unit for length."))
    }
  }
  
  
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
  if (isGiven(Unit)){
    mla$FdaReport <- setUnits(mla$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), Unit, "length")  
  }
  
  if (isGiven(Decimals)){
    mla$FdaReport <- setDecimals(mla$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), Decimals)
  }
  
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
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @param DecimalOptions logical determining whether the StoX user interface should show decimal options
#' @param DecimalTotalNumber integer specifying the number of decimals to report for 'TotalNumber', and the corresponding 'SD', 'Low' and 'High'. to 0
#' @param DecimalTotalWeight integer specifying the number of decimals to report for 'TotalWeightDefaults', and the corresponding 'SD', 'Low' and 'High'. to 0
#' @param DecimalMeanAge integer specifying the number of decimals to report for 'MeanIndividualAge', and the corresponding 'SD', 'Low' and 'High'. Defaults to 1
#' @param DecimalMeanWeight integer specifying the number of decimals to report for 'MeanIndividualWeight', and the corresponding 'SD', 'Low' and 'High'. Defaults to 3
#' @param DecimalMeanLength integer specifying the number of decimals to report for 'MeanIndividualLength', and the corresponding 'SD', 'Low' and 'High'. Defaults to 2
#' @param UnitOptions logical determining whether the StoX user interface should show unit options
#' @param UnitTotalNumber unit for total catch in numbers. Defaults to Mi (millions)
#' @param UnitTotalWeight unit for weight of total catch. Defaults to kt
#' @param UnitMeanWeight unit for mean weight. Defaults to kg
#' @param UnitMeanLength unit for mean length. Defaults to cm.
#' @return \code{\link[RstoxFDA]{ReportFdaSummaryData}}
#' @seealso 
#'  \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for reporting catch, mean length and mean weight by age groups
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtLength}} for reporting length distributions
#'  \code{\link[RstoxFDA]{ReportRecaCatchAtLengthAndAge}} for reporting catch by age-group and length-group combinations.
#' @export
#' @md
ReportRecaCatchStatistics <- function(RecaCatchAtAge, IntervalWidth=numeric(), 
                                      DecimalOptions=FALSE, 
                                      DecimalTotalNumber=integer(), 
                                      DecimalTotalWeight=integer(), 
                                      DecimalMeanAge=integer(), 
                                      DecimalMeanWeight=integer(), 
                                      DecimalMeanLength=integer(), 
                                      UnitOptions=FALSE, 
                                      UnitTotalNumber=RstoxData::getUnitOptions("cardinality", conversionRange=c(1, 1e12)), 
                                      UnitTotalWeight=RstoxData::getUnitOptions("mass", conversionRange=c(1, 1e12)), 
                                      UnitMeanWeight=RstoxData::getUnitOptions("mass", conversionRange=c(1e-4, 10)), 
                                      UnitMeanLength=RstoxData::getUnitOptions("length", conversionRange=c(1e-4, 10))){
  
  if (!isGiven(RecaCatchAtAge)){
    stop("Parameter 'RecaCatchAtAge' must be provided")
  }
  if (!isGiven(IntervalWidth)){
    IntervalWidth <- 0.9
  }
  
  if (!isGiven(DecimalTotalNumber)){
    DecimalTotal=0  
  }
  if (!isGiven(DecimalTotalWeight)){
    DecimalTotalWeight=0
  }
  if (!isGiven(DecimalMeanAge)){
    DecimalMeanAge=1
  }
  if (!isGiven(DecimalMeanWeight)){
    DecimalMeanWeight=3
  }
  if (!isGiven(DecimalMeanLength)){
    DecimalMeanLength=2
  }
  
  if (isGiven(UnitMeanLength)){
    UnitMeanLength <- UnitMeanLength[1]
    if (!(UnitMeanLength %in% RstoxData::getUnitOptions("length"))){
      stop(paste(UnitMeanLength, "is not a recognized unit for length."))
    }
  }
  
  if (isGiven(UnitMeanWeight)){
    UnitMeanWeight <- UnitMeanWeight[1]
    if (!(UnitMeanWeight %in% RstoxData::getUnitOptions("mass"))){
      stop(paste(UnitMeanWeight, "is not a recognized unit for mass / weight."))
    }
  }
  
  if (isGiven(UnitTotalWeight)){
    UnitTotalWeight <- UnitTotalWeight[1]
    if (!(UnitTotalWeight %in% RstoxData::getUnitOptions("mass"))){
      stop(paste(UnitTotalWeight, "is not a recognized unit for mass / weight."))
    }
  }
  
  if (isGiven(UnitTotalNumber)){
    UnitTotalNumber <- UnitTotalNumber[1]
    if (!(UnitTotalNumber %in% RstoxData::getUnitOptions("cardinality"))){
      stop(paste(UnitTotalNumber, "is not a recognized unit for catch in numbers."))
    }
  }
  
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
  
  
  if (isGiven(DecimalMeanAge)){
    meanAge$FdaReport <- setDecimals(meanAge$FdaReport, c("MeanIndividualAge", "SD", "Low", "High"), DecimalMeanAge)
  }
  if (isGiven(UnitAge)){
    meanAge$FdaReport <- setUnits(meanAge$FdaReport, c("MeanIndividualAge", "SD", "Low", "High"), UnitAge, "age")    
  }
  
  # mean weight
  mcw <- getPlusGroupMeans(RecaCatchAtAge, "MeanWeight", "MeanIndividualWeight",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  meanWeight <- reportParameterAtAge(mcw, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualWeight", alpha = 1 - IntervalWidth)
  meanWeight$FdaReport$AgeGroup <- NULL
  meanWeight$FdaReport$Age <- NULL
  
  if (isGiven(DecimalMeanWeight)){
    meanWeight$FdaReport <- setDecimals(meanWeight$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), DecimalMeanWeight)
  }
  
  meanWeight$FdaReport <- setUnits(meanWeight$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), "kg", "mass")
  if (isGiven(UnitMeanWeight)){
    meanWeight$FdaReport <- setUnits(meanWeight$FdaReport, c("MeanIndividualWeight", "SD", "Low", "High"), UnitMeanWeight, "mass")
  }
  
  # mean length
  mlw <- getPlusGroupMeans(RecaCatchAtAge, "MeanLength", "MeanIndividualLength",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  meanLength <- reportParameterAtAge(mlw, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualLength", alpha = 1 - IntervalWidth)
  meanLength$FdaReport$AgeGroup <- NULL
  meanLength$FdaReport$Age <- NULL
  
  if (isGiven(DecimalMeanLength)){
    meanLength$FdaReport <- setDecimals(meanLength$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), DecimalMeanLength)
  }
  
  meanLength$FdaReport <- setUnits(meanLength$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), "cm", "length")
  if (isGiven(UnitMeanWeight)){
    meanLength$FdaReport <- setUnits(meanLength$FdaReport, c("MeanIndividualLength", "SD", "Low", "High"), UnitMeanLength, "length")
  }
  
  
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
  
  if (isGiven(UnitTotalWeight)){
    TotalWeight$NbyAge <- setUnits(TotalWeight$NbyAge, c("TotalWeight", "SD", "Low", "High"), UnitTotalWeight, "mass")
  }
  if (isGiven(DecimalTotalWeight)){
    TotalWeight$NbyAge <- setDecimals(TotalWeight$NbyAge, c("TotalWeight", "SD", "Low", "High"), DecimalTotalWeight)
  }
  
  
  # total number
  TotalNumber<-ReportRecaCatchAtAge(RecaCatchAtAge, PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age), Unit = "individuals")
  names(TotalNumber$NbyAge)[names(TotalNumber$NbyAge)=="CatchAtAge"] <- "TotalNumber"
  TotalNumber$NbyAge$AgeGroup <- NULL
  TotalNumber$NbyAge$Age <- NULL
  
  if (isGiven(UnitTotalNumber)){
    TotalNumber$NbyAge <- setUnits(TotalNumber$NbyAge, c("TotalNumber", "SD", "Low", "High"), UnitTotalNumber, "cardinality")
  }
  if (isGiven(DecimalTotalNumber)){
    TotalNumber$NbyAge <- setDecimals(TotalNumber$NbyAge, c("TotalNumber", "SD", "Low", "High"), DecimalTotalNumber)
  }
  
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
#' @param DecimalWeight integer specifying the number of decimals to report for weights: 'TotalWeightEstimated', 'LandedWeight', and 'Difference'. Defaults to 0
#' @param DecimalFraction integer specifying the number of decimals to report for 'RelativeDifference'. Defaults to 3.
#' @param UnitFraction unit for the RelativeDifference. E.g. '0.' for decimal notation or '\%' for percent.
#' @return \code{\link[RstoxFDA]{ReportFdaSopData}}
#' @seealso 
#'  \code{\link[RstoxFDA]{ReportRecaWeightAtAge}} and \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for some ways of preparing 'ReportFdaWeightAtAgeData' and 'ReportFdaCatchAtAgeData'.
#'  \code{\link[RstoxData]{StoxLanding}} and \code{\link[RstoxData]{FilterStoxLanding}} for ways of preparing 'StoxLandingData'.
#' @export
ReportFdaSOP <- function(ReportFdaCatchAtAgeData, ReportFdaWeightAtAgeData, StoxLandingData, 
                         GroupingVariables=character(), 
                         DecimalWeight=integer(), 
                         DecimalFraction=integer(), 
                         UnitFraction=RstoxData::getUnitOptions("fraction")){
  
  if (!isGiven(ReportFdaCatchAtAgeData)){
    stop("Parameter 'ReportFdaCatchAtAgeData' must be provided")
  }
  if (!isGiven(ReportFdaWeightAtAgeData)){
    stop("Parameter 'ReportFdaWeightAtAgeData' must be provided")
  }
  if (!isGiven(StoxLandingData)){
    stop("Parameter 'StoxLandingData' must be provided")
  }
  
  ReportFdaCatchAtAgeData$NbyAge$CatchAtAge <- RstoxData::setUnit(ReportFdaCatchAtAgeData$NbyAge$CatchAtAge, "cardinality-N")
  ReportFdaWeightAtAgeData$MeanWeightByAge$MeanIndividualWeight <- RstoxData::setUnit(ReportFdaWeightAtAgeData$MeanWeightByAge$MeanIndividualWeight, "mass-kg")
  
  if (!isGiven(DecimalWeight)){
    DecimalWeight = 0
  }
  if (!isGiven(DecimalFraction)){
    DecimalFraction = 3
  }
  
  if (length(GroupingVariables)==0){
    GroupingVariables <- NULL
  }
  
  if (isGiven(UnitFraction)){
    UnitFraction <- UnitFraction[1]
    if (!(UnitFraction %in% RstoxData::getUnitOptions("fraction"))){
      stop(paste(UnitFraction, "is not a recognized unit for fraction."))
    }
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
#' @export
#' @md
ReportRecaParameterStatistics <- function(RecaParameterData, ParameterizationSummaryData, AppendReport=FALSE){
  
  if (!isGiven(RecaParameterData)){
    stop("Parameter 'RecaParameterData' must be provided")
  }
  
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
#' @param Tolerance threshold for reporting parameters. Defaults to 0.1. See details
#' @param Decimals integer specifying the number of decimals to report for 'GelmanRubinR'. Defaults to 2
#' @return \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @export
#' @md
ReportParameterConvergence <- function(ParameterizationSummaryData, Tolerance=numeric(), Decimals=integer()){
  
  if (!isGiven(ParameterizationSummaryData)){
    stop("Parameter 'ParameterizationSummaryData' must be provided")
  }

  if (!isGiven(Tolerance)){
    Tolerance = 0.1
  }
  
  if (!isGiven(Decimals)){
    Decimals = 2
  }
  
  if (length(unique(ParameterizationSummaryData$RunParameters$Iterations))!=1){
    stop("All chains must be run for the same number of iterations")
  }
  iterations <- ParameterizationSummaryData$RunParameters$Iterations[1]
  
  output <- list()
  output$ConvergenceReport <- crossChainConvergence(ParameterizationSummaryData$ParameterSummary, iterations, Tolerance)

  output$ConvergenceReport$GelmanRubinR <- desimals(output$ConvergenceReport$GelmanRubinR, Decimals)
  
  return(output)
}
