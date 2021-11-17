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
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries
#'  and approriate columns added for identifying corresponding landings.
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns added for identifying corresponding samples
#' @param GroupingVariables Columns of 'StoxBioticData' and 'StoxLandingData' that partitions the fisheries. Defaults to all column names that are found in both inputs.
#' @return \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @export
ReportFdaSampling <- function(StoxBioticData, StoxLandingData, GroupingVariables=character()){
  
  flatlandings <- StoxLandingData$Landing
  flatbiotic <- RstoxData::MergeStoxBiotic(StoxBioticData)
  
  if (!isGiven(GroupingVariables)){
    GroupingVariables <- names(flatbiotic)[names(flatbiotic) %in% names(flatlandings)]
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
  
  
  bioticAggList <- list()
  for (v in GroupingVariables){
    bioticAggList[[v]] <- flatbiotic[[v]]
  }

  samples <- flatbiotic[,c(GroupingVariables, "IndividualRoundWeight", "IndividualAge", "IndividualTotalLength", "CatchFractionWeight", "CatchPlatform", "StationKey", "IndividualKey"), with=F]
  sampledTab <- data.table::data.table(stats::aggregate(list(Catches=samples$StationKey), by=bioticAggList, FUN=function(x){length(unique(x))}))
  vessels <- data.table::data.table(stats::aggregate(list(Vessels=samples$CatchPlatform), by=bioticAggList, FUN=function(x){length(unique(x))}))
  sampledTab <- merge(sampledTab, vessels, by=GroupingVariables)
  weights <- data.table::data.table(stats::aggregate(list(WeightMeasurments=samples$IndividualRoundWeight), by=bioticAggList, FUN=function(x){sum(!is.na(x))}))
  sampledTab <- merge(sampledTab, weights, by=GroupingVariables)
  lengths <- data.table::data.table(stats::aggregate(list(LengthMeasurments=samples$IndividualTotalLength), by=bioticAggList, FUN=function(x){sum(!is.na(x))}))
  sampledTab <- merge(sampledTab, lengths, by=GroupingVariables)
  ages <- data.table::data.table(stats::aggregate(list(AgeReadings=samples$IndividualAge), by=bioticAggList, FUN=function(x){sum(!is.na(x))}))
  sampledTab <- merge(sampledTab, ages, by=GroupingVariables)
  sampledWeights <- data.table::data.table(stats::aggregate(list(WeightOfSampledCatches=samples$CatchFractionWeight), by=bioticAggList, FUN=function(x){sum(x, na.rm=T)}))
  sampledTab <- merge(sampledTab, sampledWeights, by=GroupingVariables)
  
  landingsAggList <- list()
  for (v in GroupingVariables){
    landingsAggList[[v]] <- flatlandings[[v]]
  }
  landings <- flatlandings[,c(GroupingVariables, "RoundWeight"), with=F]
  landingsTab <- data.table::data.table(stats::aggregate(list(LandedRoundWeight=landings$RoundWeight), by=landingsAggList, FUN=function(x){sum(x, na.rm=T)}))
  
  tab <- merge(landingsTab, sampledTab, by=GroupingVariables, all=T)
  tab <- tab[order(tab$LandedRoundWeight),]
  
  output <- list()
  output$FisheriesSampling <- tab
  output$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  
  return(output)
}

#' @noRd
reportParameterAtAge <- function(table, aggVariables, parameter, digits=2, alpha=.1){
  
  stopifnot(all(c("Age", "AgeGroup") %in% names(table)))
  
  output <- list()
  
  aggNames <- aggVariables
  aggNames <- c("Age", "AgeGroup", aggNames)
  stopifnot(length(aggNames) == (ncol(table)-2))

  result <- table[,list(par=round(mean(get(parameter)), digits=digits), SD=round(stats::sd(get(parameter)), digits=digits), Low=round(stats::quantile(get(parameter), probs = alpha/2.0), digits=digits), High=round(stats::quantile(get(parameter), probs = 1-(alpha/2.0)), digits=digits)), by=aggNames]
  
  data.table::setcolorder(result ,c("AgeGroup", "Age", "par", "SD", "Low", "High", aggVariables))
  names(result) <- c("AgeGroup", "Age", parameter, "SD", "Low", "High", aggVariables)
  data.table::setorderv(result, c(aggVariables, "Age"))
  
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

#' Report catch at age
#' @description 
#'  Tabulates summary statistics for total catch at age from MCMC simulations using Reca.
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @export
ReportRecaCatchAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric()){
  
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
  
  return(reportParameterAtAge(totalOverLength, aggNames, "CatchAtAge", alpha = 1-IntervalWidth))
  
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
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as 90% equal-tailed credible intervals.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @return \code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @export
ReportRecaWeightAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric()){
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }

  meanWeightAtAge <- getPlusGroupMeans(RecaCatchAtAge, "MeanWeight", "MeanIndividualWeight", PlusGroup)
  return(reportParameterAtAge(meanWeightAtAge, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualWeight", alpha = 1 - IntervalWidth, digits=3))

}

#' Report length at age
#' @description 
#'  Tabulates summary statistics for mean length at age from MCMC simulations using Reca.
#'  MCMC simulations are typically obtained with \code{\link[RstoxFDA]{RunRecaModels}}.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals are reported.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @return \code{\link[RstoxFDA]{ReportFdaLengthAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @export
ReportRecaLengthAtAge <- function(RecaCatchAtAge, PlusGroup=integer(), IntervalWidth=numeric()){
  stopifnot(is.RecaCatchAtAge(RecaCatchAtAge))
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }
  
  meanLengthAtAge <- getPlusGroupMeans(RecaCatchAtAge, "MeanLength", "MeanIndividualLength", PlusGroup)
  
  return(reportParameterAtAge(meanLengthAtAge, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualLength", alpha = 1 - IntervalWidth, digits=2))
}

#' Report catch statistics
#' @description 
#'  Report summary statistics for landed catches from MCMC simulations using Reca.
#'  Summary statistics are obtained from the posterior distribution, and
#'  the interval is reported as equal-tailed credible intervals are reported.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @return \code{\link[RstoxFDA]{ReportFdaCatchData}}
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @export
ReportRecaCatchStatistics <- function(RecaCatchAtAge, IntervalWidth=numeric()){
  
  if (length(IntervalWidth) == 0){
    IntervalWidth <- 0.9
  }
  
  # get mean catch statistics by collapsing to a singe plusgroup
  
  # mean age
  # hack caa table so that it fits getPLusGroupMeans
  RecaCatchAtAge$MeanAge <- RecaCatchAtAge$CatchAtAge
  RecaCatchAtAge$MeanAge$MeanIndividualAge <- RecaCatchAtAge$CatchAtAge$Age
  RecaCatchAtAge$MeanAge$CatchAtAge <- NULL
  mca <- getPlusGroupMeans(RecaCatchAtAge, "MeanAge", "MeanIndividualAge",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  
  meanAge <- reportParameterAtAge(mca, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualAge", alpha = 1 - IntervalWidth, digits=2)
  meanAge$FdaReport$AgeGroup <- NULL
  meanAge$FdaReport$Age <- NULL
  
  
  # mean weight
  mcw <- getPlusGroupMeans(RecaCatchAtAge, "MeanWeight", "MeanIndividualWeight",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  meanWeight <- reportParameterAtAge(mcw, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualWeight", alpha = 1 - IntervalWidth, digits=2)
  meanWeight$FdaReport$AgeGroup <- NULL
  meanWeight$FdaReport$Age <- NULL
  
  # mean length
  mlw <- getPlusGroupMeans(RecaCatchAtAge, "MeanLength", "MeanIndividualLength",PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  meanLength <- reportParameterAtAge(mlw, RecaCatchAtAge$GroupingVariables$GroupingVariables, "MeanIndividualLength", alpha = 1 - IntervalWidth, digits=2)
  meanLength$FdaReport$AgeGroup <- NULL
  meanLength$FdaReport$Age <- NULL
  
  # combine
  output <- list()
  output$MeanAge <- meanAge$FdaReport
  output$MeanWeight <- meanWeight$FdaReport
  output$MeanLength <- meanLength$FdaReport
  output$GroupingVariables <- meanAge$GroupingVariables
  
  return(output)
}

#' Report SOP test
#' @description 
#'  Report sum-of-product test (SOP-test) for catch estimates
#' 
#'  Mean weight at age and estimated catch at age is used to compute total catches
#'  and the relative difference to reported landings are reported.
#'  
#'  The report will be generated for landings decomposed on the provided 'GroupingVariables'
#'  which must be available in both 'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData'
#'  and 'StoxLandingData'.
#'  
#'  'ReportFdaCatchAtAgeData' and 'ReportFdaWeightAtAgeData' must be decomposed on the same
#'  'GroupingVariables' and must be reported for the same age groups
#' @param ReportFdaCatchAtAgeData \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}} with estimates of total catch at age
#' @param ReportFdaWeightAtAgeData \code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}} with estimates of mean weight at age for individual fish
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#' @param GroupingVariables Columns of 'StoxLandingData' that partitions the landings into groups SOP tests should be reported for.
#' @return \code{\link[RstoxFDA]{ReportFdaSopData}}
#' @seealso 
#'  \code{\link[RstoxFDA]{ReportRecaWeightAtAge}} and \code{\link[RstoxFDA]{ReportRecaCatchAtAge}} for some ways of preparing 'ReportFdaWeightAtAgeData' and 'ReportFdaCatchAtAgeData'.
#'  \code{\link[RstoxData]{StoxLanding}} and \code{\link[RstoxData]{FilterStoxLanding}} for ways of preparing 'StoxLandingData'.
#' @export
ReportFdaSOP <- function(ReportFdaCatchAtAgeData, ReportFdaWeightAtAgeData, StoxLandingData, GroupingVariables=character()){
  
  if (length(GroupingVariables)==0){
    GroupingVariables <- NULL
  }
  
  aggVars <- GroupingVariables
  
  # set a temporary aggregationvariable if none is requested.
  if (is.null(aggVars)){
    aggVars <- c("DummyAgg")
    stopifnot(!("DummyAgg" %in% names(ReportFdaWeightAtAgeData$FdaReport)))
    stopifnot(!("DummyAgg" %in% names(ReportFdaCatchAtAgeData$FdaReport)))
    stopifnot(!("DummyAgg" %in% names(StoxLandingData$Landing)))
    ReportFdaWeightAtAgeData$FdaReport$DummyAgg <- "1"
    ReportFdaWeightAtAgeData$GroupingVariables <- data.table::data.table(GroupingVariables=c("DummyAgg", ReportFdaWeightAtAgeData$GroupingVariables$GroupingVariables))
    ReportFdaCatchAtAgeData$FdaReport$DummyAgg <- "1"
    ReportFdaCatchAtAgeData$GroupingVariables <- data.table::data.table(GroupingVariables=c("DummyAgg", ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables))
    StoxLandingData$Landing$DummyAgg <- "1"
  }
  
  msg <- "All 'GroupingVariables' must be present in both 'ReportFdaCatchAtAgeData', 'ReportFdaWeightAtAgeData' and 'StoxLandingData'"
  if (!all(aggVars %in% names(ReportFdaCatchAtAgeData$FdaReport))){
    stop(msg)
  }
  if (!all(aggVars %in% names(ReportFdaWeightAtAgeData$FdaReport))){
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
  
  if (!all(ReportFdaWeightAtAgeData$FdaReport$AgeGroup %in% ReportFdaCatchAtAgeData$FdaReport$AgeGroup)){
    stop(msg)
  }
  if (!all(ReportFdaCatchAtAgeData$FdaReport$AgeGroup %in% ReportFdaWeightAtAgeData$FdaReport$AgeGroup)){
    stop(msg)
  }

  #merge reports and estimate total for their aggragtion variables
  jointTab <- merge(ReportFdaCatchAtAgeData$FdaReport, ReportFdaWeightAtAgeData$FdaReport, by=c("Age", "AgeGroup", ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables), suffixes = c("mw", "caa"))
  stopifnot(nrow(jointTab) == nrow(ReportFdaCatchAtAgeData$FdaReport))
  
  jointTab$TotalWeightEstimated <- jointTab$CatchAtAge*jointTab$MeanIndividualWeight
  
  # aggregate on requested variables and merge with landings
  estTab <- jointTab[, list(TotalWeightEstimated=sum(get("TotalWeightEstimated"))), by=aggVars]
  landTab <- StoxLandingData$Landing[, list(LandedWeight=sum(get("RoundWeight"))), by=aggVars]
  
  # calculate differences
  reportTab <- merge(estTab, landTab, all=T)
  reportTab$Difference <- reportTab$TotalWeightEstimated - reportTab$LandedWeight
  reportTab$RelativeDifference <- reportTab$Difference / reportTab$LandedWeight
  
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

