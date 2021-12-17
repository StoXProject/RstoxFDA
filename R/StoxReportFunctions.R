#' Rounds the specified number of decimals
#' @noRd
desimals <- function(x, Decimals=integer()){
  
  if (isGiven(Decimals)){
    return(round(x, digits = Decimals))
  }
  
  return(x)
  
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
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries
#'  and approriate columns added for identifying corresponding landings.
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns added for identifying corresponding samples
#' @param GroupingVariables Columns of 'StoxBioticData' and 'StoxLandingData' that partitions the fisheries. Defaults to all column names that are found in both inputs.
#' @param Decimals integer specifying the number of decimals to report for 'LandedRoundWeight' and 'WeightOfSampledCatches'
#' @return \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @export
ReportFdaSampling <- function(StoxBioticData, StoxLandingData, GroupingVariables=character(), Decimals=integer()){
  
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
  tab <- tab[order(tab$LandedRoundWeight, decreasing = T),]
  
  if (isGiven(Decimals)){
    tab$WeightOfSampledCatches <- desimals(tab$WeightOfSampledCatches, Decimals)
    tab$LandedRoundWeight <- desimals(tab$LandedRoundWeight, Decimals)
  }
  
  output <- list()
  output$FisheriesSampling <- tab
  output$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  
  return(output)
}

#' @noRd
reportParameterAtAge <- function(table, aggVariables, parameter, digits=6, alpha=.1){
  
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
#'  Tabulates summary statistics for total catch (number) at age from MCMC simulations using Reca.
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
#'  Tabulates summary statistics for mean weights (kg) at age from MCMC simulations using Reca.
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
#'  Tabulates summary statistics for mean length (cm) at age from MCMC simulations using Reca.
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
#'  Weights are reported in kg, lengths in cm.
#'  
#'  If 'RecaCatchAtAge' contains estimate for a set of aggregation variables, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param IntervalWidth The width of the reported credible interval. Defaults to 0.9 for 90 per cent credible intervals.
#' @return \code{\link[RstoxFDA]{ReportFdaSummaryData}}
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
  
  
  
  # total weight
  # hack to use ReportRecaCatchAtAge
  mm <- merge(RecaCatchAtAge$CatchAtAge, RecaCatchAtAge$MeanWeight)
  mm$CatchAtAge <- mm$CatchAtAge*mm$MeanIndividualWeight
  mm$MeanIndividualWeight <- NULL
  ss <- RecaCatchAtAge
  ss$CatchAtAge <- mm
  TotalWeight<-ReportRecaCatchAtAge(ss, PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  names(TotalWeight$FdaReport)[names(TotalWeight$FdaReport)=="CatchAtAge"] <- "TotalWeight"
  TotalWeight$FdaReport$AgeGroup <- NULL
  TotalWeight$FdaReport$Age <- NULL
  
  # total number
  TotalNumber<-ReportRecaCatchAtAge(RecaCatchAtAge, PlusGroup = min(RecaCatchAtAge$CatchAtAge$Age))
  names(TotalNumber$FdaReport)[names(TotalNumber$FdaReport)=="CatchAtAge"] <- "TotalNumber"
  TotalNumber$FdaReport$AgeGroup <- NULL
  TotalNumber$FdaReport$Age <- NULL
  
  # combine
  output <- list()
  output$MeanAge <- meanAge$FdaReport
  output$MeanWeight <- meanWeight$FdaReport
  output$MeanLength <- meanLength$FdaReport
  output$TotalWeight <- TotalWeight$FdaReport
  output$TotalNumber <- TotalNumber$FdaReport
  output$GroupingVariables <- meanAge$GroupingVariables
  
  return(output)
}

#' Report SOP test
#' @description 
#'  Report sum-of-product test (SOP-test) for catch estimates.
#' 
#'  Mean weight (kg) at age and estimated catch (numbers) at age is used to compute total catches
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
#'  <model name>-<covariate name>-<any covariate value/level>:<any Age group> <parameter type>,
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
ReportRecaParameterStatistics <- function(RecaParameterData, ParameterizationSummaryData, AppendReport=FALSE){
  
  
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
#' @return \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @export
ReportParameterConvergence <- function(ParameterizationSummaryData, Tolerance=numeric()){
  
  if (!isGiven(Tolerance)){
    Tolerance = 0.1
  }
  
  if (length(unique(ParameterizationSummaryData$RunParameters$Iterations))!=1){
    stop("All chains must be run for the same number of iterations")
  }
  iterations <- ParameterizationSummaryData$RunParameters$Iterations[1]
  
  output <- list()
  output$ConvergenceReport <- crossChainConvergence(ParameterizationSummaryData$ParameterSummary, iterations, Tolerance)

  return(output)
}
