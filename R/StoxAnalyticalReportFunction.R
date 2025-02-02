#' Redefines age variable as lowest age in age group
#' Adds plusgroup and puts NA group to 0.
#' Assumes zero covariance for unkown covariances.
#' Aggregates data
#' @noRd
makePlusGroupAnalytical <- function(AnalyticalPopulationEstimateData, PlusGroup, AgeDomainVar){
  
  AnalyticalPopulationEstimateData$DomainVariables$AgeGroup <- paste("Age", as.character(AnalyticalPopulationEstimateData$DomainVariables[[AgeDomainVar]]))
  AnalyticalPopulationEstimateData$DomainVariables[[AgeDomainVar]][is.na(AnalyticalPopulationEstimateData$DomainVariables[[AgeDomainVar]])] <- 0
  if (isGiven(PlusGroup)){
    if (PlusGroup > max(AnalyticalPopulationEstimateData$DomainVariables[[AgeDomainVar]], na.rm=T)){
      stop("'PlusGroup' is larger than the oldest age group.")
    }
    if (PlusGroup < min(AnalyticalPopulationEstimateData$DomainVariables[[AgeDomainVar]], na.rm=T)){
      stop("'PlusGroup' is smaller than the smallest age group")
    }

    oldDomains <- AnalyticalPopulationEstimateData$DomainVariables
    
    mask <- oldDomains[[AgeDomainVar]] >= PlusGroup
    minAgePlusGroup <- min(oldDomains[[AgeDomainVar]][mask])
    oldDomains[[AgeDomainVar]][mask] <- minAgePlusGroup
    
    newDomains <- AnalyticalPopulationEstimateData$DomainVariables[!mask |  AnalyticalPopulationEstimateData$DomainVariables[[AgeDomainVar]] == minAgePlusGroup,]
    newDomains$Domain[newDomains[[AgeDomainVar]]==minAgePlusGroup] <- paste(newDomains$Domain[newDomains[[AgeDomainVar]]==minAgePlusGroup], "+")
    newDomains$AgeGroup[newDomains[[AgeDomainVar]]>=PlusGroup] <- paste0("Age ", PlusGroup, "+")
    keys <- names(oldDomains)[!(names(oldDomains) %in%  c("Domain", "AgeGroup"))]
    
    newDomains <- merge(oldDomains, newDomains, by=keys, suffix=c("", ".new"))
    newDomains$AgeGroup <- newDomains$AgeGroup.new
    domainMap <- newDomains[,.SD,.SDcols = c("Domain", "Domain.new")]

    #Annotate and aggregate totals and means
    AnalyticalPopulationEstimateData$Variables <- merge(AnalyticalPopulationEstimateData$Variables, domainMap, by="Domain")
    AnalyticalPopulationEstimateData$Variables <- merge(AnalyticalPopulationEstimateData$Variables, AnalyticalPopulationEstimateData$Abundance, by=c("Stratum", "Domain"))
    AnalyticalPopulationEstimateData$Variables$Domain <- AnalyticalPopulationEstimateData$Variables$Domain.new
    #nan for means is for zero-abundance domains
    AnalyticalPopulationEstimateData$Variables <- AnalyticalPopulationEstimateData$Variables[,list(Total=sum(get("Total")), Mean=sum(get("Mean")[!is.nan(get("Mean"))]*get("Abundance")[!is.nan(get("Mean"))])/(sum(get("Abundance")[!is.nan(get("Mean"))]))), by=c("Stratum", "Domain", "Variable")]
    
    #Annotate and aggregate total and mean covariance
    
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, domainMap, by.x="Domain1", by.y="Domain")
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, domainMap, by.x="Domain2", by.y="Domain",  suffixes = c("Domain1", "Domain2"))
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, AnalyticalPopulationEstimateData$Abundance, by.x=c("Stratum", "Domain1"), by.y=c("Stratum", "Domain"))
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, AnalyticalPopulationEstimateData$Abundance, by.x=c("Stratum", "Domain2"), by.y=c("Stratum", "Domain"), suffixes = c("Domain1", "Domain2"))
    
    #assume all covariances are 0 for plus group
    plusgr <- newDomains[get(AgeDomainVar)>=PlusGroup]$Domain
    AnalyticalPopulationEstimateData$VariablesCovariance$MeanCovariance[(AnalyticalPopulationEstimateData$VariablesCovariance$Domain1 %in% plusgr |
                                                                          AnalyticalPopulationEstimateData$VariablesCovariance$Domain2 %in% plusgr) &
                                                                          AnalyticalPopulationEstimateData$VariablesCovariance$Domain1 != AnalyticalPopulationEstimateData$VariablesCovariance$Domain2] <- 0

    AnalyticalPopulationEstimateData$VariablesCovariance$Domain1 <- AnalyticalPopulationEstimateData$VariablesCovariance$Domain.newDomain1
    AnalyticalPopulationEstimateData$VariablesCovariance$Domain2 <- AnalyticalPopulationEstimateData$VariablesCovariance$Domain.newDomain2
    
    totalDomainAbundance <- merge(AnalyticalPopulationEstimateData$Abundance, domainMap, by="Domain")
    totalDomainAbundance <- totalDomainAbundance[,list(TotalFrequency=sum(get("Frequency"))), by=c("Stratum", "Domain.new")]
    
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, totalDomainAbundance, by.x=c("Stratum", "Domain1"), by.y=c("Stratum", "Domain.new"), all.x=T)
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, totalDomainAbundance, by.x=c("Stratum", "Domain2"), by.y=c("Stratum", "Domain.new"), suffixes = c("Domain1", "Domain2"), all.x=T)
    
    
    AnalyticalPopulationEstimateData$VariablesCovariance <- AnalyticalPopulationEstimateData$VariablesCovariance[,list(TotalCovariance=sum(get("TotalCovariance")), 
                                                                                                                       MeanCovariance=sum(get("MeanCovariance") * 
                                                                                                                                            (get("FrequencyDomain1") / get("TotalFrequencyDomain1")) *
                                                                                                                                            (get("FrequencyDomain2") / get("TotalFrequencyDomain2"))
                                                                                                                                          )
                                                                                                                         ), 
                                                                                                                 by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]

    
    #Annotate and aggregate abundance and frquencies
    AnalyticalPopulationEstimateData$Abundance <- merge(AnalyticalPopulationEstimateData$Abundance, domainMap, by="Domain")
    AnalyticalPopulationEstimateData$Abundance$Domain <- AnalyticalPopulationEstimateData$Abundance$Domain.new
    AnalyticalPopulationEstimateData$Abundance <- AnalyticalPopulationEstimateData$Abundance[,list(Abundance=sum(get("Abundance")), Frequency=sum(get("Frequency"))), by=c("Stratum", "Domain")]
    
    #Annotate and aggregate abundance and frequency covariance
    AnalyticalPopulationEstimateData$AbundanceCovariance <- merge(AnalyticalPopulationEstimateData$AbundanceCovariance, domainMap, by.x="Domain1", by.y="Domain")
    AnalyticalPopulationEstimateData$AbundanceCovariance <- merge(AnalyticalPopulationEstimateData$AbundanceCovariance, domainMap, by.x="Domain2", by.y="Domain",  suffixes = c("Domain1", "Domain2"))
    AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1 <- AnalyticalPopulationEstimateData$AbundanceCovariance$Domain.newDomain1
    AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2 <- AnalyticalPopulationEstimateData$AbundanceCovariance$Domain.newDomain2
    AnalyticalPopulationEstimateData$AbundanceCovariance <- AnalyticalPopulationEstimateData$AbundanceCovariance[,list(AbundanceCovariance=sum(get("AbundanceCovariance")), FrequencyCovariance=sum(get("FrequencyCovariance"))), by=c("Stratum", "Domain1", "Domain2")]
    
    #update domainVariables
    AnalyticalPopulationEstimateData$DomainVariables <- newDomains[!duplicated(get("Domain.new")),]
    AnalyticalPopulationEstimateData$DomainVariables$Domain <- AnalyticalPopulationEstimateData$DomainVariables$Domain.new
    AnalyticalPopulationEstimateData$DomainVariables$Domain.new <- NULL
    
    # set covariances with plusgroup to NA
    plusgr <- newDomains[get(AgeDomainVar)>=PlusGroup]$Domain.new
    AnalyticalPopulationEstimateData$VariablesCovariance$MeanCovariance[(AnalyticalPopulationEstimateData$VariablesCovariance$Domain1 %in% plusgr |
                                                                           AnalyticalPopulationEstimateData$VariablesCovariance$Domain2 %in% plusgr) &
                                                                          AnalyticalPopulationEstimateData$VariablesCovariance$Domain1 != AnalyticalPopulationEstimateData$VariablesCovariance$Domain2] <- NA
    
  }
  
  return(AnalyticalPopulationEstimateData)
}


#' Report catch at age
#' @description
#'  Tabulates summary statistics for analytical catch at age estimate.
#'  Summary statistics are obtained as analytical domain estimates,
#'  along with an estimate of the standard deviation of their sampling distribution (the standard error).
#'  Confidence intervals are calculated from a Gaussian approximation to the sampling distribution.
#'  
#'  If AnalyticalPopulationEstimateData contains estimates for domains that include more than just age, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#' 
#'  The units considered valid for catch at age in numbers are those listed for quantity 'cardinaltiy' in \code{\link[RstoxData]{StoxUnits}}
#' @param AnalyticalPopulationEstimateData Results from analytical estimates (\code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}). The StoxBiotic variable 'IndividualAge' must be among the DomainVariables.
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported confidence interval. A value of 0.9 gives 90 per cent confidence intervals. 
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtAge', 'SD', 'Low' and 'High'.
#' @param Unit unit for 'CatchAtAge', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} and \code{\link[RstoxFDA]{AnalyticalRatioEstimate}} for obtaining analytical estimates.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
ReportAnalyticalCatchAtAge <- function(AnalyticalPopulationEstimateData, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  AgeDomainVar = "IndividualAge"
  
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  stopifnot(is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData))
  
  if (!(AgeDomainVar %in% names(AnalyticalPopulationEstimateData$DomainVariables))){
    stop(paste("Catch-at-age reporting, requires the StoxBiotic variable 'IndividualAge' to be among the domain variables of 'AnalyticalPopulationEstimateData'."))
  }
  
  checkMandatory(Decimals, "Decimals")
  checkMandatory(IntervalWidth, "IntervalWidth")
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("cardinality"))
  check_intervalWidth(IntervalWidth)
      
  GroupingVariables <- names(AnalyticalPopulationEstimateData$DomainVariables)
  GroupingVariables <- GroupingVariables[!(GroupingVariables %in% c("Domain", AgeDomainVar))]
  AnalyticalPopulationEstimateData <- makePlusGroupAnalytical(AnalyticalPopulationEstimateData, PlusGroup, AgeDomainVar)    
  
  pointEst <- merge(AnalyticalPopulationEstimateData$Abundance, AnalyticalPopulationEstimateData$DomainVariables, by="Domain")
  varEst <- AnalyticalPopulationEstimateData$AbundanceCovariance[AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1==AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2,]
  varEst$Domain <- varEst$Domain1

  tab <- merge(pointEst, varEst, by=c("Stratum", "Domain"))
  tab$Age <- tab[[AgeDomainVar]]
  
  alpha <- (1-IntervalWidth)/2.0
  result <- tab[,list(CatchAtAge=get("Abundance"), SD=sqrt(get("AbundanceCovariance")), Low=max(stats::qnorm(alpha,mean=get("Abundance"), sd=sqrt(get("AbundanceCovariance"))),0), High=stats::qnorm(1-alpha,mean=get("Abundance"), sd=sqrt(get("AbundanceCovariance")))), by=c(GroupingVariables, "AgeGroup", "Age")]
  
  caa <- list()
  caa$NbyAge <- result
  caa$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  
  
  caa$NbyAge <- setUnits(caa$NbyAge, "Age", "year", "age")
  caa$NbyAge <- setUnits(caa$NbyAge, c("CatchAtAge", "SD", "Low", "High"), "individuals", "cardinality")
  if (isGiven(Unit)){
    caa$NbyAge <- setUnits(caa$NbyAge, c("CatchAtAge", "SD", "Low", "High"), Unit, "cardinality")  
  }
  
  if (isGiven(Decimals)){
    caa$NbyAge <- setDecimals(caa$NbyAge, c("CatchAtAge", "SD", "Low", "High"), Decimals)
  }
  
  #order by age
  caa$NbyAge <- caa$NbyAge[order(caa$NbyAge$Age),]
  
  return(caa)
}

#' Report catch at length
#' @description
#'  Tabulates summary statistics for analytical catch at length estimate.
#'  Summary statistics are obtained as analytical domain estimates, including a length-group
#'  domain, obtained by annotating sample data with the function \code{\link[RstoxFDA]{AddLengthGroupStoxBiotic}}.
#'  An estimate of the standard deviation of their sampling distribution (the standard error) is also provided.
#'  Confidence intervals are calculated from a Gaussian approximation to the sampling distribution.
#'  
#'  If AnalyticalPopulationEstimateData contains estimates for domains that include more than just length group, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#' 
#'  The units considered valid for catch at length in numbers are those listed for quantity 'cardinaltiy' in \code{\link[RstoxData]{StoxUnits}}
#' @param AnalyticalPopulationEstimateData Results from analytical estimates (\code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}). A variable identifying length groups (argument: 'LengthGroupVariable') must be among the domain variables. This must be formatted as done by \code{\link[RstoxFDA]{AddLengthGroupStoxBiotic}}.
#' @param LengthGroupVariable Name of domain variable in 'AnalyticalPopulationEstimateData' that identifies length group.
#' @param IntervalWidth The width of the reported confidence interval. A value of 0.9 gives 90 per cent confidence intervals. 
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtLength', 'SD', 'Low' and 'High'.
#' @param Unit unit for 'CatchAtLength', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtLengthData}}
#' @seealso \code{\link[RstoxFDA]{AddLengthGroupStoxBiotic}} for annotating length groups, \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} and \code{\link[RstoxFDA]{AnalyticalRatioEstimate}} for obtaining analytical estimates. See \code{\link[RstoxFDA]{ReportAnalyticalCatchAtAge}} for reporting catch at age.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
ReportAnalyticalCatchAtLength <- function(AnalyticalPopulationEstimateData, LengthGroupVariable=character(), IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  stopifnot(is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData))
  checkMandatory(LengthGroupVariable, "LengthGroupVariable")
  if (!(LengthGroupVariable %in% names(AnalyticalPopulationEstimateData$DomainVariables))){
    stop(paste("Catch-at-length reporting, requires the StoxBiotic variable identified by 'LengthGroupVariable'", LengthGroupVariable," to be among the domain variables of 'AnalyticalPopulationEstimateData'."))
  }
  if (!(all(startsWith(AnalyticalPopulationEstimateData$DomainVariables[[LengthGroupVariable]], "[")) |
      all(startsWith(AnalyticalPopulationEstimateData$DomainVariables[[LengthGroupVariable]], "(")))){
    stop("Malformed length groups. Use AddLengthGroupStoxBiotic to annotate length groups")
  }
  if (!(all(endsWith(AnalyticalPopulationEstimateData$DomainVariables[[LengthGroupVariable]], "]")) |
        all(endsWith(AnalyticalPopulationEstimateData$DomainVariables[[LengthGroupVariable]], ")")))){
    stop("Malformed length groups. Use AddLengthGroupStoxBiotic to annotate length groups")
  }
  if (!(all(grepl(",", AnalyticalPopulationEstimateData$DomainVariables[[LengthGroupVariable]])))){
    stop("Malformed length groups. Use AddLengthGroupStoxBiotic to annotate length groups")
  }
  
  checkMandatory(Decimals, "Decimals")
  checkMandatory(IntervalWidth, "IntervalWidth")
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("cardinality"))
  check_intervalWidth(IntervalWidth)
  
  GroupingVariables <- names(AnalyticalPopulationEstimateData$DomainVariables)
  GroupingVariables <- GroupingVariables[!(GroupingVariables %in% c("Domain", LengthGroupVariable))]

  pointEst <- merge(AnalyticalPopulationEstimateData$Abundance, AnalyticalPopulationEstimateData$DomainVariables, by="Domain")
  varEst <- AnalyticalPopulationEstimateData$AbundanceCovariance[AnalyticalPopulationEstimateData$AbundanceCovariance$Domain1==AnalyticalPopulationEstimateData$AbundanceCovariance$Domain2,]
  varEst$Domain <- varEst$Domain1
  
  tab <- merge(pointEst, varEst, by=c("Stratum", "Domain"))
  tab$LengthGroup <- tab[[LengthGroupVariable]]
  tab$Length <- as.numeric(gsub("]", "", gsub(")", "", sapply(strsplit(AnalyticalPopulationEstimateData$DomainVariables[[LengthGroupVariable]], ","), FUN=function(x){x[[2]]}))))
  
  alpha <- (1-IntervalWidth)/2.0
  result <- tab[,list(CatchAtLength=get("Abundance"), SD=sqrt(get("AbundanceCovariance")), Low=max(stats::qnorm(alpha,mean=get("Abundance"), sd=sqrt(get("AbundanceCovariance"))),0), High=stats::qnorm(1-alpha,mean=get("Abundance"), sd=sqrt(get("AbundanceCovariance")))), by=c(GroupingVariables, "LengthGroup", "Length")]

  cal <- list()
  cal$NbyLength <- result
  cal$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  
  
  cal$NbyLength <- setUnits(cal$NbyLength, "Length", "cm", "length")
  cal$NbyLength <- setUnits(cal$NbyLength, c("CatchAtLength", "SD", "Low", "High"), "individuals", "cardinality")
  if (isGiven(Unit)){
    cal$NbyLength <- setUnits(cal$NbyLength, c("CatchAtLength", "SD", "Low", "High"), Unit, "cardinality")  
  }
  
  if (isGiven(Decimals)){
    cal$NbyLength <- setDecimals(cal$NbyLength, c("CatchAtLength", "SD", "Low", "High"), Decimals)
  }
  
  #order by length
  cal$NbyLength <- cal$NbyLength[order(cal$NbyLength$Length),]
  
  return(cal)
}

#' mean by age report
#' @noRd
meanByAgeDomain <- function(AnalyticalPopulationEstimateData, PlusGroup, IntervalWidth=numeric(), Decimals=integer(), AgeDomainVar=character(), obsVar=character()){
  GroupingVariables <- names(AnalyticalPopulationEstimateData$DomainVariables)
  GroupingVariables <- GroupingVariables[!(GroupingVariables %in% c("Domain", AgeDomainVar))]
  AnalyticalPopulationEstimateData <- makePlusGroupAnalytical(AnalyticalPopulationEstimateData, PlusGroup, AgeDomainVar)    
  
  pointEst <- merge(AnalyticalPopulationEstimateData$Variables[AnalyticalPopulationEstimateData$Variables$Variable==obsVar,], AnalyticalPopulationEstimateData$DomainVariables, by="Domain")
  varEst <- AnalyticalPopulationEstimateData$VariablesCovariance[AnalyticalPopulationEstimateData$VariablesCovariance$Domain1==AnalyticalPopulationEstimateData$VariablesCovariance$Domain2 & 
                                                                   AnalyticalPopulationEstimateData$VariablesCovariance$Variable1==AnalyticalPopulationEstimateData$VariablesCovariance$Variable2 &
                                                                   AnalyticalPopulationEstimateData$VariablesCovariance$Variable1==obsVar,]
  varEst$Domain <- varEst$Domain1

  tab <- merge(pointEst, varEst, by=c("Stratum", "Domain"))
  tab$Age <- tab[[AgeDomainVar]]

  alpha <- (1-IntervalWidth)/2.0
  
  result <- tab[,list(Mean=get("Mean"), SD=sqrt(get("MeanCovariance")), Low=max(stats::qnorm(alpha,mean=get("Mean"), sd=sqrt(get("MeanCovariance"))),0), High=stats::qnorm(1-alpha,mean=get("Mean"), sd=sqrt(get("MeanCovariance")))), by=c(GroupingVariables, "AgeGroup", "Age")]
  
  mba <- list()
  mba$MeanByAge <- result
  mba$GroupingVariables <- data.table::data.table(GroupingVariables=GroupingVariables)
  
  if (isGiven(Decimals)){
    mba$MeanByAge <- setDecimals(mba$MeanByAge, c("Mean", "SD", "Low", "High"), Decimals)
  }
  
  #order by age
  mba$MeanByAge <- mba$MeanByAge[order(mba$MeanByAge$Age),]
  
  return(mba)
}

#' Report mean weight at age
#' @description
#'  Tabulates summary statistics for analytical mean weight at age estimates.
#'  Summary statistics are obtained as analytical domain estimates,
#'  along with an estimate of the standard deviation of their sampling distribution (the standard error).
#'  Confidence intervals are calculated from a Gaussian approximation to the sampling distribution.
#'  
#'  If AnalyticalPopulationEstimateData contains estimates for domains that include more than just age, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#'  
#'  Covariances for means of a variable between domains are not always defined, and the variances (and hence 'SD', 'Low' and 'High') 
#'  for means in the plusgroup is approximated with an assumption of independence.
#' 
#'  The units considered valid for catch at age in numbers are those listed for quantity 'mass' in \code{\link[RstoxData]{StoxUnits}}
#' @param AnalyticalPopulationEstimateData Results from analytical estimates (\code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}). The StoxBiotic variable 'IndividualAge' must be among the DomainVariables, and estimates must be available for the StoxBiotic-variable 'IndividualRoundWeight'.
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported confidence interval. A value of 0.9 gives 90 per cent confidence intervals.
#' @param Decimals integer specifying the number of decimals to report for 'MeanIndividualWeight', 'SD', 'Low' and 'High'.
#' @param Unit unit for 'MeanIndividualWeight', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} and \code{\link[RstoxFDA]{AnalyticalRatioEstimate}} for obtaining analytical estimates.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
ReportAnalyticalWeightAtAge <- function(AnalyticalPopulationEstimateData, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("mass", conversionRange=c(1e-3,1))){
  AgeDomainVar = "IndividualAge"
  WeightVar = "IndividualRoundWeight"
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  stopifnot(is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData))
  if (!(AgeDomainVar %in% names(AnalyticalPopulationEstimateData$DomainVariables))){
    stop(paste("Weight-at-age reporting, requires the StoxBiotic variable '",AgeDomainVar,"' to be among the domain variables of 'AnalyticalPopulationEstimateData'."))
  }
  if (!(WeightVar %in% AnalyticalPopulationEstimateData$Variables$Variable)){
    stop(paste("Weight-at-age reporting, requires the StoxBiotic variable '",WeightVar,"' to be among the variables of 'AnalyticalPopulationEstimateData'.", sep=""))
  }
  checkMandatory(Decimals, "Decimals")
  checkMandatory(IntervalWidth, "IntervalWidth")
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("mass"))
  check_intervalWidth(IntervalWidth)

  mwa <- meanByAgeDomain(AnalyticalPopulationEstimateData, PlusGroup, IntervalWidth, Decimals, AgeDomainVar, WeightVar)
  
  names(mwa)[names(mwa)=="MeanByAge"] <- "MeanWeightByAge"
  names(mwa$MeanWeightByAge)[names(mwa$MeanWeightByAge)=="Mean"] <- "MeanIndividualWeight"
  

  mwa$MeanWeightByAge <- setUnits(mwa$MeanWeightByAge, c("MeanIndividualWeight", "SD", "Low", "High"), "g", "mass")
  if (isGiven(Unit)){
    mwa$MeanWeightByAge <- setUnits(mwa$MeanWeightByAge, c("MeanIndividualWeight", "SD", "Low", "High"), Unit, "mass")  
  }
    
  return(mwa)
}

#' Report mean length at age
#' @description
#'  Tabulates summary statistics for analytical mean total length at age estimates.
#'  Summary statistics are obtained as analytical domain estimates,
#'  along with an estimate of the standard deviation of their sampling distribution (the standard error).
#'  Confidence intervals are calculated from a Gaussian approximation to the sampling distribution.
#'  
#'  If AnalyticalPopulationEstimateData contains estimates for domains that include more than just age, such as
#'  area, gear, stock, etc., summary statistics will be presented similarly.
#'  
#'  Rounding of numbers according to the argument 'Decimals' is done with \code{\link[base]{round}},
#'  so that negative numbers specify rounding to powers of ten, and rounding of the digit 5 is towards the even digit.
#' 
#'  Covariances for means of a variable between domains are not always defined, and the variances (and hence 'SD', 'Low' and 'High') 
#'  for means in the plusgroup is approximated with an assumption of independence.
#' 
#'  The units considered valid for catch at age in numbers are those listed for quantity 'length' in \code{\link[RstoxData]{StoxUnits}}
#' @param AnalyticalPopulationEstimateData Results from analytical estimates (\code{\link[RstoxFDA]{AnalyticalPopulationEstimateData}}). The StoxBiotic variable 'IndividualAge' must be among the DomainVariables, and estimates must be available for the StoxBiotic-variable 'IndividualTotalLength'.
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param IntervalWidth The width of the reported confidence interval. A value of 0.9 gives 90 per cent confidence intervals.
#' @param Decimals integer specifying the number of decimals to report for 'MeanIndividualLength', 'SD', 'Low' and 'High'.
#' @param Unit unit for 'MeanIndividualLength', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaLengthAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} and \code{\link[RstoxFDA]{AnalyticalRatioEstimate}} for obtaining analytical estimates.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
ReportAnalyticalLengthAtAge <- function(AnalyticalPopulationEstimateData, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("length", conversionRange=c(1e-3,1))){
  AgeDomainVar = "IndividualAge"
  LengthVar = "IndividualTotalLength"
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  stopifnot(is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData))
  if (!(AgeDomainVar %in% names(AnalyticalPopulationEstimateData$DomainVariables))){
    stop(paste("Length-at-age reporting, requires the StoxBiotic variable '",AgeDomainVar,"' to be among the domain variables of 'AnalyticalPopulationEstimateData'."))
  }
  if (!(LengthVar %in% AnalyticalPopulationEstimateData$Variables$Variable)){
    stop(paste("Length-at-age reporting, requires the StoxBiotic variable '",LengthVar,"' to be among the variables of 'AnalyticalPopulationEstimateData'.", sep=""))
  }
  checkMandatory(Decimals, "Decimals")
  checkMandatory(IntervalWidth, "IntervalWidth")
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("length"))
  check_intervalWidth(IntervalWidth)
  
  mla <- meanByAgeDomain(AnalyticalPopulationEstimateData, PlusGroup, IntervalWidth, Decimals, AgeDomainVar, LengthVar)
  
  names(mla)[names(mla)=="MeanByAge"] <- "MeanLengthByAge"
  names(mla$MeanLengthByAge)[names(mla$MeanLengthByAge)=="Mean"] <- "MeanIndividualLength"
  
  
  mla$MeanLengthByAge <- setUnits(mla$MeanLengthByAge, c("MeanIndividualLength", "SD", "Low", "High"), "cm", "length")
  if (isGiven(Unit)){
    mla$MeanLengthByAge <- setUnits(mla$MeanLengthByAge, c("MeanIndividualLength", "SD", "Low", "High"), Unit, "length")  
  }
  
  return(mla)
}
