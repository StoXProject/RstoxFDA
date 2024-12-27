#' Redefines age variable as lowest age in age group
#' Adds plusgroup and puts NA group to 0.
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
    AnalyticalPopulationEstimateData$Variables <- AnalyticalPopulationEstimateData$Variables[,list(Total=sum(get("Total")), Mean=sum(get("Mean")*get("Abundance"))/(sum(get("Abundance")))), by=c("Stratum", "Domain", "Variable")]
    
    #Annotate and aggregate total and mean covariance
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, domainMap, by.x="Domain1", by.y="Domain")
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, domainMap, by.x="Domain2", by.y="Domain",  suffixes = c("Domain1", "Domain2"))
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, AnalyticalPopulationEstimateData$Abundance, by.x=c("Stratum", "Domain1"), by.y=c("Stratum", "Domain"))
    AnalyticalPopulationEstimateData$VariablesCovariance <- merge(AnalyticalPopulationEstimateData$VariablesCovariance, AnalyticalPopulationEstimateData$Abundance, by.x=c("Stratum", "Domain2"), by.y=c("Stratum", "Domain"), suffixes = c("Domain1", "Domain2"))
    AnalyticalPopulationEstimateData$VariablesCovariance$Domain1 <- AnalyticalPopulationEstimateData$VariablesCovariance$Domain.newDomain1
    AnalyticalPopulationEstimateData$VariablesCovariance$Domain2 <- AnalyticalPopulationEstimateData$VariablesCovariance$Domain.newDomain2
    AnalyticalPopulationEstimateData$VariablesCovariance <- AnalyticalPopulationEstimateData$VariablesCovariance[,list(TotalCovariance=sum(get("TotalCovariance")), MeanCovariance=sum(get("MeanCovariance")*get("AbundanceDomain1")*get("AbundanceDomain2"))/(sum(get("AbundanceDomain1"))*sum(get("AbundanceDomain2")))), by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2")]

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
#' @param IntervalWidth The width of the reported confidence interval. A value of 0.9 gives 90 per cent confidence intervals. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportAnalyticalCatchAtAge$functionParameterDefaults$IntervalWidth`
#' @param Decimals integer specifying the number of decimals to report for 'CatchAtAge', 'SD', 'Low' and 'High'. Defaults to `r RstoxFDA:::stoxFunctionAttributes$ReportAnalyticalCatchAtAge$functionParameterDefaults$Decimals`.
#' @param Unit unit for 'CatchAtAge', 'SD', 'Low' and 'High'
#' @return \code{\link[RstoxFDA]{ReportFdaCatchAtAgeData}}
#' @seealso \code{\link[RstoxFDA]{AnalyticalPopulationEstimate}} and \code{\link[RstoxFDA]{AnalyticalRatioEstimate}} for obtaining analytical estimates.
#' @concept StoX-Reca functions
#' @concept StoX-functions
#' @export
ReportAnalyticalCatchAtAge <- function(AnalyticalPopulationEstimateData, PlusGroup=integer(), IntervalWidth=numeric(), Decimals=integer(), Unit=RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12))){
  AgeDomainVar = "IndividualAge"
  
  if (!(AgeDomainVar %in% names(AnalyticalPopulationEstimateData$DomainVariables))){
    stop(paste("Catch-at-age reporting, requires the StoxBiotic variable 'IndividualAge' to be among the domain variables of 'AnalyticalPopulationEstimateData'."))
  }
  
  checkMandatory(AnalyticalPopulationEstimateData, "AnalyticalPopulationEstimateData")
  stopifnot(is.AnalyticalPopulationEstimateData(AnalyticalPopulationEstimateData))

  Decimals <- getDefault(Decimals, "Decimals", F, RstoxFDA::stoxFunctionAttributes$ReportAnalyticalCatchAtAge$functionParameterDefaults$Decimals)
  Unit <- checkOptions(Unit, "Unit", RstoxData::getUnitOptions("cardinality", conversionRange=c(1,1e12)))
  IntervalWidth <- getDefault(IntervalWidth, "IntervalWidth", F, RstoxFDA::stoxFunctionAttributes$ReportAnalyticalCatchAtAge$functionParameterDefaults$IntervalWidth)
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
