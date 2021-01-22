#' Report FDA sampling
#' @description 
#'  Report sampling of fisheries against landings in partitions of the fisheries.
#' @details 
#'  Sampling is reported partitioned on the provided 'AggregationVariables'.
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
#' @param AggregationVariables Columns of 'StoxBioticData' and 'StoxLandingData' that partitions the fisheries. Defaults to all column names that are found in both inputs.
#' @return \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @export
ReportFdaSampling <- function(StoxBioticData, StoxLandingData, AggregationVariables=NULL){
  
  flatlandings <- StoxLandingData$landings
  flatbiotic <- RstoxData::MergeStoxBiotic(StoxBioticData)
  
  if (!isGiven(AggregationVariables)){
    AggregationVariables <- names(flatbiotic)[names(flatbiotic) %in% names(flatlandings)]
  }
  
  if (length(AggregationVariables) == 0){
    stop("No variables to stats::aggregate and compare. Provide parameter 'AggregationVariables'")
  }
  
  if (!all(AggregationVariables %in% names(flatlandings))){
    missing <- AggregationVariables[!(AggregationVariables %in% names(flatlandings))]
    stop(paste("All 'AggregationVariables' must be present in 'StoxLandingData'. Missing:", paste(missing, sep=",")))
  }
  if (!all(AggregationVariables %in% names(flatbiotic))){
    missing <- AggregationVariables[!(AggregationVariables %in% names(flatbiotic))]
    stop(paste("All 'AggregationVariables' must be present in 'StoxBioticData'. Missing:", paste(missing, sep=",")))
  }
  
  
  bioticAggList <- list()
  for (v in AggregationVariables){
    bioticAggList[[v]] <- flatbiotic[[v]]
  }

  samples <- flatbiotic[,c(AggregationVariables, "IndividualRoundWeight", "IndividualAge", "IndividualTotalLength", "CatchFractionWeight", "CatchPlatform", "StationKey", "IndividualKey"), with=F]
  sampledTab <- data.table::data.table(stats::aggregate(list(Catches=samples$StationKey), by=bioticAggList, FUN=function(x){length(unique(x))}))
  vessels <- data.table::data.table(stats::aggregate(list(Vessels=samples$CatchPlatform), by=bioticAggList, FUN=function(x){length(unique(x))}))
  sampledTab <- merge(sampledTab, vessels, by=AggregationVariables)
  weights <- data.table::data.table(stats::aggregate(list(WeightMeasurments=samples$IndividualRoundWeight), by=bioticAggList, FUN=function(x){sum(!is.na(x))}))
  sampledTab <- merge(sampledTab, weights, by=AggregationVariables)
  lengths <- data.table::data.table(stats::aggregate(list(LengthMeasurments=samples$IndividualTotalLength), by=bioticAggList, FUN=function(x){sum(!is.na(x))}))
  sampledTab <- merge(sampledTab, lengths, by=AggregationVariables)
  ages <- data.table::data.table(stats::aggregate(list(AgeReadings=samples$IndividualAge), by=bioticAggList, FUN=function(x){sum(!is.na(x))}))
  sampledTab <- merge(sampledTab, ages, by=AggregationVariables)
  sampledWeights <- data.table::data.table(stats::aggregate(list(WeightOfSampledCatches=samples$CatchFractionWeight), by=bioticAggList, FUN=function(x){sum(x, na.rm=T)}))
  sampledTab <- merge(sampledTab, sampledWeights, by=AggregationVariables)
  
  landingsAggList <- list()
  for (v in AggregationVariables){
    landingsAggList[[v]] <- flatlandings[[v]]
  }
  landings <- flatlandings[,c(AggregationVariables, "RoundWeight"), with=F]
  landingsTab <- data.table::data.table(stats::aggregate(list(LandedRoundWeight=landings$RoundWeight), by=landingsAggList, FUN=function(x){sum(x, na.rm=T)}))
  
  tab <- merge(landingsTab, sampledTab, by=AggregationVariables, all=T)
  tab <- tab[order(tab$LandedRoundWeight),]
  
  output <- list()
  output$FisheriesSampling <- tab
  output$AggregationVariables <- AggregationVariables
  
  return(output)
}