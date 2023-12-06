#
# Contains some quality assurance checks on lottery parameters. Package resources are not prepared here, but in prepDesignParameters.R
#


#' Reads catch lottery parameters from file as exported by lottery system Pr oct 2023.
#' @param filename path to file with lottery parameters
#' @return ~\code{\link[data.table]{data.table}} with colmns:
#'  \describe{
#'  \item{aar}{year (integer)}
#'  \item{RC}{Radio call signal of vessel (character)}
#'  \item{SQ}{serialnumber for message (integer) identifies message given year, vessel and message recipient}
#'  \item{TM}{ERS message type (character). HIA means departure message sent to IMR catch lottery (determines target species and therefor inclusion in lottery). HIF means catch message sent to IMR catch lottery, also encoding wheter sample is requested, along with lottery parameters}
#'  \item{BD}{Date for start of fishing operation in UTC: YYYYMMDD}
#'  \item{BT}{Tome for start of fishing operation in UTC: HHMM}
#'  \item{Svar}{Code for whether sample is requested. Code 641 means that a sample is requested. 642 means sample is not requested. 643 means no (more) samples will be requested from this trip (until ned departure message).}
#'  \item{i.prob}{Inclusion probability used in sample selection. Assigned also to samples not selected}
#'  \item{lotteri}{Identifier for lottery, sampling frame, all inclusion probabilities are conditioned only on catch being in lottery}
#'  \item{HIF.stratum}{Any stratification used in setting sampling parameters. Stratification is already factored into inclusion probabilities, some other column that goes into inclusion prob calculation depends on HIF.stratum (e.g. kapasitet)}
#'  \item{kvote}{quota / expected total catch, used in calculation of inclusion probabilities}
#'  \item{kapasitet}{sampling capacity / expected number of samples, used in calculation of inclusion probabilities}
#'  \item{lotteri.kg}{reported catch in kg that was used in calculation of inclusion probability}
#'  
#'  The fields RC, SQ, TM, BC and BT are defined in the ERS regulation (https://lovdata.no/dokument/SF/forskrift/2009-12-21-1743).
#'  }
#' @noRd
parseLotteryFile <- function(filename){
  lotteriparams <- data.table::fread(filename, sep = "\t", dec=".", header = T, colClasses = c("integer", "character", "integer", "character", "character", "character", "character", "numeric", "character", "character", "numeric", "numeric", "numeric"))
  return(lotteriparams)
}

est_total <- function(lotteryParams, lottery, stratum){
  
  lott <- lotteryParams[lotteryParams$lotteri==lottery & lotteryParams$HIF.stratum==stratum]
  stopifnot(length(unique(lott$kvoteT))==1)
  
  kapasitet <- NA
  if (length(unique(lott$kapasitet))==1){
    kapasitet <- lott$kapasitet[1]
  }
  
  totalFrame <- sum(lott$lotteri.kg[!is.na(lott$i.prob) & lott$i.prob>0])
  kvote <- lott$kvoteT[1]*1000
  sample <- lott[lott$Svar=="641",]
  dekning <- sum(lott$lotteri.kg[!is.na(lott$i.prob)& lott$i.prob>0]) / kvote
  HTtot <- sum(sample$lotteri.kg/sample$i.prob)
  HHtot <- mean(sample$lotteri.kg/(sample$lotteri.kg/kvote))*(sum(lott$lotteri.kg, na.rm=T)/kvote)
  
  result <- data.table::data.table(lotteri=lottery, stratum=stratum, totalFrame=totalFrame, kvote=kvote, HTtot=HTtot, kapasitet=kapasitet, n=nrow(sample), RelErrHTFrame=(HTtot-totalFrame)/totalFrame)
  return(result)
}

lotteryStats <- function(lotteryParams){
  results <- NULL
  for (lottery in unique(lotteryParams$lotteri)){
    for (stratum in unique(lotteryParams$HIF.stratum[lotteryParams$lotteri==lottery])){
      stats <- est_total(lotteryParams, lottery, stratum)
      results <- rbind(stats, results)
    }
  }
  results <- results[order(results$n, decreasing=T),]
  return(results)
}

simulate <- function(lotteryParams, lottery, stratum, iterations=1000){
  frame <- lotteryParams[lotteryParams$lotteri==lottery & lotteryParams$HIF.stratum==stratum & !is.na(lotteryParams$i.prob) & lotteryParams$i.prob>0, ]
  
  tab <- NULL
  for (it in 1:iterations){
    for (i in 1:nrow(frame)){
      p <- frame$i.prob[i]
      if (sample(c(T,F), 1, replace = TRUE,prob=c(p,1-p))){
        frame$Svar[i] <- "641"
      }
      else{
        frame$Svar[i] <- "642"
      }
    }
    res <- est_total(frame, lottery, stratum)
    tab <- rbind(tab, res)
  }

  tab$iteration <- 1:iterations
  return(tab)
}

lotteryParams <- parseLotteryFile("~/hi_sync/fiskerisampling/fangstprøvelotteri/lotterifiler/example2022.txt")
lotteryStats <- lotteryStats(lotteryParams)

#check if the perfomranse for Norsjo is extreme:
nssim<-simulate(lotteryParams, "Sild2022", "Nordsjo")
print(paste("Percentile Sild2002, Nordsjo: ", sum(nssim$RelErrHTFrame<(-.191))*100/nrow(nssim), "%"))
