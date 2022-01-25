#
# Plots that can be made from StoX-functions. Not necesarrily included in StoX
#

#' Plot landings
#' @description
#'  Plots landings by date of catch and by group
#' @param \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @seealso \code{\link[RstoxData]{ReportFdaLandings}}
#' @export
FisheriesOverviewTemporal <- function(ReportFdaLandingData){
  
  if (!("CatchDate" %in% ReportFdaLandingData$GroupingVariables$GroupingVariables)){
    stop("Requires 'CatchDate' to be in the 'GroupingVariable' og 'ReportFdaLandingData'")
  }
  
  ftab <- ReportFdaLandingData$FisheriesLandings
  groupvars <- ReportFdaLandingData$GroupingVariables$GroupingVariables[ReportFdaLandingData$GroupingVariables$GroupingVariables != "CatchDate"]
  
  if (length(groupvars)==0){
    ftab$group <- "All"  
  }
  else{
    group <- groupvars
    head <- group[[1]]
    ftab$group <- ftab[[head]]
    group <- group[group != head]
    while (length(group)>0){
      head <- group[[1]]
      ftab$group <- paste(ftab$group, ftab[[head]], sep="/")
      group <- group[group != head]  
    }
  }
  
  ftab <- ftab[order(ftab$CatchDate),]
  pl <- ggplot2::ggplot(data=ftab, ggplot2::aes_string(x="CatchDate", y="LandedRoundWeight", group="group"))
  pl <- pl + ggplot2::geom_line(ggplot2::aes_string(col="group"))
  pl <- pl + ggplot2::ylab(paste("weight [", RstoxData::getUnit(ReportFdaLandingData$FisheriesLandings$LandedRoundWeight, property = "shortname"), "]", sep=""))
  pl <- pl + ggplot2::xlab("catch date")
  pl <- pl + ggplot2::ggtitle(paste(groupvars, collapse=","))
  
  return(pl)
}

#' Plot landings
#' @description
#'  Plots landings 
#' @param \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @seealso \code{\link[RstoxData]{ReportFdaLandings}}
#' @export
FisheriesOverviewSpatial <- function(ReportFdaLandingData, StratumPolygon){
  
  if (!("Area" %in% ReportFdaLandingData$GroupingVariables$GroupingVariables)){
    stop("Requires 'Area' to be in the 'GroupingVariable' og 'ReportFdaLandingData'")
  }
  
  ftab <- ReportFdaLandingData$FisheriesLandings
  groupvars <- ReportFdaLandingData$GroupingVariables$GroupingVariables[ReportFdaLandingData$GroupingVariables$GroupingVariables != "Area"]
  
  if (length(groupvars)==0){
    ftab$group <- "All"  
  }
  else{
    group <- groupvars
    head <- group[[1]]
    ftab$group <- ftab[[head]]
    group <- group[group != head]
    while (length(group)>0){
      head <- group[[1]]
      ftab$group <- paste(ftab$group, ftab[[head]], sep="/")
      group <- group[group != head]  
    }
  }
  
  ftab <- ftab[order(ftab$CatchDate),]
  pl <- ggplot2::ggplot(data=ftab, ggplot2::aes_string(x="CatchDate", y="LandedRoundWeight", group="group"))
  pl <- pl + ggplot2::geom_line(ggplot2::aes_string(col="group"))
  pl <- pl + ggplot2::ylab(paste("weight [", RstoxData::getUnit(ReportFdaLandingData$FisheriesLandings$LandedRoundWeight, property = "shortname"), "]", sep=""))
  pl <- pl + ggplot2::xlab("catch date")
  pl <- pl + ggplot2::ggtitle(paste(groupvars, collapse=","))
  
  return(pl)
}