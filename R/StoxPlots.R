#
# Plots that can be made from StoX-functions. Not necesarrily included in StoX
#

#' Plot landings
#' @description
#'  Plots landings by date of catch and by group
#' @param ReportFdaLandingData \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @seealso \code{\link[RstoxData]{ReportFdaLandings}}
#' @noRd
PlotFisheriesOverviewTemporal <- function(ReportFdaLandingData){
  
  if (!("CatchDate" %in% ReportFdaLandingData$GroupingVariables$GroupingVariables)){
    stop("Requires 'CatchDate' to be in the 'GroupingVariable' of 'ReportFdaLandingData'")
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
  
  pl <- pl + ggplot2::theme_minimal()
  
  return(pl)
}

#' Plot landings
#' @description
#'  Plots catch density of landings on polygons.
#' @param ReportFdaLandingData \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @param StratumPolygon \code{\link[RstoxBase]{StratumPolygon}}
#' @param AreaLabels if TRUE, labels with area codes are plotted on map.
#' @seealso \code{\link[RstoxData]{ReportFdaLandings}}
#' @noRd
PlotFisheriesOverviewSpatial <- function(ReportFdaLandingData, StratumPolygon, AreaLabels=F){
  
  if (!("Area" %in% ReportFdaLandingData$GroupingVariables$GroupingVariables) | length(ReportFdaLandingData$GroupingVariables$GroupingVariables)>1){
    stop("Requires 'Area' to be the only variable in the 'GroupingVariable' of 'ReportFdaLandingData'")
  }
  
  if (!all(ReportFdaLandingData$FisheriesLandings$Area %in% StratumPolygon$StratumName)){
    stop("The provided polygons does not include all areas in 'Area'.")
  }
  
  ftab <- ReportFdaLandingData$FisheriesLandings
  groupvars <- ReportFdaLandingData$GroupingVariables$GroupingVariables[ReportFdaLandingData$GroupingVariables$GroupingVariables != "Area"]
  
  sfPoly <- sf::st_as_sf(StratumPolygon)
  sfPoly$area <- sf::st_area(sfPoly)
  
  sfPoly <- merge(sfPoly, ReportFdaLandingData
                $FisheriesLandings, by.y="Area", by.x="StratumName", all.x=T)
  
  sfPoly$CatchDensity <- sfPoly$LandedRoundWeight / as.numeric(sfPoly$area)
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  bbox <- sf::st_bbox(sfPoly)
  pl <- ggplot2::ggplot(data=sfPoly)
  pl <- pl + ggplot2::geom_sf(data=world)
  pl <- pl + ggplot2::geom_sf(data=sfPoly, ggplot2::aes_string(fill="CatchDensity"), col="black")
  pl <- pl + ggplot2::xlim(c(bbox[["xmin"]], bbox[["xmax"]]))
  pl <- pl + ggplot2::ylim(c(bbox[["ymin"]], bbox[["ymax"]]))
  
  if (AreaLabels){
    labelPos <- suppressWarnings(cbind(sfPoly, sf::st_coordinates(sf::st_centroid(sfPoly))))
    pl <- pl + ggplot2::geom_label(data=labelPos, mapping=ggplot2::aes_string(x="X",y="Y",label="StratumName"))
  }
  
  pl <- pl + ggplot2::theme_minimal()

  return(pl)
}

#' Plot landings
#' @description
#'  Plots catch by group.
#' @param ReportFdaLandingData \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @seealso \code{\link[RstoxData]{ReportFdaLandings}}
#' @noRd
PlotFisheriesOverviewTable <- function(ReportFdaLandingData){
  
  ftab <- ReportFdaLandingData$FisheriesLandings
  groupvars <- ReportFdaLandingData$GroupingVariables$GroupingVariables
  
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
  
  ftab <- ftab[order(ftab$LandedRoundWeight, decreasing = T),]
  ftab$group <- factor(ftab$group, levels = ftab$group,  ordered = T)
  pl <- ggplot2::ggplot(data=ftab, ggplot2::aes_string(x="group", y="LandedRoundWeight"))
  pl <- pl + ggplot2::geom_col()
  pl <- pl + ggplot2::ylab(paste("weight [", RstoxData::getUnit(ReportFdaLandingData$FisheriesLandings$LandedRoundWeight, property = "shortname"), "]", sep=""))
  pl <- pl + ggplot2::xlab("")
  pl <- pl + ggplot2::ggtitle(paste(groupvars, collapse=","))
  pl <- pl + ggplot2::theme_minimal()
  if (nrow(ftab)>10){
    pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  
  
  return(pl)
  
}

#' @noRd
PlotCatcAtAgeTotals <- function(ReportFdaCatchAtAgeData){
  
  ReportFdaCatchAtAgeData$NbyAge <- ReportFdaCatchAtAgeData$NbyAge[order(ReportFdaCatchAtAgeData$NbyAge$Age),]
  levels <- ReportFdaCatchAtAgeData$NbyAge$AgeGroup[!duplicated(ReportFdaCatchAtAgeData$NbyAge$AgeGroup)]
  ReportFdaCatchAtAgeData$NbyAge$AgeGroup <- factor(ReportFdaCatchAtAgeData$NbyAge$AgeGroup, levels=levels, ordered=T)
  
  if (length(ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables)==0){
    ReportFdaCatchAtAgeData$NbyAge$Group <- "All"    
  }
  else{
    vars <- ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables
    g1 <- head(vars,1)
    vars <- vars[vars!=g1]
    ReportFdaCatchAtAgeData$NbyAge$Group <- ReportFdaCatchAtAgeData$NbyAge[[g1]]
    while(length(vars)>0){
      g <- head(vars,1)
      ReportFdaCatchAtAgeData$NbyAge$Group <- paste(ReportFdaCatchAtAgeData$NbyAge$Group, ReportFdaCatchAtAgeData$NbyAge[[g]], sep="/")
      vars <- vars[vars!=g]
    }
    
    ReportFdaCatchAtAgeData$NbyAge <- ReportFdaCatchAtAgeData$NbyAge[order(ReportFdaCatchAtAgeData$NbyAge$CatchAtAge),]
    ReportFdaCatchAtAgeData$NbyAge$Group <- factor(ReportFdaCatchAtAgeData$NbyAge$Group, ReportFdaCatchAtAgeData$NbyAge$Group[!duplicated(ReportFdaCatchAtAgeData$NbyAge$Group)], ordered=T)
  }

  
  pl <- ggplot2::ggplot(ReportFdaCatchAtAgeData$NbyAge, ggplot2::aes_string(x="AgeGroup", y="CatchAtAge", fill="Group"))
  pl <- pl + ggplot2::geom_col(position=ggplot2::position_dodge())
  pl <- pl + ggplot2::geom_errorbar(position=ggplot2::position_dodge(0.9), ggplot2::aes_string(ymin="Low", ymax="High"),width=0.8/(length(ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables)+1))
  pl <- pl + ggplot2::theme_minimal()
  pl <- pl + ggplot2::ylab(RstoxData::getUnit(ReportFdaCatchAtAgeData$NbyAge$CatchAtAge, property = "shortname"))
  pl <- pl + ggplot2::xlab("Age Group")
  pl <- pl + ggplot2::ggtitle("Catch At Age")
  
  return(pl)
}

#' Plot covariances (catch at age)
#' @description
#'  Plots covariances between age groups and other grouping variables catch at age.
#' @param ReportFdaLandingData \code{\link[RstoxFDA]{ReportFdaCatchAtAgeCovarianceData}}
#' @seealso \code{\link[RstoxData]{ReportRecaCatchAtAgeCovariance}}
#' @noRd
PlotCatcAtAgeCovariances <- function(ReportFdaCatchAtAgeCovarianceData){
  
  ReportFdaCatchAtAgeCovarianceData$Variables <- ReportFdaCatchAtAgeCovarianceData$Variables[order(ReportFdaCatchAtAgeCovarianceData$Variables$Age, ReportFdaCatchAtAgeCovarianceData$Variables$VariableId),]
  
  ReportFdaCatchAtAgeCovarianceData$CovarianceNbyAge$VariableId1 <- factor(ReportFdaCatchAtAgeCovarianceData$CovarianceNbyAge$VariableId1, levels=ReportFdaCatchAtAgeCovarianceData$Variables$VariableId, ordered = T)
  ReportFdaCatchAtAgeCovarianceData$CovarianceNbyAge$VariableId2 <- factor(ReportFdaCatchAtAgeCovarianceData$CovarianceNbyAge$VariableId2, levels=ReportFdaCatchAtAgeCovarianceData$Variables$VariableId, ordered = T)
  
  pl <- ggplot2::ggplot(data=ReportFdaCatchAtAgeCovarianceData$CovarianceNbyAge, ggplot2::aes_string(x="VariableId1", y="VariableId2", fill="Covariance"))
  pl <- pl + ggplot2::geom_tile()
  pl <- pl + ggplot2::theme_minimal()
  pl <- pl + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  pl <- pl + ggplot2::theme(axis.title = ggplot2::element_blank())
  pl <- pl + ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                             midpoint = 0, limit = c(min(ReportFdaCatchAtAgeCovarianceData$CovarianceNbyAge$Covariance),max(ReportFdaCatchAtAgeCovarianceData$CovarianceNbyAge$Covariance)))
  pl <- pl + ggplot2::ggtitle("Catch At Age")
  
  return(pl)
}