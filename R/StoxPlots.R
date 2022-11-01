#
# Plots that can be made from StoX-functions. Not necesarrily included in StoX
#

#' Plot landings
#' @description
#'  Plots landings by date of catch and by group
#' @param ReportFdaLandingData \code{\link[RstoxFDA]{ReportFdaLandingData}}
#' @seealso \code{\link[RstoxData]{ReportFdaLandings}}
#' @concept StoX-functions
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
#' @concept StoX-functions
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
  
  densityUnit <- paste(RstoxData::getUnit(ReportFdaLandingData$FisheriesLandings$LandedRoundWeight, property = "symbol"), "/ sq.", attributes(sfPoly$area)$units$numerator[[1]])
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  bbox <- sf::st_bbox(sfPoly)
  pl <- ggplot2::ggplot(data=sfPoly)
  pl <- pl + ggplot2::geom_sf(data=world)
  pl <- pl + ggplot2::geom_sf(data=sfPoly, ggplot2::aes_string(fill="CatchDensity"), col="black")
  pl <- pl + ggplot2::labs(fill=densityUnit)
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
#' @concept StoX-functions
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

#' Cell plot
#' @description 
#'  Makes 'cell plot' overview of samples and landings.
#' @details
#'  Each part or 'cell' of the fishery identified by the groupingvariables of a \code{\link[RstoxFDA]{ReportFdaSamplingData}} report
#'  is visualised as a cell on a two dimensional grid, and annotated with information about how much was landed in each 'cell'.
#'  Cells that has been sampled are also annotated with a triplet (#vessels, #catches, #individuals) indicating the number of
#'  sampled vessels, catches and individuals. The cells are also color coded to reflect how well they are sampled.
#'  
#'  The visualisation gives an overview of which part of the fishery has few samples, and may suggest ways to group or post-stratify
#'  variables for estimation. By extension it also suggests how supplemental sampling can be improved, for instance by revealing where 
#'  a small extra sampling effort prevents the need for grouping or post-stratifying.
#'  The plot does not provide a direct visualisation of the efficiency of the sampling. 
#'  Efficient sampling may well leave many cells unsampled, as the activity (e.g. volume landed) may be very different between cells.
#'  
#'  The color coding indicates five categories of sampling depending on how many vessels, catches, and individuals are sampled in the cell.
#'  #'  The coloring is controlled by the arguments 'MinVessels', 'MinCatches', and 'MinMeasurements':
#'  \describe{
#'   \item{Good}{Cell has sufficient number of individuals, catches and vessels sampled.}
#'   \item{Few vessels}{Cell has sufficient number of individuals and catches sampled, but not sufficient number of vessels}
#'   \item{Few catches}{Cell has sufficient number of individuals sampled, but not sufficient number of catches}
#'   \item{No samples}{Cell is not sampled}
#'   \item{No Landings}{Cell is sampled, but there are no official landings recorded for the cell.}
#'  }
#'  
#'  See the documentation for \code{\link[RstoxFDA]{ReportFdaSampling}} for an explanation for why the category 'No landings' may occur.
#'  The thresholds for what is considered sufficient sampling of vessels, catches and individuals, respectively, 
#'  is configured with the arguments 'MinVessels', 'MinCatches', 'MinMeasurements'
#'  
#' @param ReportFdaSamplingData \code{\link[RstoxFDA]{ReportFdaSamplingData}} with sampling report to plot
#' @param ColumnVariable The grouping variable in 'ReportFdaSamplingData' that should be used for columns in the cell plot
#' @param Measurement The kind of fish measurement that should be used to determine the color of a cell
#' @param MinVessels The minimum number of vessels sampled for a quality "Good" coloring of a cell. Defaults to 2.
#' @param MinCatches The minimum number of catches sampled for quality "Good" or "Few vessels" coloring of a cell. Defaults to 2.
#' @param MinMeasurements The minimum number of measurements (parameter 'Measurement') for quality "Good", "Few vessels" or "Few catches" coloring of a cell. Defaults to 100.
#' @param TextSize size of text in cellplot. Defaults to 2.
#' @concept StoX-functions
#' @noRd
PlotSamplingOverviewCell <- function(ReportFdaSamplingData, ColumnVariable, Measurement=c("AgeReadings","LengthMeasurements","WeightMeasurements"), MinVessels=integer(), MinCatches=integer(), MinMeasurements=integer(), TextSize=numeric()){
  if (!is.ReportFdaSamplingData(ReportFdaSamplingData)){
    stop("Input must be 'RstoxFDA:::ReportFdaSamplingData'")
  }
  if (nrow(ReportFdaSamplingData$GroupingVariables) == 0){
    stop("Cell plot can only be constructed when sampling report has grouping variables")
  }
  if (!(ColumnVariable %in% ReportFdaSamplingData$GroupingVariables$GroupingVariables)){
    stop("'ColumnVariable' must be one of the variables in 'GroupingVariables'")
  }
  if (nrow(ReportFdaSamplingData$SamplingVariables) != 0){
    stop("Cell plot cannot be constructed when sampling report has sampling variables")
  }

  Measurement <- match.arg(Measurement, Measurement)
  if (isGiven(Measurement)){
    if (!Measurement %in% c("AgeReadings","LengthMeasurements","WeightMeasurements")){
      stop(paste("Does not recognize option for measurement:", Measurement))
    }
  }
  else{
    Measurement <- "AgeReadings"
  }

  if (!isGiven(MinVessels)){
    MinVessels <- 2
  }
  if (!isGiven(MinCatches)){
    MinCatches <- 2
  }
  if (!isGiven(MinMeasurements)){
    MinMeasurements <- 100
  }
  if (!isGiven(TextSize)){
    TextSize <- 2
  }
  
  RowVariables <- ReportFdaSamplingData$GroupingVariables$GroupingVariables[ReportFdaSamplingData$GroupingVariables$GroupingVariables != ColumnVariable]
  if (length(RowVariables) == 0){
    RowVariables <- ("NA")
    ReportFdaSamplingData$FisheriesSampling[[RowVariables]] <- ""
  }
  
  RowAxisLabel <- paste(RowVariables, collapse = "-")
  ReportFdaSamplingData$FisheriesSampling$RowLabels <- apply(ReportFdaSamplingData$FisheriesSampling[,.SD, .SDcols=RowVariables], FUN=function(x){paste(x, collapse="-")}, MARGIN = 1)
  ReportFdaSamplingData$FisheriesSampling$Samples <- "No samples"
  ReportFdaSamplingData$FisheriesSampling$Samples[ReportFdaSamplingData$FisheriesSampling[[Measurement]] >= MinMeasurements] <- "Few catches"
  ReportFdaSamplingData$FisheriesSampling$Samples[ReportFdaSamplingData$FisheriesSampling$Catches >= MinCatches & ReportFdaSamplingData$FisheriesSampling$Samples=="Few catches"] <- "Few vessels"
  ReportFdaSamplingData$FisheriesSampling$Samples[ReportFdaSamplingData$FisheriesSampling$Vessels >= MinVessels & ReportFdaSamplingData$FisheriesSampling$Samples=="Few vessels"] <- "Good"
  ReportFdaSamplingData$FisheriesSampling$Samples[is.na(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight)] <- "No Landings"
  
  ReportFdaSamplingData$FisheriesSampling$Text <- ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight
  filterSampled <- ReportFdaSamplingData$FisheriesSampling$Samples != "No samples"
  ReportFdaSamplingData$FisheriesSampling$Text[filterSampled] <- paste(ReportFdaSamplingData$FisheriesSampling$Text[filterSampled], apply(ReportFdaSamplingData$FisheriesSampling[filterSampled,.SD, .SDcols=c("Vessels", "Catches", Measurement)], FUN=function(x){paste(x, collapse=",")}, MARGIN = 1), sep="\n")
                                                    
  pl <- ggplot2::ggplot(ReportFdaSamplingData$FisheriesSampling, ggplot2::aes_string(ColumnVariable, "RowLabels")) +
    ggplot2::geom_tile(ggplot2::aes_string(fill="Samples"), color="grey") +
    ggplot2::coord_equal() +
    ggplot2::geom_text(ggplot2::aes_string(label="Text"), size=TextSize) +
    ggplot2::ylab(RowAxisLabel) +
    ggplot2::ggtitle(paste("Landed weight (", RstoxData::getUnit(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, property = "symbol"),")",sep=""), paste(Measurement, ": #Vessels, #Catches, #Individuals", sep="")) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(
      values = c(
        "No samples" = "#ffffcc",
        "Few catches" = "#c2e699",
        "Few vessels" = "#78c679",
        "Good" = "#238443",
        "No Landings" = "white"
      )
    )
  
  return(pl)
}

#' Coverage plot
#' @description 
#'  Plots a barplot of landings with color codes for how much each part of the fishery is sampled.
#' @details 
#'  Plots an ordered barplot of landings for each part of the fishery identified by the grouping variables of 'ReportFdaSamplingData'.
#'  This visualizes the efficiency of the sampling, in the sense that sampling intensity is compared with total landings.
#'  
#'  The bars, each representing a part of the fishery are colored to representing the sampling of each part, according to one of the following color schemes:
#'  
#'  ### color scheme 'Gradient'
#'  The color scheme 'Gradient' colors the bars according to how many sampling units are sampled.
#'  The sampling unit counted is controlled by the argument 'SamplingUnit':
#'  \describe{
#'   \item{"Vessels"}{The gradient reflect the number of vessels sampled}
#'   \item{"Cacthes"}{The gradient reflect the number of cacthes sampled}
#'   \item{"Measurements"}{The gradient reflect the number of measurements taken (see argment 'Measurement')}
#'  }
#'  
#'  ### color scheme 'Cellplot'
#'  The color scheme "CellPlot" colors the bars similar to the color scheme used in \code{\link[RstoxFDA]{PlotSamplingOverviewCell}}.
#'  It uses a color coding that indicates five categories of sampling 
#'  depending on how many vessels, catches, and individuals are sampled in the cell.
#'  The coloring is controlled by the arguments 'MinVessels', 'MinCatches', and 'MinMeasurements':
#'  \describe{
#'   \item{Good}{Cell has sufficient number of individuals, catches and vessels sampled.}
#'   \item{Few vessels}{Cell has sufficient number of individuals and catches sampled, but not sufficient number of vessels}
#'   \item{Few catches}{Cell has sufficient number of individuals sampled, but not sufficient number of catches}
#'   \item{No samples}{Cell is not sampled}
#'  }
#' @param ReportFdaSamplingData \code{\link[RstoxFDA]{ReportFdaSamplingData}} with sampling report to plot
#' @param Cumulative logical indicating if the cumulative fraction of the landed weight should be plotted on a secondary axis.
#' @param ColorScheme 'CellPlot' or 'Gradient'. See details.
#' @param Measurement The kind of fish measurement that should be used to determine the color of a cell
#' @param MinVessels For color scheme "CellPlot". The minimum number of vessels sampled for a quality "Good" coloring of a cell. Defaults to 2.
#' @param MinCatches color scheme "CellPlot". The minimum number of catches sampled for quality "Good" or "Few vessels" coloring of a cell. Defaults to 2.
#' @param MinMeasurements color scheme "CellPlot". The minimum number of measurements (parameter 'Measurement') for quality "Good", "Few vessels" or "Few catches" coloring of a cell. Defaults to 100.
#' @param SamplingUnit color scheme "Gradient". The sampling unit used: "Vessels","Catches", or "Measurement"
#' @concept StoX-functions
#' @md
#' @noRd
PlotSamplingCoverage <- function(ReportFdaSamplingData, Cumulative=FALSE, ColorScheme=c("CellPlot"), Measurement=c("AgeReadings","LengthMeasurements","WeightMeasurements"), MinVessels=integer(), MinCatches=integer(), MinMeasurements=integer(), SamplingUnit=c("Vessels","Catches","Measurements")){
  
  if (!is.ReportFdaSamplingData(ReportFdaSamplingData)){
    stop("Input must be 'RstoxFDA:::ReportFdaSamplingData'")
  }
  if (nrow(ReportFdaSamplingData$GroupingVariables) == 0){
    stop("Coverage plot can only be constructed when sampling report has grouping variables")
  }
  if (nrow(ReportFdaSamplingData$SamplingVariables) != 0){
    stop("Coverage plot cannot be constructed when sampling report has sampling variables")
  }
  
  ColorScheme <- match.arg(ColorScheme, ColorScheme)
  if (!isGiven(ColorScheme)){
    stop("Argument 'ColorScheme' must be provided")
  }
  
  Measurement <- match.arg(Measurement, Measurement)
  if (isGiven(Measurement)){
    if (!Measurement %in% c("AgeReadings","LengthMeasurements","WeightMeasurements")){
      stop(paste("Does not recognize option", Measurement, "for 'Measurement'"))
    }
  }
  else{
    Measurement <- "AgeReadings"
  }
  
  SamplingUnit <- match.arg(SamplingUnit, SamplingUnit)
  if (!isGiven(SamplingUnit) & ColorScheme == "Gradient"){
    stop("Argument 'Measurement' must be provided for color scheme 'Gradient'.")
  }
  if (!(SamplingUnit %in% c("Vessels","Catches","Measurements"))){
    stop(paste("Does not recognize option", SamplingUnit, "for 'SamplingUnit'"))
  }
  if (SamplingUnit == "Measurements"){
    SamplingUnit <- Measurement
  }
  
  if (!isGiven(MinVessels)){
    MinVessels <- 2
  }
  if (!isGiven(MinCatches)){
    MinCatches <- 2
  }
  if (!isGiven(MinMeasurements)){
    MinMeasurements <- 100
  }
  
  axisLabel <- paste(ReportFdaSamplingData$GroupingVariables$GroupingVariables, collapse = "-")
  ReportFdaSamplingData$FisheriesSampling$axisLabel <- apply(ReportFdaSamplingData$FisheriesSampling[,.SD, .SDcols=ReportFdaSamplingData$GroupingVariables$GroupingVariables], FUN=function(x){paste(x, collapse="-")}, MARGIN = 1)
  ReportFdaSamplingData$FisheriesSampling$Samples <- "No samples"
  ReportFdaSamplingData$FisheriesSampling$Samples[ReportFdaSamplingData$FisheriesSampling[[Measurement]] >= MinMeasurements] <- "Few catches"
  ReportFdaSamplingData$FisheriesSampling$Samples[ReportFdaSamplingData$FisheriesSampling$Catches >= MinCatches & ReportFdaSamplingData$FisheriesSampling$Samples=="Few catches"] <- "Few vessels"
  ReportFdaSamplingData$FisheriesSampling$Samples[ReportFdaSamplingData$FisheriesSampling$Vessels >= MinVessels & ReportFdaSamplingData$FisheriesSampling$Samples=="Few vessels"] <- "Good"
  
  ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight[is.na(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight)]<-0
  
  ReportFdaSamplingData$FisheriesSampling <- ReportFdaSamplingData$FisheriesSampling[order(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, decreasing = T),]
  ReportFdaSamplingData$FisheriesSampling$cumSumPercent <- 100*cumsum(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight)/(sum(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight))
  ReportFdaSamplingData$FisheriesSampling$cumSumPercent <- ReportFdaSamplingData$FisheriesSampling$cumSumPercent*(max(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, na.rm = T)/100)
  ReportFdaSamplingData$FisheriesSampling$axisLabel <- factor(ReportFdaSamplingData$FisheriesSampling$axisLabel, levels=ReportFdaSamplingData$FisheriesSampling$axisLabel, ordered = T)
  
  if (ColorScheme == "CellPlot"){
    pl <- ggplot2::ggplot(ReportFdaSamplingData$FisheriesSampling, ggplot2::aes_string("axisLabel", "LandedRoundWeight")) +
      ggplot2::geom_col(ggplot2::aes_string(fill="Samples")) +
      ggplot2::xlab(axisLabel) +
      ggplot2::ylab(paste("Landed weight (", RstoxData::getUnit(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, property = "symbol"),")",sep="")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::scale_fill_manual(
        values = c(
          "No samples" = "#ffffcc",
          "Few catches" = "#c2e699",
          "Few vessels" = "#78c679",
          "Good" = "#238443"
        )
      )
  }
  else if (ColorScheme == "Gradient"){
    if (any(is.na(ReportFdaSamplingData$FisheriesSampling[[SamplingUnit]]))){
      ReportFdaSamplingData$FisheriesSampling[[SamplingUnit]][is.na(ReportFdaSamplingData$FisheriesSampling[[SamplingUnit]])] <- 0      
    }
    pl <- ggplot2::ggplot(ReportFdaSamplingData$FisheriesSampling, ggplot2::aes_string("axisLabel", "LandedRoundWeight")) +
      ggplot2::geom_col(ggplot2::aes_string(fill=SamplingUnit)) +
      ggplot2::xlab(axisLabel) +
      ggplot2::ylab(paste("Landed weight (", RstoxData::getUnit(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, property = "symbol"),")",sep="")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::scale_fill_gradient2(low="white", mid="#F6FAAF", high="#238443")
  }
  else{
    stop(paste("Color scheme", ColorScheme, "is not recognized."))
  }

  
  # add secondary scale with cumulative catches in %
  if (Cumulative){
    coeff <- max(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight)/100
    pl <- pl + ggplot2::geom_line(ggplot2::aes_string(y="cumSumPercent"), group=1) + 
      ggplot2::scale_y_continuous(
        
        # Features of the first axis
        name = paste("Landed weight (", RstoxData::getUnit(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, property = "symbol"),")",sep=""),
        
        # Add a second axis and specify its features
        sec.axis = ggplot2::sec_axis(~.x/coeff, name="Landed weight (cumulative %)")
      )  
  }
  
  
  
  return(pl)
  
}

#' Plot sampling variables
#' @description 
#'  Plot a stacked barplot of sampling variables for each part of the fishery, with total landings on a secondary axis.
#' @param ReportFdaSamplingData \code{\link[RstoxFDA]{ReportFdaSamplingData}} with sampling report to plot
#' @param Quantity the quantity to plot for each sampling variable: "Catches", "Vessels", "WeightMeasurements", "LengthMeasurements", "AgeReadings", or "WeightOfSampledCatches"
#' @concept StoX-functions
#' @noRd
PlotSamplingVariables <- function(ReportFdaSamplingData, Quantity=c("Catches", "Vessels", "WeightMeasurements", "LengthMeasurements", "AgeReadings", "WeightOfSampledCatches")){
  
  Quantity <- match.arg(Quantity, Quantity)
  
  if (!isGiven(Quantity)){
    stop("Argument 'Quantity' must be provided")
  }
  if (!(Quantity %in% c("Catches", "Vessels", "WeightMeasurements", "LengthMeasurements", "AgeReadings", "WeightOfSampledCatches"))){
    stop(paste("Does not recognize option", Quantity, "for 'Quantity'"))
  }
  
  ReportFdaSamplingData$FisheriesSampling$cell <- apply(ReportFdaSamplingData$FisheriesSampling[,.SD, .SDcols=ReportFdaSamplingData$GroupingVariables$GroupingVariables], FUN=function(x){paste(x, collapse="-")}, MARGIN = 1)
  ReportFdaSamplingData$FisheriesSampling$SamplingVariable <- apply(ReportFdaSamplingData$FisheriesSampling[,.SD, .SDcols=ReportFdaSamplingData$SamplingVariables$SamplingVariables], FUN=function(x){paste(x, collapse="-")}, MARGIN = 1)
  ReportFdaSamplingData$FisheriesSampling$SamplingVariable[is.na(ReportFdaSamplingData$FisheriesSampling$SamplingVariable)] <- 0
  ReportFdaSamplingData$FisheriesSampling[[Quantity]][is.na(ReportFdaSamplingData$FisheriesSampling[[Quantity]])] <- 0
  
  ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight[is.na(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight)] <- 0
  ReportFdaSamplingData$FisheriesSampling <- ReportFdaSamplingData$FisheriesSampling[order(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, decreasing=T),]
  ReportFdaSamplingData$FisheriesSampling$cell <- factor(ReportFdaSamplingData$FisheriesSampling$cell, levels=ReportFdaSamplingData$FisheriesSampling$cell[!duplicated(ReportFdaSamplingData$FisheriesSampling$cell)], ordered = T)
  
  samplingVariableLabel <- paste(ReportFdaSamplingData$SamplingVariables$SamplingVariables, collapse = "-")
  cellLabel <- paste(ReportFdaSamplingData$GroupingVariables$GroupingVariables, collapse = "-")
  tab <- ReportFdaSamplingData$FisheriesSampling[,.SD, .SDcol=c("cell", "SamplingVariable", Quantity)]
  
  if (length(unique(tab$cell))==1){
    pl <- ggplot2::ggplot(tab, ggplot2::aes_string("SamplingVariable", Quantity)) +
      ggplot2::geom_col(ggplot2::aes_string(fill="SamplingVariable"), group=1) +
      ggplot2::xlab(samplingVariableLabel) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::ggtitle(tab$cell[[1]])
    return(pl)
  }
  
  pl <- ggplot2::ggplot(tab, ggplot2::aes_string("cell", Quantity)) +
    ggplot2::geom_col(ggplot2::aes_string(fill="SamplingVariable")) +
    ggplot2::xlab(cellLabel) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::guides(fill=ggplot2::guide_legend(title=samplingVariableLabel))
  
  
  #
  # add secondary axis w landings
  #
  landings <- ReportFdaSamplingData$FisheriesSampling[,.SD, .SDcol=c("cell", "LandedRoundWeight")]
  landings <- landings[!duplicated(landings$cell),]
  
  barsizes <- ReportFdaSamplingData$FisheriesSampling[,list(barsize=sum(get(Quantity))), list(cell=get("cell"))]
  maxbarsize <- max(barsizes$barsize)
  
  coeff <- max(landings$LandedRoundWeight, na.rm=T) / maxbarsize
  landings$scaledLandings <- landings$LandedRoundWeight / coeff
  
  
  pl <- pl + ggplot2::geom_line(ggplot2::aes_string(y="scaledLandings"), landings, group=1) + 
    ggplot2::scale_y_continuous(
      
      # Features of the first axis
      name = Quantity,
      
      # Add a second axis and specify its features
      sec.axis = ggplot2::sec_axis(~.x*coeff, name=paste("Landed weight (", RstoxData::getUnit(ReportFdaSamplingData$FisheriesSampling$LandedRoundWeight, property = "symbol"),")",sep=""))
    )  
  
  return(pl)
}

#' @concept StoX-functions
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
    g1 <- utils::head(vars,1)
    vars <- vars[vars!=g1]
    ReportFdaCatchAtAgeData$NbyAge$Group <- ReportFdaCatchAtAgeData$NbyAge[[g1]]
    while(length(vars)>0){
      g <- utils::head(vars,1)
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

#' Plot mean of a variable for each age group, along with dashed line for the interval (Low / High)
#' 
#' @noRd
PlotMeanVariableAtAge <- function(ReportFdaVariableAtAgeData, tableName="MeanWeightByAge", variable="MeanIndividualWeight", ylabel=variable){
  
  if (nrow(ReportFdaVariableAtAgeData$GroupingVariables) == 0){

    pl <- ggplot2::ggplot(ReportFdaVariableAtAgeData[[tableName]], ggplot2::aes(group=1)) + 
      ggplot2::geom_line(ggplot2::aes_string(x="AgeGroup", y=variable), linetype="solid") +
      ggplot2::geom_line(ggplot2::aes_string(x="AgeGroup", y="High"), linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes_string(x="AgeGroup", y="Low"), linetype="dashed")
  }
  else{
  
    groupLabel <- paste(ReportFdaVariableAtAgeData$GroupingVariables$GroupingVariables, collapse = "-")
    ReportFdaVariableAtAgeData[[tableName]]$group <- apply(ReportFdaVariableAtAgeData[[tableName]][,.SD, .SDcols=ReportFdaVariableAtAgeData$GroupingVariables$GroupingVariables], FUN=function(x){paste(x, collapse="-")}, MARGIN = 1)
    pl <- ggplot2::ggplot(ReportFdaVariableAtAgeData[[tableName]]) + 
      ggplot2::geom_line(ggplot2::aes_string(x="AgeGroup", y=variable, group="group", color="group"), linetype="solid") +
      ggplot2::geom_line(ggplot2::aes_string(x="AgeGroup", y="High", group="group", color="group"), linetype="dashed") +
      ggplot2::geom_line(ggplot2::aes_string(x="AgeGroup", y="Low", group="group", color="group"), linetype="dashed") +
      ggplot2::guides(color=ggplot2::guide_legend(title=groupLabel))
  
  }
  
  pl <- pl + ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::ylab(ylabel)
  
  return(pl)
}

#' Plot weight at age
#' @description
#'  Plot mean individual weight for each age group, along with an interval rerpesentation for the error of the mean.
#'  The interval plotted is as configured in 'ReportFdaWeightAtAgeData' (the interval Low / High) and is represented by dashed lines.
#'  
#'  The interval does not represent the range of length occuring in each age group, but the error of the estiamte of mean weight
#'  
#'  If any grouping variables are configued for argument 'ReportFdaWeightAtAgeData', groups will be plotted in different colors.
#' @param ReportFdaWeightAtAgeData \code{\link[RstoxFDA]{ReportFdaWeightAtAgeData}} with mean weight statistics from Reca simulations
#' @concept StoX-functions
#' @concept convergence-checks
#' @noRd
PlotMeanWeightAtAge <- function(ReportFdaWeightAtAgeData){
 if (!is.ReportFdaByAgeData(ReportFdaWeightAtAgeData)){
   stop("Malformed argument: 'ReportFdaWeightAtAgeData'")
 }
 return(PlotMeanVariableAtAge(ReportFdaWeightAtAgeData, "MeanWeightByAge", "MeanIndividualWeight", ylabel = paste("mean individual weight (",RstoxData::getUnit(ReportFdaWeightAtAgeData$MeanWeightByAge$MeanIndividualWeight, property = "symbol"),")",sep="")))
}

#' Plot length at age
#' @description
#'  Plot mean individual length for each age group, along with an interval rerpesentation for the error of the mean.
#'  The interval plotted is as configured in 'ReportFdaLengthAtAgeData' (the interval Low / High) and is represented by dashed lines.
#'  
#'  The interval does not represent the range of weights occuring in each age group, but the error of the estimate of mean length
#'  
#'  If any grouping variables are configued for argument 'ReportFdaLengthAtAgeData', groups will be plotted in different colors.
#' @param ReportFdaLengthAtAgeData \code{\link[RstoxFDA]{ReportFdaLengthAtAgeData}} with mean weight statistics from Reca simulations
#' @concept StoX-functions
#' @concept convergence-checks
#' @noRd
PlotMeanLengthAtAge <- function(ReportFdaLengthAtAgeData){
  if (!is.ReportFdaByAgeData(ReportFdaLengthAtAgeData)){
    stop("Malformed argument: 'ReportFdaLengthAtAgeData'")
  }
  return(PlotMeanVariableAtAge(ReportFdaLengthAtAgeData, "MeanLengthByAge", "MeanIndividualLength", ylabel = paste("mean length (",RstoxData::getUnit(ReportFdaLengthAtAgeData$MeanLengthByAge$MeanIndividualLength, property = "symbol"),")",sep="")))
}

#' Plot covariances (catch at age)
#' @description
#'  Plots covariances between age groups and other grouping variables catch at age.
#' @param ReportFdaLandingData \code{\link[RstoxFDA]{ReportFdaCatchAtAgeCovarianceData}}
#' @seealso \code{\link[RstoxData]{ReportRecaCatchAtAgeCovariance}}
#' @concept StoX-functions
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

#' Reca Traceplots
#' @description 
#'  Plots the posterior distribution of parameters estimated with Reca.
#'  Useful to corroborate convergence of simulations.
#'  
#'  The distribution is plotted in order of iterations, so that issues with autocorrelation in the simulation can be detected.
#'  Autocorrelation issues may be addressed by adjusting the argument 'Thin' to \code{\link[RstoxFDA]{ParameterizeRecaModels}}
#'  The upper and lower quantiles of the distributions are highlighted, so that rare spikes or multi-modalities can be detected.
#'  Rare spikes and multi-modalities can bed addressed by adjusting the argument 'Burnin' to \code{\link[RstoxFDA]{ParameterizeRecaModels}}
#'  
#'  In order to provide an overview of many age-groups at once. Several panels are created and age-groups that
#'  have closer mean values are plotted together. This is achieved with a k-means clustering (\code{\link[stats]{kmeans}}),
#'  and some key parameters for the clustering algorithm is provided as options Nclust, Iter.max, Nstart, 
#'  
#'  Any grouping variables or length groups in 'RecaCatchAtAge' are incorporated into the age group definition.
#'  This tends to crowd the plots, and may make them unreadable. While it is desirable to ensure that the parameter has convergened
#'  for all ages, grouping variables and length groups, it is often necessary to compromise. One may
#'  \describe{
#'   \item{increase 'LengthInterval'}{Default is to collapse length groups entirely}
#'   \item{adjust 'PlusGroup'}{Reduces the total number of age groups}
#'   \item{adjust 'CatLimit'}{which removes legends from the most crowded plots}
#'   \item{remove grouping variables}{make additional estimates for the same parameterisation, with fewer grouping variables. See: \code{\link[RstoxFDA]{RunRecaModels}}}
#'  }
#'  If 'LengthInterval' specifies only one length-group. Length groups will be removed from plot legends.
#'  
#'  Additional convergence checks can be set up using several parameterisation runs and 
#'  the functions \code{\link[RstoxFDA]{ReportRecaParameterStatistics}} and \code{\link[RstoxFDA]{ReportParameterConvergence}}.
#'  That analysis checks the convergence of model parameters, rather than the estimated parameters,
#'  and supports handling a large number of model parameters, and filter out indications of non-convergence.
#' @param RecaCatchAtAge Results from MCMC simulations (\code{\link[RstoxFDA]{RecaCatchAtAge}}).
#' @param Parameter which parameter plot traceplots for "TotalCatch", "MeanLength", or "MeanWeight", Defaults to TotalCatch
#' @param PlusGroup If given, ages 'PlusGroup' or older are included in a plus group.
#' @param LengthInterval width of length bins in cm, for TotalCatch traceplots. If not provided, length inteval will be set to the maximum length group..
#' @param Nclust the number of plots to distribute the ages and plus group on. Defaults to 4
#' @param Iter.max maximal number of iterations for k-means clustering deciding which ages are plotted in same plot. Defaults to 20.
#' @param Nstart the number of random sets chosen for the k-means clustering. Defaults to 10.
#' @param LowerLuant lower quantile in each age group to plot as points. Defaults to 0.05.
#' @param UpperQuant upper quantile in each age group to plot as points. Defaults to 0.95
#' @param CatLimit the upper limit for number of ages in a plot using categorical coloring. Plots with more than this number of age greoups will use a gradient coloring scheme. Defaults to 8.
#' @param LegendLimit the upper limit for number of ages in a plot showing legends. Plots with more than this number of age groups will not show plot legend. Defaults to 8.
#' @concept StoX-functions
#' @concept convergence-checks
#' @noRd
#' @md
PlotPosteriorTraces <- function(RecaCatchAtAge, 
                                Parameter=c("TotalCatch", "MeanLength", "MeanWeight"), 
                                PlusGroup=integer(), 
                                LengthInterval=numeric(),
                                Nclust=integer(), 
                                Iter.max=integer(), 
                                Nstart=integer(), 
                                LowerQuant=numeric(), 
                                UpperQuant=numeric(), 
                                CatLimit=integer(),
                                LegendLimit=integer()){
  
  if (!is.RecaCatchAtAge(RecaCatchAtAge)){
    stop("'RecaCatchAtAge' is not correctly formatted.")
  }
  
  Parameter <- match.arg(Parameter, Parameter)
  
  if (isGiven(PlusGroup)){
    if (PlusGroup > max(RecaCatchAtAge$CatchAtAge$Age)){
      stop("'PlusGroup' is larger than the oldest age in the model.")
    }
    if (PlusGroup < min(RecaCatchAtAge$CatchAtAge$Age)){
      stop("'PlusGroup' is smaller than the smallest age in the model.")
    }
  }
  
  if (!isGiven(LengthInterval)){
    LengthInterval <- max(RecaCatchAtAge$CatchAtAge$Length)
  }
  if (!isGiven(Nclust)){
    Nclust <- 4
  }
  if (!isGiven(Iter.max)){
    Iter.max <- 20
  }
  if (!isGiven(Nstart)){
    Nstart <- 10
  }
  if (!isGiven(LowerQuant)){
    LowerQuant <- .05
  }
  if (!isGiven(UpperQuant)){
    UpperQuant <- .95
  }
  if (!isGiven(CatLimit)){
    CatLimit <- 8
  }
  if (!isGiven(LegendLimit)){
    LegendLimit <- 8
  }
  
  if (Parameter == "TotalCatch"){
    
    tab <- RecaCatchAtAge$CatchAtAge
    var <- "CatchAtAge"
    
    tab <- setLengthGroup(tab, LengthInterval)
    tab <- setAgeGroup(tab)
    
    aggNames <- c("Iteration", "AgeGroup", "LengthGroup", RecaCatchAtAge$GroupingVariables$GroupingVariables)
    if (isGiven(PlusGroup)){
      tab$AgeGroup[tab$Age>=PlusGroup] <- paste("Age ", PlusGroup, "+", sep="")
    }
    tab <- tab[, list(CatchAtAge=sum(get("CatchAtAge"))), by=aggNames]
    if (length(unique(tab$LengthGroup))==1){
      tab$LengthGroup <- NULL
    }
    
  }
  else if (Parameter == "MeanLength"){
    
    if (isGiven(PlusGroup)){
      tab <- getPlusGroupMeans(RecaCatchAtAge, "MeanLength", "MeanIndividualLength", PlusGroup)      
    }
    else{
      tab <- RecaCatchAtAge$MeanLength      
    }
    var <- "MeanIndividualLength"
    
  }
  else if (Parameter == "MeanWeight"){
    
    if (isGiven(PlusGroup)){
      tab <- getPlusGroupMeans(RecaCatchAtAge, "MeanWeight", "MeanIndividualWeight", PlusGroup)      
    }
    else{
      tab <- RecaCatchAtAge$MeanWeight      
    }
    var <- "MeanIndividualWeight"
    
  }
  else{
    stop(paste("Does not recogize option for 'Paramater':", Parameter))
  }
  
  groupIdVars <- names(tab)[!(names(tab) %in% c("Iteration", var))]
  tab$AgeGroup <- apply(tab[,.SD, .SDcols=groupIdVars], FUN=function(x){paste(x, collapse="-")}, MARGIN = 1)
  tab <- tab[,.SD, .SDcols=c("Iteration", "AgeGroup", var)]
  
  #
  # K means clustering
  #
  
  if (Nclust > length(unique(tab$AgeGroup))){
    stop("Choose nclust to be lower than the number of age group traces to plot.")
  }
  tabWide <- data.table::dcast(tab, Iteration~AgeGroup, value.var=var, fun=sum)
  tabWide$Iteration <- NULL
  mat <- as.matrix(tabWide)
  
  means <- apply(mat, FUN=mean, MARGIN=2)
  
  #clustering ages in plots. kemans on log(means) seems to work well, but sometimes failes due to 0 means, which is avoided by adding lowest non-zero mean
  llo <- min(means[means>0])
  clust <- stats::kmeans(log(means+llo), Nclust, iter.max = Iter.max, nstart = Nstart)
  
  
  ageGroupLabel <- paste(groupIdVars, collapse="-")
  Agecolors <- c(RColorBrewer::brewer.pal(8, "Accent"), RColorBrewer::brewer.pal(9, "Set1"), RColorBrewer::brewer.pal(8, "Dark2"), RColorBrewer::brewer.pal(8, "Set3"))
  
  lq <- tab[,list(lq=stats::quantile(get(var), LowerQuant)), list(AgeGroup=get("AgeGroup"))]
  uq <- tab[,list(uq=stats::quantile(get(var), UpperQuant)), list(AgeGroup=get("AgeGroup"))]
  tab <- merge(tab, lq)
  tab <- merge(tab, uq)
  
  #
  # Plots
  #
  
  plots <- list()
  plotnr <- 1
  for (i in seq(1,Nclust)[order(clust$centers, decreasing = T)]){
    mcp <- tab[tab$AgeGroup %in% names(clust$cluster[clust$cluster==i]),]
    maxy <- max(mcp[[var]]) + max(mcp[[var]])*.1
    
    if (sum(clust$cluster==i)<=CatLimit){
      mcp$AgeGroup <- as.factor(mcp$AgeGroup)
      plots[[plotnr]]<-ggplot2::ggplot(data=mcp, ggplot2::aes_string(x="Iteration", y=var, group="AgeGroup"))+
        ggplot2::geom_line(ggplot2::aes_string(color="AgeGroup")) + 
        ggplot2::geom_point(data=mcp[mcp[[var]] > mcp$uq | mcp[[var]] < mcp$lq,], ggplot2::aes_string(color="AgeGroup")) + 
        ggplot2::scale_color_manual(values = Agecolors) + ggplot2::ylim(0,maxy)
    }
    else{
      mcp$AgeGroup <- as.numeric(as.factor(mcp$AgeGroup))
      plots[[plotnr]]<-ggplot2::ggplot(data=mcp, ggplot2::aes_string(x="Iteration", y=var, group="AgeGroup"))+
        ggplot2::geom_line(data=mcp, ggplot2::aes_string(color="AgeGroup")) + 
        ggplot2::geom_point(data=mcp[mcp[[var]] > mcp$uq | mcp[[var]] < mcp$lq,], ggplot2::aes_string(color="AgeGroup")) + 
        ggplot2::ylim(0,maxy) +
        ggplot2::scale_color_gradient()
    }
    plots[[plotnr]] <- plots[[plotnr]] + ggplot2::theme_minimal() +
        ggplot2::ylab(NULL) +
        ggplot2::theme(axis.ticks.x=ggplot2::element_blank(), axis.text.x=ggplot2::element_blank())
    
    if (sum(clust$cluster==i)>LegendLimit){
      plots[[plotnr]] <- plots[[plotnr]] + ggplot2::guides(color = "none")
    }
    
    plotnr <- plotnr+1
  }
  gridExtra::grid.arrange(grobs=plots, top=grid::textGrob(paste("Traceplot", Parameter),gp=grid::gpar(fontsize=20,font=1)), ncol=2)
  
}
