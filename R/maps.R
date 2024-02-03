
#' Plot area definitions on a map
#' @description
#'  Plots area polygons on a map, may plot a set of positions as well.
#' @details
#'  Map ranges to plot are specified by 'xlim' and 'ylim'. These should always be given in lon/lat.
#'  if these are not given they will be derived in order to include all positions in 'data'.
#'  if 'data is not given they will be derived from the bounding box of 'areaDef'.
#'  To allow for flexible projection, some space will be added around 'xlim' and 'ylim'.
#'  
#'  Both 'areaDef' and 'data' are optional, so the function may be used to plot position without an area defintion, 
#'  or even to just plot maps if xlim and ylim are provided.
#'  
#'  Colors can be specified as understood by ggplot2. E.g. one of those listed by \code{\link[grDevices]{colors}}.
#'  
#' @param data data.frame with any points to be plotted
#' @param latCol character() identifing column in 'data' that specify latitudes (WGS84)
#' @param lonCol character() identifing column in 'data' that specify longitudes (WGS84)
#' @param groupCol character() identifying column in 'daat' that specify grouping of points
#' @param areaDef \code{\link[sf]{sf}} data.frame
#' @param areaNameCol identifies column in 'areaDef' with label names for the areas
#' @param areaLabels logical whether to plot area labels
#' @param xlim x axis limits in degrees longitude
#' @param ylim y axis limits in degrees latitude
#' @param areaLabelSize size for any area labels
#' @param pointColor color of any points to be plotted.
#' @param pointShape ggplot2 shape for any points to be plotted
#' @param pointSize size for any points to be plotted
#' @param title plot title
#' @param projection proj4string or EPSG code specifying the desired projection, see \code{\link[sf]{st_crs}}. Defaults to mercator projection.
#' @param polygonColor color to be used for plotting polygons.
#' @examples
#'  # plot ICES areas with default projection
#'  data(ICESareas)
#'  plotArea(areaDef=ICESareas)
#'
#'  data(mainareaFdir2018)
#'  data(NAFOareas)
#'  # plot mainarea and NAFO areas combined in a Lambert Conformal Conic projection.
#'  plotArea(title="Main area + NAFO",
#'           areaDef=rbind(mainareaFdir2018[,c("StratumName")],
#'                   NAFOareas[,c("StratumName")]),
#'           projection="+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 
#'           +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")
#' @concept spatial coding functions
#' @importFrom ggplot2 .data
#' @export
plotArea <- function(data=NULL, latCol=NULL, lonCol=NULL, groupCol=NULL, areaDef=NULL, areaNameCol="StratumName", areaLabels=is.null(data), xlim=NULL, ylim=NULL, areaLabelSize=2, pointColor="darkred", pointShape=23, pointSize=1, title="", projection=NULL, polygonColor="blue"){

  if ((is.null(data) || is.null(latCol) || is.null(lonCol)) && is.null(areaDef) && (is.null(xlim) || is.null(ylim))){
    stop("Provide either some data with latitude and longitue (data, latCol, lonCol), an area definition (areaDef) or extent to plot (xlim, ylim)")
  }
  
  if (is.null(projection)) {
    projection <- "+proj=merc +datum=WGS84"
  }
  
  if (!is.null(data)){
    if (!(latCol %in% names(data))){
      stop("'latCol' not found in 'data'")
    }
    if (!(lonCol %in% names(data))){
      stop("'lonCol' not found in 'data'")
    }
    if (is.null(xlim)){
      xlim <- c(min(data[[lonCol]], na.rm = T), max(data[[lonCol]], na.rm = T))
    }
    if (is.null(ylim)){
      ylim <- c(min(data[[latCol]], na.rm = T), max(data[[latCol]], na.rm = T))
    }
  }

  newcrs <- sf::st_crs(projection)

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  pl <- ggplot2::ggplot(data)
  pl <- pl + ggplot2::geom_sf(data=world)

  limbox <- NULL
  if (!is.null(areaDef)){
    areaDef <- sf::st_as_sf(areaDef)
    limbox <- sf::st_bbox(sf::st_transform(areaDef, sf::st_crs(4326))) #get bounding box in lat lon
  }
  
  if (!is.null(data)){
    if (is.null(groupCol)){
      pl <- pl + ggplot2::geom_sf(data=sf::st_as_sf(data, coords=c(lonCol,latCol), crs=sf::st_crs(4326)), size = pointSize,
                                  shape = pointShape, fill = pointColor, color=pointColor)
    }
    else{
      pl <- pl + ggplot2::geom_sf(data=sf::st_as_sf(data, coords=c(lonCol,latCol), crs=sf::st_crs(4326)), size = pointSize,
                                  shape = pointShape, ggplot2::aes(color=.data[[groupCol]]))
    }
    if (is.null(limbox)){
      limbox <- sf::st_bbox(sf::st_as_sf(data, coords=c(lonCol,latCol), crs=sf::st_crs(4326))) #get bounding box in lat lon
    }
  }

  if (is.null(xlim)){
    xlim <- c(limbox$xmin, limbox$xmax)
  }
  if (is.null(ylim)){
    ylim <- c(limbox$ymin, limbox$ymax)
  }
  
  
  
  if (!is.null(areaDef)){
    pl <- pl + ggplot2::geom_sf(data=areaDef, fill=NA, colour=polygonColor)

    if (areaLabels){
      labelPos <- suppressWarnings(cbind(areaDef, sf::st_coordinates(sf::st_centroid(sf::st_transform(areaDef, newcrs)))))
      pl <- pl + ggplot2::geom_label(data=labelPos, mapping=ggplot2::aes(x=.data[["X"]],y=.data[["Y"]],label=.data[[areaNameCol]]), size=areaLabelSize)
    }
  }

  #transform limits to desired projection
  limboxP <- sf::st_bbox(sf::st_transform(sf::st_sfc(sf::st_point(x = c(min(xlim), min(ylim))),
                                                     sf::st_point(x = c(max(xlim), min(ylim))),
                                                     sf::st_point(x = c(min(xlim), max(ylim))),
                                                     sf::st_point(x = c(max(xlim), max(ylim))),
                                                     sf::st_point(x = c(min(xlim), mean(ylim))),
                                                     sf::st_point(x = c(max(xlim), mean(ylim))),
                                                     sf::st_point(x = c(mean(xlim), min(ylim))),
                                                     sf::st_point(x = c(mean(xlim), max(ylim))),crs = sf::st_crs(4326)), newcrs))

  pl <- pl + ggplot2::coord_sf(crs = newcrs, xlim = c(limboxP$xmin, limboxP$xmax), ylim = c(limboxP$ymin, limboxP$ymax), expand = T)
  pl <- pl + ggplot2::xlab("Longitude")
  pl <- pl + ggplot2::ylab("Latitude")
  pl <- pl + ggplot2::theme_bw()
  pl <- pl + ggplot2::ggtitle(title)
  
  pl
}

#' Compare area definitions
#' @description 
#'  Plots two area defintions on top of each other for comparison
#' @details
#'  Map ranges to plot are specified by 'xlim' and 'ylim'. These should always be given in lon/lat.
#'  if these are not given they will be derived from the bounding box of 'areaDef'.
#'  To allow for flexible projection, some space will be added around 'xlim' and 'ylim'.
#'  
#'  Colors can be specified as understood by ggplot2. E.g. one of those listed by \code{\link[grDevices]{colors}}.
#'  Linetypes can be specified as understood by ggplot2. E.g: "solid", "dashed", "dotted", "dotdash", "longdash", or "twodash".
#'  
#' @param areaDef1 A \code{\link[sf]{sf}} data.frame. The area definition to be plotted first ("behind" the other one)
#' @param areaDef2 A \code{\link[sf]{sf}} data.frame. The area definition to be plotted second ("on top of" the other one)
#' @param areaNameCol1 The column in areaDef1 that provides the names to be used for any labeling of areas
#' @param areaNameCol2 The column in areaDef2 that provides the names to be used for any labeling of areas
#' @param areaLabels1 logical, specifying whether labels should be plotted for areaDef1. These are plotted first ("behind" any labels plotted for areaDef2)
#' @param areaLabels2 logical, specifying whether labels should be plotted for areaDef2. These are second ("on top of" any labels plotted for areaDef1)
#' @param xlim x axis limits in degrees longitude
#' @param ylim y axis limits in degrees latitudeD
#' @param areaLabelSize size for any area labels
#' @param title plot title
#' @param projection proj4string or EPSG code specifying the desired projection, see \code{\link[sf]{st_crs}}. Defaults to mercator projection.
#' @param polygonColor1 color to be used for plotting the areaDef1 polygons.
#' @param polygonColor2 color to be used for plotting the areaDef2 polygons.
#' @param linetype1 the linetype used for plotting the areaDef1 polygons.
#' @param linetype2 the linetype used for plotting the areaDef2 polygons.
#' @examples 
#'  #illustratting the difference between to similar area coding systems:
#'  plotAreaComparison(RstoxFDA::mainareaFdir2017, 
#'                     RstoxFDA::mainareaFdir2018, 
#'                     xlim=c(0,12), 
#'                     ylim=c(54,60), 
#'                     areaLabels2 = TRUE, 
#'                     title="Comparing Fdir main area definitions, before and as of 2018")
#' @concept spatial coding functions
#' @export
plotAreaComparison <- function(areaDef1, areaDef2, areaNameCol1="StratumName", areaNameCol2=areaNameCol1, areaLabels1=F, areaLabels2=F, xlim=NULL, ylim=NULL, areaLabelSize=2, title="", projection=NULL, polygonColor1="blue", polygonColor2="red", linetype1="solid", linetype2="dotdash"){
  if (is.null(projection)) {
    projection <- "+proj=merc +datum=WGS84"
  }
  
  newcrs <- sf::st_crs(projection)
  
  areaDef1 <- sf::st_as_sf(areaDef1)
  areaDef2 <- sf::st_as_sf(areaDef2)
  
  limbox <- sf::st_bbox(sf::st_transform(areaDef1, sf::st_crs(4326))) #get bounding box in lat lon
  if (is.null(xlim)){
    xlim <- c(limbox$xmin, limbox$xmax)
  }
  if (is.null(ylim)){
    ylim <- c(limbox$ymin, limbox$ymax)
  }
  
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  pl <- ggplot2::ggplot()
  pl <- pl + ggplot2::geom_sf(data=world)
  
  pl <- pl + ggplot2::geom_sf(data=areaDef1, fill=NA, colour=polygonColor1, linetype=linetype1)
  pl <- pl + ggplot2::geom_sf(data=areaDef2, fill=NA, colour=polygonColor2, linetype=linetype2)
    
  if (areaLabels1){
      labelPos <- suppressWarnings(cbind(areaDef1, sf::st_coordinates(sf::st_centroid(sf::st_transform(areaDef1, newcrs)))))
      pl <- pl + ggplot2::geom_label(data=labelPos, mapping=ggplot2::aes(x=.data[["X"]],y=.data[["Y"]],label=.data[[areaNameCol1]]), size=areaLabelSize, col=polygonColor1)
  }
  if (areaLabels2){
      labelPos <- suppressWarnings(cbind(areaDef2, sf::st_coordinates(sf::st_centroid(sf::st_transform(areaDef2, newcrs)))))
      pl <- pl + ggplot2::geom_label(data=labelPos, mapping=ggplot2::aes(x=.data[["X"]],y=.data[["Y"]],label=.data[[areaNameCol2]]), size=areaLabelSize, col=polygonColor2)
    }
  
  #transform limits to desired projection
  limboxP <- sf::st_bbox(sf::st_transform(sf::st_sfc(sf::st_point(x = c(min(xlim), min(ylim))),
                                                     sf::st_point(x = c(max(xlim), min(ylim))),
                                                     sf::st_point(x = c(min(xlim), max(ylim))),
                                                     sf::st_point(x = c(max(xlim), max(ylim))),
                                                     sf::st_point(x = c(min(xlim), mean(ylim))),
                                                     sf::st_point(x = c(max(xlim), mean(ylim))),
                                                     sf::st_point(x = c(mean(xlim), min(ylim))),
                                                     sf::st_point(x = c(mean(xlim), max(ylim))),crs = sf::st_crs(4326)), newcrs))
  
  pl <- pl + ggplot2::coord_sf(crs = newcrs, xlim = c(limboxP$xmin, limboxP$xmax), ylim = c(limboxP$ymin, limboxP$ymax), expand = T)
  pl <- pl + ggplot2::xlab("Longitude")
  pl <- pl + ggplot2::ylab("Latitude")
  pl <- pl + ggplot2::theme_bw()
  pl <- pl + ggplot2::ggtitle(title)
  
  pl
}

#' Plots bubble plot on map
#' @description
#'  Plots scalar nonegative quantities associated with an area code, as bubbles on a map.
#' @details
#'  The quantities (quantityCol) is aggregated (summed, ignoring NAs) over combinations of area and group ('areaCol' and 'groupCol') (if given)
#'  Bubble areas are proportional to the quantities.
#' @param data data.frame with quantities to be plotted
#' @param areaCol character() identifing column in 'data' that specify area codes, must correspond to 'areaNameCol'
#' @param quantityCol character() identifing column in 'data' that specify quantities to be plotted
#' @param areaDef \code{\link[sf]{sf}} data.frame
#' @param areaNameCol identifies column in 'areaDef' with label names for the areas, must correspond to 'areaCol'
#' @param legendTitle title for the legend (explains what the quantities are)
#' @param areaLabels logical whether to plot area labels
#' @param xlim x axis limits in degrees longitude
#' @param ylim y axis limits in degrees latitude
#' @param areaLabelSize size for any area labels
#' @param bubbleColor color of the bubbles
#' @param bubbleSize size for the bubbles
#' @param bubbleShape shape of the "bubbles". Default to circle (21). Use e.g. 22 for squares (consult list of pch values)
#' @param title plot title
#' @param projection proj4string or EPSG code specifying the desired projection, see \code{\link[sf]{st_crs}}. Defaults to mercator projection
#' @examples
#'  data(landings)
#'  data(ICESareas)
#'  plotBubbleMap(landings, "Area", "LiveWeightKG",
#'        areaDef = ICESareas, areaNameCol = "Area_Full",
#'        bubbleSize = 20, title="Landings on ICES areas")
#' @concept landings functions
#' @export
plotBubbleMap <- function(data, areaCol, quantityCol, areaDef, areaNameCol="StratumName", legendTitle=quantityCol, areaLabels=T, xlim=NULL, ylim=NULL, areaLabelSize=2, bubbleColor="darkred", bubbleSize=10, bubbleShape=21, title="", projection=NULL){
  requireNamespace("rnaturalearth")
  requireNamespace("sf")

  #force planar geometry for sf operations, for compability reasons
  sphergeom <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)
  
  
  if (is.null(projection)) {
    projection <- "+proj=merc +datum=WGS84"
  }

  if (!(areaCol %in% names(data))){
    stop("'areaCol' not found in 'data'")
  }
  if (!(quantityCol %in% names(data))){
    stop("'quantityCol' not found in 'data'")
  }

  newcrs <- sf::st_crs(projection)

  areaDef <- sf::st_as_sf(areaDef)

  limbox <- sf::st_bbox(sf::st_transform(areaDef, sf::st_crs(4326))) #get bounding box in lat lon
  if (is.null(xlim)){
    xlim <- c(limbox$xmin, limbox$xmax)
  }
  if (is.null(ylim)){
    ylim <- c(limbox$ymin, limbox$ymax)
  }

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # add maps an area polygons
  pl <- ggplot2::ggplot(data)
  pl <- pl + ggplot2::geom_sf(data=world)
  pl <- pl + ggplot2::geom_sf(data=areaDef, fill=NA)

  #aggregate data

    plotdata <- stats::aggregate(list(quant=data[[quantityCol]]), by=list(area=data[[areaCol]]), FUN=function(x){sum(x, na.rm=T)})

  # add positions from areaDef to data
  pos <- suppressWarnings(sf::st_centroid(areaDef))

  pos <- merge(pos, plotdata, all.x=T, by.x=areaNameCol, by.y="area")
  pos <- pos[!is.na(pos$quant),]


    pl <- pl + ggplot2::geom_sf(data=pos, ggplot2::aes(size = .data[["quant"]]), shape=bubbleShape, alpha = 0.7, colour = "black",fill=bubbleColor,stroke = .2) +
      ggplot2::scale_size_area(max_size=bubbleSize)

  pl <- pl +  ggplot2::labs(size=legendTitle)

  #add area labels
  if (areaLabels){
    labelPos <- suppressWarnings(cbind(pos, sf::st_coordinates(sf::st_centroid(sf::st_transform(pos, newcrs)))))
    pl <- pl + ggplot2::geom_text(data=labelPos, mapping=ggplot2::aes(x=.data[["X"]],y=.data[["Y"]],label=.data[[areaNameCol]]), size=areaLabelSize)
  }

  #transform limits to desired projection
  limboxP <- sf::st_bbox(sf::st_transform(sf::st_sfc(sf::st_point(x = c(min(xlim), min(ylim))),
                                                     sf::st_point(x = c(max(xlim), min(ylim))),
                                                     sf::st_point(x = c(min(xlim), max(ylim))),
                                                     sf::st_point(x = c(max(xlim), max(ylim))),
                                                     sf::st_point(x = c(min(xlim), mean(ylim))),
                                                     sf::st_point(x = c(max(xlim), mean(ylim))),
                                                     sf::st_point(x = c(mean(xlim), min(ylim))),
                                                     sf::st_point(x = c(mean(xlim), max(ylim))),crs = sf::st_crs(4326)), newcrs))

  pl <- pl + ggplot2::coord_sf(crs = newcrs, xlim = c(limboxP$xmin, limboxP$xmax), ylim = c(limboxP$ymin, limboxP$ymax), expand = T)
  pl <- pl + ggplot2::xlab("Longitude")
  pl <- pl + ggplot2::ylab("Latitude")
  pl <- pl + ggplot2::theme_bw()
  pl <- pl + ggplot2::ggtitle(title)

  sf::sf_use_s2(sphergeom)
  
  pl

}

#' Writes shape files as WKT files
#' @description
#'  Writes \code{\link[sp]{SpatialPolygonsDataFrame}} or \code{\link[sf]{sf}} data.frames, such as \code{\link[RstoxBase]{StratumPolygon}} as Stox-WKT files (stratafiles)
#' @param shape \code{\link[sp]{SpatialPolygonsDataFrame}} or \code{\link[sf]{sf}} data.frame stratadefinition to convert
#' @param output filename to save output to
#' @param namecol name of column in 'shape' that are to be used as strata names. Defaults to 'StratumName' pr the definition of \code{\link[RstoxBase]{StratumPolygon}}
#' @concept spatial coding functions
#' @export
writeSpDataFrameAsWKT <- function(shape, output, namecol="StratumName"){
  
  if (file.exists(output)){
    stop(paste("File", output, "exists already."))
  }
  
  obj <- sf::st_as_sf(shape)
  trans <- sf::st_transform(obj, sf::st_crs(4326))
  
  f<-file(output, open="w")
  
  for (i in 1:nrow(trans)){
   wkt <-  sf::st_as_text(sf::st_geometry(trans[i,]))
   write(paste(as.character(trans[[namecol]][[i]]), wkt,sep="\t"), f)
  }
  
  close(f)
  
}

#' Merges polygons
#' @description
#'  Merge \code{\link[sf]{sf}} data table, such as \code{\link[RstoxBase]{StratumPolygon}}
#' @details 
#'  All columns must have the same value for all polygons that are to be merged. If columns are not consistent in this regard, an error is raised.
#' @param shape \code{\link[sf]{sf}} data.table with stratadefinition to convert
#' @param mergeCol name of column that should be used for merging, all polygons with the same value in this column will be merged into one.
#' @return \code{\link[sf]{sf}} with polygons merged
#' @concept spatial coding functions
#' @export
mergePolygons <- function(shape, mergeCol){
  shape <- sf::st_as_sf(shape)
  if (nrow(unique(sf::st_drop_geometry(shape))) != length(unique(shape[[mergeCol]]))){
    stop("All columns must have the same value for polygons that are to be merged")
  }
  

  newPolygons <- NULL
  for (newName in unique(shape[[mergeCol]])){
    ff <- sf::st_union(shape[shape[[mergeCol]]==newName,])
    cols <- sf::st_drop_geometry(shape[shape[[mergeCol]]==newName,])[1,]
    ff <- cbind(cols, ff)
    newPolygons <- rbind(newPolygons, ff)
  }
  
  newPolygons <- sf::st_as_sf(newPolygons)
  
  return(newPolygons)
  
}
