
#' Plot area definitions on a map
#' @description
#'  Plots area polygons on a map, may point a set of positions as well.
#' @details
#'  Map ranges to plot are specified by 'xlim' and 'ylim'. These should always be given in lon/lat.
#'  if these are not given they will be derived in order to include all positions in 'data'.
#'  if 'data is not given they will be derived from the bounding box of 'areaDef'.
#'  To allow for flexible projection, some space will be added around 'xlim' and 'ylim'.
#'  
#'  Colors can be specified as understood by ggplot2. E.g. one of those listed by \code{\link[grDevices]{colors()}}.
#'  
#' @param data data.frame with any points to be plotted
#' @param latCol character() identifing column in 'data' that specify latitudes (WGS84)
#' @param lonCol character() identifing column in 'data' that specify longitudes (WGS84)
#' @param groupCol character() identifying column in 'daat' that specify grouping of points
#' @param areaDef \code{\link[sp]{SpatialPolygonsDataFrame}}
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
#' @export
plotArea <- function(data=NULL, latCol=NULL, lonCol=NULL, groupCol=NULL, areaDef, areaNameCol="StratumName", areaLabels=is.null(data), xlim=NULL, ylim=NULL, areaLabelSize=2, pointColor="darkred", pointShape=23, pointSize=1, title="", projection=NULL, polygonColor="blue"){

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

  areaDef <- sf::st_as_sf(areaDef)

  limbox <- sf::st_bbox(sf::st_transform(areaDef, sf::st_crs(4326))) #get bounding box in lat lon
  if (is.null(xlim)){
    xlim <- c(limbox$xmin, limbox$xmax)
  }
  if (is.null(ylim)){
    ylim <- c(limbox$ymin, limbox$ymax)
  }

  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  pl <- ggplot2::ggplot(data)
  pl <- pl + ggplot2::geom_sf(data=world)

  if (!is.null(data)){
    if (is.null(groupCol)){
      pl <- pl + ggplot2::geom_sf(data=sf::st_as_sf(data, coords=c(lonCol,latCol), crs=sf::st_crs(4326)), size = pointSize,
                                  shape = pointShape, fill = pointColor, color=pointColor)
    }
    else{
      pl <- pl + ggplot2::geom_sf(data=sf::st_as_sf(data, coords=c(lonCol,latCol), crs=sf::st_crs(4326)), size = pointSize,
                                  shape = pointShape, ggplot2::aes_string(color=groupCol))
    }
  }

  if (!is.null(areaDef)){
    pl <- pl + ggplot2::geom_sf(data=areaDef, fill=NA, colour=polygonColor)

    if (areaLabels){
      labelPos <- suppressWarnings(cbind(areaDef, sf::st_coordinates(sf::st_centroid(sf::st_transform(areaDef, newcrs)))))
      pl <- pl + ggplot2::geom_label(data=labelPos, mapping=ggplot2::aes_string(x="X",y="Y",label=areaNameCol), size=areaLabelSize)
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

#' Plots bubble plot on map
#' @description
#'  Plots scalar nonegative quantities associated with an area code, as bubbles on a map.
#' @details
#'  The quantities (quantityCol) is aggregated (summed, ignoring NAs) over combinations of area and group ('areaCol' and 'groupCol') (if given)
#'  Bubble areas are proportional to the quantities.
#' @param data data.frame with quantities to be plotted
#' @param areaCol character() identifing column in 'data' that specify area codes, must correspond to 'areaNameCol'
#' @param quantityCol character() identifing column in 'data' that specify quantities to be plotted
#' @param areaDef \code{\link[sp]{SpatialPolygonsDataFrame}}
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


    pl <- pl + ggplot2::geom_sf(data=pos, ggplot2::aes_string(size = "quant"), shape=bubbleShape, alpha = 0.7, colour = "black",fill=bubbleColor,stroke = .2) +
      ggplot2::scale_size_area(max_size=bubbleSize)

  pl <- pl +  ggplot2::labs(size=legendTitle)

  #add area labels
  if (areaLabels){
    labelPos <- suppressWarnings(cbind(pos, sf::st_coordinates(sf::st_centroid(sf::st_transform(pos, newcrs)))))
    pl <- pl + ggplot2::geom_text(data=labelPos, mapping=ggplot2::aes_string(x="X",y="Y",label=areaNameCol), size=areaLabelSize)
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
#'  Writes \code{\link[sp]{SpatialPolygonsDataFrame}}, such as \code{\link[RstoxBase]{StratumPolygon}} as Stox-WKT files (stratafiles)
#' @param shape \code{\link[sp]{SpatialPolygonsDataFrame}} stratadefinition to convert
#' @param output filename to save output to
#' @param namecol name of column in 'shape' that are to be used as strata names. Defaults to 'StratumName' pr the definition of \code{\link[RstoxBase]{StratumPolygon}}
#' @export
writeSpDataFrameAsWKT <- function(shape, output, namecol="StratumName"){
  requireNamespace("rgeos", quietly = TRUE)
  requireNamespace("sp", quietly = TRUE)
  if (file.exists(output)){
    stop(paste("File", output, "exists already."))
  }
  
  projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  shp <- sp::spTransform(shape, sp::CRS(projection))
  
  f<-file(output, open="w")
  
  for (i in 1:nrow(shp)){
    poly <- shp[i,]
    write(paste(as.character(poly[[namecol]]), rgeos::writeWKT(poly, byid = F),sep="\t"), f)
  }
  close(f)
  
}

#' Merges polygons
#' @description
#'  Merge \code{\link[sp]{SpatialPolygonsDataFrame}}, such as \code{\link[RstoxBase]{StratumPolygon}}
#' @details 
#'  All columns must have the same value for all polygons that are to be merged. If columns are not consistent in this regard, an error is raised.
#' @param shape \code{\link[sp]{SpatialPolygonsDataFrame}} stratadefinition to convert
#' @param mergeCol name of column that should be used for merging, all polygons with the same value in this column will be merged into one.
#' @return \code{\link[sp]{SpatialPolygonsDataFrame}} with polygons merged
#' @export
mergePolygons <- function(shape, mergeCol){
  
  if (nrow(unique(shape@data)) != length(unique(shape@data[[mergeCol]]))){
    stop("All columns must have the same value for polygons that are to be merged")
  }
  
  dd<-sf::st_as_sf(shape)
  
  newPolygons <- NULL
  for (newName in unique(shape@data[[mergeCol]])){
    ff<-sf::st_union(dd[dd$StratumName==newName,])
    spat <- sf::as_Spatial(ff)
    stopifnot(length(spat@polygons)==1)
    spat@polygons[[1]]@ID <- newName
    
    if (is.null(newPolygons)){
      newPolygons <- spat
    }
    else{
      newPolygons <- rbind(newPolygons, spat)      
    }
  }
  
  newPolygons <- sp::SpatialPolygonsDataFrame(newPolygons, shape@data[!duplicated(shape@data[[mergeCol]]),], match.ID = mergeCol)
  
  return(newPolygons)
  
}
