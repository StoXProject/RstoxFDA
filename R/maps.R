
#' Plot area definitions on a map
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
#' @param pointcol color of any points to be plotted.
#' @param pointshape ggplot2 shape for any points to be plotted
#' @param pointsize size for any points to be plotted
#' @param title plot title
#' @param projection proj4string or EPSG code specifying the desired projection, see \code{\link[sf]{st_crs}}
#' @examples
#'  # plot ICES areas with default projection
#'  data(ICESareas)
#'  plotArea(areaDef=ICESareas)
#'
#'  data(mainareaFdir2018)
#'  data(NAFOareas)
#'  # plot mainarea and NAFO areas combined in mercator projection
#'  plotArea(title="Main area + NAFO",
#'           areaDef=rbind(mainareaFdir2018[,c("polygonName")],
#'                   NAFOareas[,c("polygonName")]),
#'           projection="+proj=merc +datum=WGS84")
#' @export
plotArea <- function(data=NULL, latCol=NULL, lonCol=NULL, groupCol=NULL, areaDef, areaNameCol="polygonName", areaLabels=is.null(data), xlim=NULL, ylim=NULL, areaLabelSize=2, pointcol="darkred", pointshape=23, pointsize=1, title="", projection=102014){

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
      pl <- pl + ggplot2::geom_sf(data=sf::st_as_sf(data, coords=c(lonCol,latCol), crs=sf::st_crs(4326)), size = pointsize,
                                  shape = pointshape, fill = pointcol, color=pointcol)
    }
    else{
      pl <- pl + ggplot2::geom_sf(data=sf::st_as_sf(data, coords=c(lonCol,latCol), crs=sf::st_crs(4326)), size = pointsize,
                                  shape = pointshape, ggplot2::aes_string(color=groupCol))
    }
  }

  if (!is.null(areaDef)){
    pl <- pl + ggplot2::geom_sf(data=areaDef, fill=NA)

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
