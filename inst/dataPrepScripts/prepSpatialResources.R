#
# prep main areas (Fdir) as of 2018
#

mainareaFdir2018 <- sf::read_sf("~/shapefiles/fdir/hovedomraader_2018/", as_tibble = F, stringsAsFactors = F)
mainareaFdir2018$StratumName <- sprintf("%02d", mainareaFdir2018$FID)
mainareaFdir2018$FID <- NULL
mainareaFdir2018 <- mainareaFdir2018[,c("StratumName", "geometry")]
mainareaFdir2018 <- sf::st_transform(mainareaFdir2018, crs = RstoxBase:::getRstoxBaseDefinitions("proj4string_longlat"))

usethis::use_data(mainareaFdir2018, overwrite = T, compress = "xz")

#
# prep main areas (Fdir) before 2018
#


mainareaFdir2017 <- sf::read_sf("~/shapefiles/fdir/hovedomraader_2017/", as_tibble = F, stringsAsFactors = F)
mainareaFdir2017$StratumName <- sprintf("%02d", mainareaFdir2017$FID)
mainareaFdir2017$FID <- NULL
mainareaFdir2017 <- mainareaFdir2017[,c("StratumName", "geometry")]
mainareaFdir2017 <- sf::st_transform(mainareaFdir2017, crs = RstoxBase:::getRstoxBaseDefinitions("proj4string_longlat"))

usethis::use_data(mainareaFdir2017, overwrite = T, compress = "xz")

#
# prep locations (Fdir) as of 2018
#

locationsFdir2018 <- sf::read_sf("~/shapefiles/fdir/fdir_annotated/Lokasjoner_fom_2018/", as_tibble = F, stringsAsFactors = F)
locationsFdir2018$StratumName <- locationsFdir2018$LOKREF
locationsFdir2018 <- locationsFdir2018[,c("lok", "HAVOMR", "Lokasjon", "StratumName", "geometry")]
locationsFdir2018 <- sf::st_transform(locationsFdir2018, crs = RstoxBase:::getRstoxBaseDefinitions("proj4string_longlat"))
usethis::use_data(locationsFdir2018, overwrite = T, compress = "xz")

#
# prep locations (Fdir) before 2018
#

locationsFdir2017 <- sf::read_sf("~/shapefiles/fdir/fdir_annotated/Lokasjoner_tom_2017/", as_tibble = F, stringsAsFactors = F)
locationsFdir2017$StratumName <- locationsFdir2017$LOKREF
locationsFdir2017 <- locationsFdir2017[,c("lok", "HAVOMR", "Lokasjon", "StratumName", "geometry")]
locationsFdir2017 <- sf::st_transform(locationsFdir2017, crs = RstoxBase:::getRstoxBaseDefinitions("proj4string_longlat"))
usethis::use_data(locationsFdir2017, overwrite = T, compress = "xz")


#
# prep NAFO areas
#

NAFOareas <- sf::read_sf("~/shapefiles/NAFO_hovedomr_2017_WGS84/", as_tibble = F, stringsAsFactors = F)
NAFOareas$StratumName <- NAFOareas$homr
NAFOareas$nafo_names <- NAFOareas$nafo_norsk
NAFOareas$first_nafo <- NULL
NAFOareas$nafo_norsk <- NULL
NAFOareas <- NAFOareas[,c("homr", "StratumName", "nafo_names", "geometry")]
NAFOareas <- sf::st_transform(NAFOareas, crs = RstoxBase:::getRstoxBaseDefinitions("proj4string_longlat"))
usethis::use_data(NAFOareas, overwrite = T, compress = "xz")

#
# prep ICES areas fom 2018
#

# reduced detail with https://mapshaper.org

ICESareas <- sf::read_sf("~/shapefiles/forenkelt_ICES_fra_haakon/ICES_areas_forenklet/", as_tibble = F, stringsAsFactors = F)
ICESareas <- sf::st_transform(ICESareas, crs = RstoxBase:::getRstoxBaseDefinitions("proj4string_longlat"))
ICESareas$StratumName <- ICESareas$Area_Full
ICESareas$OBJECTID_1 <- NULL
ICESareas$OBJECTID <- NULL
names(ICESareas)[names(ICESareas) == "SubDivisio"] <- "SubDivision"
usethis::use_data(ICESareas, overwrite = T, compress = "xz")

#
# copied in here to introduce the dependency to nngeo
#
mergePolygonsR <- function(shape, mergeCol){
  require(nngeo)
  if (nrow(unique(shape@data)) != length(unique(shape@data[[mergeCol]]))){
    stop("All columns must have the same value for polygons that are to be merged")
  }
  
  dd<-sf::st_as_sf(shape)
  newPolygons <- NULL
  for (newName in unique(shape@data[[mergeCol]])){
    ff <- sf::st_union(dd[dd[[mergeCol]]==newName,])
    ff <- nngeo::st_remove_holes(ff)
    
    if (!any(is.na(sf::st_dimension(ff)))){
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
    else{
      if (!all(is.na(sf::st_dimension(ff)))){
        browser()
      }
    }
    
  }
  
  newPolygons <- sp::SpatialPolygonsDataFrame(newPolygons, shape[!duplicated(shape[[mergeCol]]),]@data, match.ID = mergeCol)
  
  return(newPolygons)
  
}

#
# prep ICES SubArea
#
ICESsubArea <- RstoxFDA::ICESareas
ICESsubArea$StratumName <- paste(ICESsubArea$Major_FA, ICESsubArea$SubArea, sep=".")
ICESsubArea <- ICESsubArea[!is.na(ICESsubArea$SubArea),]
ICESsubArea <- ICESsubArea[,"StratumName"]
ICESsubArea <- mergePolygonsR(ICESsubArea, "StratumName")
usethis::use_data(ICESsubArea, overwrite = T, compress = "xz")

ICESdivision <- RstoxFDA::ICESareas
ICESdivision$StratumName <- paste(ICESdivision$Major_FA, ICESdivision$SubArea, ICESdivision$Division, sep=".")
ICESdivision <- ICESdivision[!is.na(ICESdivision$Division),]
ICESdivision <- ICESdivision[,"StratumName"]
ICESdivision <- mergePolygonsR(ICESdivision, "StratumName")
usethis::use_data(ICESdivision, overwrite = T, compress = "xz")

ICESsubDivision <- RstoxFDA::ICESareas
ICESsubDivision$StratumName <- paste(ICESsubDivision$Major_FA, ICESsubDivision$SubArea, ICESsubDivision$Division, ICESsubDivision$SubDivision, sep=".")
ICESsubDivision <- ICESsubDivision[!is.na(ICESsubDivision$SubDivision),]
ICESsubDivision <- ICESsubDivision[,"StratumName"]
ICESsubDivision <- mergePolygonsR(ICESsubDivision, "StratumName")
usethis::use_data(ICESsubDivision, overwrite = T, compress = "xz")

ICESunit <- RstoxFDA::ICESareas
ICESunit$StratumName <- paste(ICESunit$Major_FA, ICESunit$SubArea, ICESunit$Division, ICESunit$SubDivision, ICESunit$Unit, sep=".")
ICESunit <- ICESunit[!is.na(ICESunit$Unit),]
ICESunit <- ICESunit[,"StratumName"]
ICESunit <- mergePolygonsR(ICESunit, "StratumName")
usethis::use_data(ICESunit, overwrite = T, compress = "xz")



#
# prep ICES rectangles
#
ICESrectangles <- rgdal::readOGR("~/shapefiles/ICES_StatRec_mapto_ICES_Areas/", stringsAsFactors = F)
for (i in 1:nrow(ICESrectangles)){
  slot(slot(ICESrectangles, "polygons")[[i]], "ID") <- ICESrectangles$ICESNAME[i]
}
# some potentially useful attributes are removed, pending quality checks.
# other versions of this file have had areas and percentages calculated from planarized coordinates.
ICESrectangles$StratumName <- ICESrectangles$ICESNAME
ICESrectangles$ICESNAME <- NULL
ICESrectangles@data$ID <- NULL
ICESrectangles@data$OBJECTID <- NULL
ICESrectangles@data$AREA_KM2 <- NULL
ICESrectangles$stat_x <- NULL
ICESrectangles$stat_y <- NULL
ICESrectangles$Perc <- NULL
ICESrectangles$MaxPer <- NULL
ICESrectangles$RNDMaxPer <- NULL
ICESrectangles$AreasList <- NULL
ICESrectangles$Shape_Leng <- NULL
ICESrectangles$Shape_Area <- NULL
ICESrectangles$SubArea <- unlist(lapply(strsplit(ICESrectangles$Area_27, ".", fixed=T), FUN=function(x){x[1]}))
ICESrectangles$Division <- unlist(lapply(strsplit(ICESrectangles$Area_27, ".", fixed=T), FUN=function(x){x[2]}))
ICESrectangles$SubDivision <- unlist(lapply(strsplit(ICESrectangles$Area_27, ".", fixed=T), FUN=function(x){x[3]}))
ICESrectangles$Unit <- unlist(lapply(strsplit(ICESrectangles$Area_27, ".", fixed=T), FUN=function(x){x[4]}))
ICESrectangles$Major_FA <- "27"
ICESrectangles$Area_Full <- paste("27", ICESrectangles$Area_27, sep=".")
ICESrectangles <- ICESrectangles[!is.na(ICESrectangles$Area_27),]
ICESrectangles <- sp::spTransform(ICESrectangles, commonCRS)
usethis::use_data(ICESrectangles, overwrite = T, compress = "xz")



#prep GSA subarea
GSAsubArea <- rgdal::readOGR("~/shapefiles/FAOsubAreas/GSAs_simplified/", stringsAsFactors = F)
GSAsubArea <- sp::spTransform(GSAsubArea, commonCRS)
GSAsubArea$StratumName <- GSAsubArea$F_DIVISION
GSAsubArea <-GSAsubArea[, c("StratumName", "F_AREA", "F_SUBAREA", "F_DIVISION")]
GSAsubArea <- RstoxFDA::mergePolygons(GSAsubArea, "F_DIVISION")
usethis::use_data(GSAsubArea, overwrite=T, compress="xz")


#prep kommune
ss<-sf::st_read("~/shapefiles/geonorge/kommuner_2022/Basisdata_0000_Norge_25833_Kommuner_GML.gml", "Kommune")
dd <- sf::st_simplify(ss, preserveTopology = T, dTolerance = 100)
dp <- sf::as_Spatial(sf::st_transform(dd, sp::CRS(sp::wkt(RstoxFDA::mainareaFdir2018))))
kommuner2022 <- dp[,c("kommunenummer", "navn")]
kommuner2022$navn <- unlist(lapply(kommuner2022$navn, FUN=function(x){x[[1]]}))
kommuner2022$StratumName <- as.character(kommuner2022$kommunenummer)
names(kommuner2022) <- c("id", "name", "StratumName")
Encoding(kommuner2022$name) <- "latin1"
kommuner2022$name <- iconv(
  kommuner2022$name, 
  "latin1", 
  "UTF-8"
)
usethis::use_data(kommuner2022, overwrite = T)
