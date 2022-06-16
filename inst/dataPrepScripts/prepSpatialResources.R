library(rgdal)


#
# prep main areas (Fdir) as of 2018
#

mainareaFdir2018 <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", system.file("dataPrepScripts", "mainarea2018.txt", package="RstoxFDA"))
commonCRS <- sp::CRS(sp::wkt(mainareaFdir2018))

usethis::use_data(mainareaFdir2018, overwrite = T, compress = "xz")

#
# prep main areas (Fdir) before 2018
#


mainareaFdir2017 <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", system.file("dataPrepScripts", "mainarea2017.txt", package="RstoxFDA"))
mainareaFdir2017 <- sp::spTransform(mainareaFdir2017, commonCRS)
slot(slot(mainareaFdir2017, "polygons")[[1]], "ID") <- "00"
slot(slot(mainareaFdir2017, "polygons")[[2]], "ID") <- "01"
slot(slot(mainareaFdir2017, "polygons")[[3]], "ID") <- "02"
slot(slot(mainareaFdir2017, "polygons")[[4]], "ID") <- "03"
slot(slot(mainareaFdir2017, "polygons")[[5]], "ID") <- "04"
slot(slot(mainareaFdir2017, "polygons")[[6]], "ID") <- "05"
slot(slot(mainareaFdir2017, "polygons")[[7]], "ID") <- "06"
slot(slot(mainareaFdir2017, "polygons")[[8]], "ID") <- "07"
slot(slot(mainareaFdir2017, "polygons")[[9]], "ID") <- "08"
slot(slot(mainareaFdir2017, "polygons")[[10]], "ID") <- "09"
usethis::use_data(mainareaFdir2017, overwrite = T, compress = "xz")

#
# prep locations (Fdir) as of 2018
#


locationsFdir2018 <- rgdal::readOGR("~/shapefiles/fdir/fdir_annotated/Lokasjoner_fom_2018/", stringsAsFactors = F)
for (i in 1:nrow(locationsFdir2018)){
  slot(slot(locationsFdir2018, "polygons")[[i]], "ID") <- locationsFdir2018$LOKREF[i]
}
locationsFdir2018$StratumName <- locationsFdir2018$LOKREF
locationsFdir2018 <- sp::spTransform(locationsFdir2018, commonCRS)
usethis::use_data(locationsFdir2018, overwrite = T, compress = "xz")

#
# prep locations (Fdir) before 2018
#

locationsFdir2017 <- rgdal::readOGR("~/shapefiles/fdir/fdir_annotated/Lokasjoner_tom_2017/", stringsAsFactors = F)
for (i in 1:nrow(locationsFdir2017)){
  slot(slot(locationsFdir2017, "polygons")[[i]], "ID") <- locationsFdir2017$LOKREF[i]
}
locationsFdir2017$StratumName <- locationsFdir2017$LOKREF
locationsFdir2017 <- sp::spTransform(locationsFdir2017, commonCRS)
usethis::use_data(locationsFdir2017, overwrite = T, compress = "xz")


#
# prep NAFO areas
#

NAFOareas <- rgdal::readOGR("~/shapefiles/NAFO_hovedomr_2017_WGS84/", stringsAsFactors = F)
for (i in 1:nrow(NAFOareas)){
  slot(slot(NAFOareas, "polygons")[[i]], "ID") <- NAFOareas$homr[i]
}
NAFOareas$StratumName <- NAFOareas$homr
NAFOareas$nafo_names <- NAFOareas$nafo_norsk
NAFOareas$first_nafo <- NULL
NAFOareas$nafo_norsk <- NULL
NAFOareas <- sp::spTransform(NAFOareas, commonCRS)
usethis::use_data(NAFOareas, overwrite = T, compress = "xz")

#
# prep ICES areas fom 2018
#

# reduced detail with https://mapshaper.org

ICESareas <- rgdal::readOGR("~/shapefiles/forenkelt_ICES_fra_haakon/ICES_areas_forenklet/", stringsAsFactors = F)
ICESareas <- sp::spTransform(ICESareas, sp::CRS("+proj=longlat +datum=WGS84"))
for (i in 1:nrow(ICESareas)){
  slot(slot(ICESareas, "polygons")[[i]], "ID") <- ICESareas$Area_Full[i]
}
ICESareas$StratumName <- ICESareas$Area_Full
ICESareas$OBJECTID_1 <- NULL
ICESareas$OBJECTID <- NULL
names(ICESareas)[names(ICESareas) == "SubDivisio"] <- "SubDivision"
ICESareas <- sp::spTransform(ICESareas, commonCRS)
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




