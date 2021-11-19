library(rgdal)


#
# prep main areas (Fdir) as of 2018
#

mainareaFdir2018 <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", system.file("dataPrepScripts", "mainarea2018.txt", package="RstoxFDA"))

if (is.na(sp::proj4string(mainareaFdir2018))){
  sp::proj4string(mainareaFdir2018) <- "+proj=longlat +datum=WGS84"
}
commonCRS <- sp::CRS(sp::proj4string(mainareaFdir2018))

usethis::use_data(mainareaFdir2018, overwrite = T, compress = "xz")

#
# prep main areas (Fdir) before 2018
#


mainareaFdir2017 <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", system.file("dataPrepScripts", "mainarea2017.txt", package="RstoxFDA"))
if (is.na(sp::proj4string(mainareaFdir2017))){
  sp::proj4string(mainareaFdir2017) <- "+proj=longlat +datum=WGS84"
}
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
# prep ICES areas
#

# reduced detail with https://mapshaper.org

ICESareas <- rgdal::readOGR("~/shapefiles/ICES_simlified/ICES_areas/", stringsAsFactors = F)
ICESareas <- sp::spTransform(ICESareas, sp::CRS("+proj=longlat +datum=WGS84"))
for (i in 1:nrow(ICESareas)){
  slot(slot(ICESareas, "polygons")[[i]], "ID") <- ICESareas$Area_Full[i]
}
ICESareas$StratumName <- ICESareas$Area_27
ICESareas$OBJECTID_1 <- NULL
ICESareas$OBJECTID <- NULL
names(ICESareas)[names(ICESareas) == "SubDivisio"] <- "SubDivision"
ICESareas <- sp::spTransform(ICESareas, commonCRS)
usethis::use_data(ICESareas, overwrite = T, compress = "xz")

#
# prep ICES rectangles
#
ICESrectangles <- rgdal::readOGR("~/shapefiles/ICES_rectangles//", stringsAsFactors = F)
for (i in 1:nrow(ICESrectangles)){
  slot(slot(ICESrectangles, "polygons")[[i]], "ID") <- ICESrectangles$ICESNAME[i]
}
ICESrectangles$StratumName <- ICESrectangles$ICESNAME
ICESrectangles@data$OBJECTID <- NULL
ICESrectangles@data$OBJECTID_1 <- NULL
ICESrectangles@data$ICESNAME_2 <- NULL
ICESrectangles@data$ICESNAME_1 <- NULL
ICESrectangles@data$Shape_STAr <- NULL
ICESrectangles@data$Shape_STLe <- NULL
ICESrectangles@data$AREA <- NULL #planarized area
ICESrectangles@data$PERCENTAGE <- NULL #percentage of main ecoregion, based on planarized area.
ICESrectangles <- sp::spTransform(ICESrectangles, commonCRS)
usethis::use_data(ICESrectangles, overwrite = T, compress = "xz")




