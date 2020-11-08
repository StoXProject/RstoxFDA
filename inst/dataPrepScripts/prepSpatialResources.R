library(rgdal)


#
# prep main areas (Fdir) as of 2018
#

mainareaFdir2018 <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", system.file("dataPrepScripts", "mainarea2018.txt", package="RstoxFDA"))

if (is.na(sp::proj4string(mainareaFdir2018))){
  sp::proj4string(mainareaFdir2018) <- "+proj=longlat +datum=WGS84"
}

usethis::use_data(mainareaFdir2018, overwrite = T)

#
# prep main areas (Fdir) before 2018
#


mainareaFdir2017 <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", system.file("dataPrepScripts", "mainarea2017.txt", package="RstoxFDA"))
if (is.na(sp::proj4string(mainareaFdir2017))){
  sp::proj4string(mainareaFdir2017) <- "+proj=longlat +datum=WGS84"
}
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
usethis::use_data(mainareaFdir2017, overwrite = T)

#
# prep locations (Fdir) as of 2018
#


locationsFdir2018 <- rgdal::readOGR("~/shapefiles/fdir/fdir_annotated/Lokasjoner_fom_2018/", stringsAsFactors = F)
for (i in 1:nrow(locationsFdir2018)){
  slot(slot(locationsFdir2018, "polygons")[[i]], "ID") <- locationsFdir2018$LOKREF[i]
}
locationsFdir2018$polygonName <- locationsFdir2018$LOKREF
usethis::use_data(locationsFdir2018, overwrite = T)

#
# prep locations (Fdir) before 2018
#

locationsFdir2017 <- rgdal::readOGR("~/shapefiles/fdir/fdir_annotated/Lokasjoner_tom_2017/", stringsAsFactors = F)
for (i in 1:nrow(locationsFdir2017)){
  slot(slot(locationsFdir2017, "polygons")[[i]], "ID") <- locationsFdir2017$LOKREF[i]
}
locationsFdir2017$polygonName <- locationsFdir2017$LOKREF
usethis::use_data(locationsFdir2017, overwrite = T)

NAFOareas <- rgdal::readOGR("~/shapefiles/NAFO_hovedomr_2017_WGS84/", stringsAsFactors = F)
for (i in 1:nrow(NAFOareas)){
  slot(slot(NAFOareas, "polygons")[[i]], "ID") <- NAFOareas$homr[i]
}
NAFOareas$polygonName <- NAFOareas$homr
NAFOareas$nafo_names <- NAFOareas$nafo_norsk
NAFOareas$first_nafo <- NULL
NAFOareas$nafo_norsk <- NULL
usethis::use_data(NAFOareas, overwrite = T)
