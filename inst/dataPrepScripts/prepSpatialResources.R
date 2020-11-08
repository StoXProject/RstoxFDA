mainareaFdir2018 <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", system.file("dataPrepScripts", "mainarea2018.txt", package="RstoxFDA"))

if (is.na(sp::proj4string(mainareaFdir2018))){
  sp::proj4string(mainareaFdir2018) <- "+proj=longlat +datum=WGS84"
}

usethis::use_data(mainareaFdir2018, overwrite = T)

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
