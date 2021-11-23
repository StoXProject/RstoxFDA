data(ICESareas)
plotArea(areaDef=ICESareas)

data(mainareaFdir2018)
data(NAFOareas)
# plot mainarea and NAFO areas combined in a Lambert Conformal Conic projection.
plotArea(title="Main area + NAFO",
         areaDef=rbind(mainareaFdir2018[,c("StratumName")],
                 NAFOareas[,c("StratumName")]),
         projection="+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 
         +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

plotArea(title="Main area + NAFO",
         data = StoxBioticData$Station,
         latCol = "Latitude",
         lonCol = "Longitude",
         xlim = c(-14,14),
         ylim = c(52,62),
         areaDef=rbind(mainareaFdir2018[,c("StratumName")],
                       NAFOareas[,c("StratumName")]))

data(landings)
data(ICESareas)
plotBubbleMap(landings, "Area", "LiveWeightKG",
      areaDef = ICESareas, areaNameCol = "Area_Full",
      bubbleSize = 20, title="Landings on ICES areas", projection = "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 
         +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

plotBubbleMap(landings, "Area", "LiveWeightKG",
              areaDef = ICESareas, areaNameCol = "Area_Full",
              bubbleSize = 20, title="Landings on ICES areas")


tempfile <- tempfile(fileext = ".txt")
writeSpDataFrameAsWKT(RstoxFDA::ICESareas, tempfile)
ia <- RstoxBase::DefineStratumPolygon(DefinitionMethod = "ResourceFile", FileName=tempfile)
unlink(tempfile, recursive = T)
expect_true("StratumName" %in% names(ia))


