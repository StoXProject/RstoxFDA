RstoxFDA::plotArea(areaDef=RstoxFDA::ICESareas)


# plot mainarea and NAFO areas combined in a Lambert Conformal Conic projection.
RstoxFDA::plotArea(title="Main area + NAFO",
         areaDef=rbind(RstoxFDA::mainareaFdir2018[,c("StratumName")],
                       RstoxFDA::NAFOareas[,c("StratumName")]),
         projection="+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 
         +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

StoxBioticFile <- system.file("testresources","StoxBioticData.rds", package="RstoxFDA")
StoxBioticData <- readRDS(StoxBioticFile)

RstoxFDA::plotArea(title="Main area + NAFO",
         data = StoxBioticData$Station,
         latCol = "Latitude",
         lonCol = "Longitude",
         xlim = c(-14,14),
         ylim = c(52,62),
         areaDef=rbind(RstoxFDA::mainareaFdir2018[,c("StratumName")],
                       RstoxFDA::NAFOareas[,c("StratumName")]))


RstoxFDA::plotBubbleMap(RstoxFDA::landings, "Area", "LiveWeightKG",
      areaDef = RstoxFDA::ICESareas, areaNameCol = "Area_Full",
      bubbleSize = 20, title="Landings on ICES areas", projection = "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 
         +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

RstoxFDA::plotBubbleMap(RstoxFDA::landings, "Area", "LiveWeightKG",
              areaDef = RstoxFDA::ICESareas, areaNameCol = "Area_Full",
              bubbleSize = 20, title="Landings on ICES areas")


#context("Test writeSpDataFrameAsWKT")
tempfile <- tempfile(fileext = ".txt")
RstoxFDA::writeSpDataFrameAsWKT(RstoxFDA::ICESareas, tempfile)
ia <- RstoxBase::DefineStratumPolygon(DefinitionMethod = "ResourceFile", FileName=tempfile)
unlink(tempfile, recursive = T)
expect_true("StratumName" %in% names(ia))

ia <- RstoxFDA::ICESareas
ia$StratumName <- paste(ia$StratumName, sep=".")

ib <- ia
ib$StratumName <- paste(ib$Major_FA, ib$SubArea, sep=".")
expect_error(RstoxFDA::mergePolygons(ib, "StratumName"), "All columns must have the same value for polygons that are to be merged")


df <- ia@data
ia <- rgeos::gSimplify(ia, tol=.4)
suppressWarnings(ia <- rgeos::gBuffer(ia, byid = T, width=.03))
ia <- sp::SpatialPolygonsDataFrame(ia, df, match.ID = "StratumName")
ia$StratumName <- paste(ia$Major_FA, ia$SubArea, sep=".")
ia@data <- ia@data[,c("StratumName","Major_FA")]

merged <- RstoxFDA::mergePolygons(ia, "StratumName")
expect_true("SpatialPolygonsDataFrame" %in% class(merged))
expect_equal(length(merged), length(unique(ia$StratumName)))

# plot area comparison
plotAreaComparison(RstoxFDA::mainareaFdir2017, RstoxFDA::mainareaFdir2018, xlim=c(0,12), ylim=c(54,60), areaLabels2 = T, projection = "+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 
         +lon_0=10 +x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

