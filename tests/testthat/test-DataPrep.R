context("test-DataPrep: categoriseDate default")

date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
quarters <- categoriseDate(date)
expect_equal(quarters, c("Q1", "Q1", "Q1", "Q2", "Q2", "Q3", "Q4"))

context("test-DataPrep: categoriseDate month")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
quarters <- categoriseDate(date, temporalType = "month")
expect_equal(quarters, strftime(date, format="%B"))

context("test-DataPrep: categoriseDate week")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
weeks <- categoriseDate(date, temporalType = "week")
expect_equal(weeks, c("W03", "W09", "W09", "W14", "W22", "W27", "W50"))

context("test-DataPrep: categoriseDate custom")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
custom <- categoriseDate(date, temporalType = "custom", FUN=function(day,month){if(month<4){return("l")};if(month==4 & day<2){return("g")};return("s")})
expect_equal(custom, c("l", "l", "l", "g", "s", "s", "s"))

context("test-DataPrep: categoriseDate non-seasonal")
date <- as.POSIXct(c("2018-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
custom <- categoriseDate(date, temporalType = "custom", seasonal = F, FUN=function(day,month){if(month<4){return("l")};if(month==4 & day<2){return("g")};return("s")})
expect_equal(custom, c("l-2018", "l-2019", "l-2019", "g-2019", "s-2019", "s-2019", "s-2019"))

context("test-DataPrep: categoriseDate undefined")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
expect_error(categoriseDate(date, temporalType = "QRT"), "Temporal type QRT not recognized.")

context("test-DataPrep: categoriseDate NAs")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", NA, "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
expect_error(categoriseDate(date, temporalType = "QRT"), "NAs in date.")


context("test-DataPrep: categoriseDate default")
gearConversion <- list()
gearConversion["TBS"] <- "OTB"
gearConversion["TBN"] <- "OTB"
gearConversion["OTB"] <- "OTB"
converted <- convertCodes(c("TBS", "TBN", "OTB"), gearConversion)
expect_equal(converted, c("OTB", "OTB", "OTB"))

context("test-DataPrep: categoriseDate NAs")
expect_error(convertCodes(c("TBS", NA, "OTB"), gearConversion))

context("test-DataPrep: categoriseDate incomplete code list")
expect_error(convertCodes(c("TBS", "PTB", "PTM", "OTB"), gearConversion), "Conversion not defined for all codes. Missing for: PTB, PTM")

context("test-DataPrep: categoriseDate mapped to integers")
gearConversion2 <- list()
gearConversion2["TBS"] <- 1
gearConversion2["TBN"] <- 2
gearConversion2["OTB"] <- 3
expect_warning(convertCodes(c("TBS", "TBN", "OTB"), gearConversion2), "Coercing converted codes to character")


context("test-DataPrep: append area code")
strp <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile",system.file("testresources", "mainarea_fdir_fom2018_strata.txt", package="RstoxFDA"))
sp::proj4string(strp) <- sp::CRS("+proj=longlat +datum=WGS84")

areafile <- system.file("testresources","mainarea_fdir_from_2018_compl.txt", package="RstoxFDA")
areaPos <- DefineAreaPosition(NULL, FileName = areafile)

areaPosPost <- appendAreaCode(areaPos, strp, "Latitude", "Longitude", "AreaAppended")
expect_true(all(as.integer(areaPosPost$Area) == as.integer(areaPosPost$AreaAppended)))

areaPosPost <- appendAreaCode(areaPos, mainareaFdir2017, "Latitude", "Longitude", "AreaAppended")
expect_true(all(!is.na((areaPosPost$AreaAppended))))

areaPosPost <- appendAreaCode(areaPos, mainareaFdir2018, "Latitude", "Longitude", "AreaAppended")
expect_true(all(as.integer(areaPosPost$Area) == as.integer(areaPosPost$AreaAppended)))


context("test-StoxBaselineFunctions: appendAreaCode wrong projection")
strp <- sp::spTransform(strp, sp::CRS("+proj=longlat +datum=NAD83"))
expect_error(appendAreaCode(areaPos, strp, "Latitude", "Longitude", "AreaAppended"))

context("test-StoxBaselineFunctions: appendAreaCode non-numeric lat")
areaPos[["Latitude"]] <- as.character(areaPos[["Latitude"]])
expect_error(appendAreaCode(areaPos, strp, "Latitude", "Longitude", "AreaAppended"))

areaPos[["Latitude"]] <- NULL
expect_error(appendAreaCode(areaPos, strp, "Latitude", "Longitude", "AreaAppended"))


context("test-StoxBaselineFunctions: appendPosition")
areaTab <- DefineAreaPosition(FileName = areafile)[,c("Area", "Location")]
areaTabAppended <- appendPosition(areaTab, mainareaFdir2018, "Area", "lat", "lon")
areaTabReAppended <- appendAreaCode(areaTabAppended, mainareaFdir2018, "lat", "lon", "Area2")
expect_true(all(areaTabReAppended$Area == areaTabReAppended$Area2))

context("test-StoxBaselineFunctions: appendPosition wrong projection")
strp <- sp::spTransform(strp, sp::CRS("+proj=merc"))
expect_warning(appendPosition(areaTab, strp, "Area", "lat", "lon"))


