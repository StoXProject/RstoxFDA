
context("test-StoxBaselineFunctions: makeUnifiedDefinitionLookupList")
regularfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")

parsedfile <- DefineGear(resourceFilePath = regularfile)

mappings <- makeUnifiedDefinitionLookupList(parsedfile)
expect_true("StoxLandingData" %in% names(mappings))
expect_equal(length(mappings), 2)
expect_equal(length(unique(mappings$StoxLandingData)), 7)
expect_equal(length(unique(mappings$StoxBioticData)), 7)

context("test-StoxBaselineFunctions: makeUnifiedDefinitionLookupList one format only")
mappings <- makeUnifiedDefinitionLookupList(parsedfile, formats = c("StoxBioticData"))
expect_equal(length(mappings), 1)
expect_equal(length(unique(mappings$StoxBioticData)), 7)
expect_false("StoxLandingData" %in% names(mappings))

context("test-StoxBaselineFunctions: makeUnifiedDefinitionLookupList missing formats")
expect_error(makeUnifiedDefinitionLookupList(parsedfile, formats = c("StoxBioticData", "ICESbiotic")), "Not all formats found in resource. Missing: ICESbiotic")

context("test-StoxBaselineFunctions: makeUnifiedDefinitionLookupList redefined codes")
errorfile <- system.file("testresources","gearfactor_error.txt", package="RstoxFDA")
parsedfile <- DefineGear(resourceFilePath = errorfile)
expect_error(makeUnifiedDefinitionLookupList(parsedfile), "Codes redefined: 3714")

context("test-StoxBaselineFunctions: makeUnifiedDefinitionLookupList repeated keys")
errorfile <- system.file("testresources","gearfactor_errorkeys.txt", package="RstoxFDA")
expect_error(DefineGear(resourceFilePath = errorfile), "Malformed resource file. Non-unique keys: repition in first two columns.")





context("test-StoxBaselineFunctions: DefineGear")
regularfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")
gear <- DefineGear(resourceFilePath = regularfile)
expect_true(data.table::is.data.table(gear))
expect_equal(nrow(gear), 14)
expect_equal(ncol(gear), 3)

context("test-StoxBaselineFunctions: DefineGear useProcessData")
nullgear <- DefineGear(NULL, resourceFilePath = regularfile, useProcessData = T)
expect_true(is.null(nullgear))
errorfile <- system.file("testresources","gearfactor_error.txt", package="RstoxFDA")
gg <- DefineGear(gear, resourceFilePath = errorfile, useProcessData = T)
expect_equal(nrow(gear), 14)
expect_equal(ncol(gear), 3)
gg <- DefineGear(gear, resourceFilePath = NULL, useProcessData = T)
expect_equal(nrow(gear), 14)
expect_equal(ncol(gear), 3)




context("test-StoxBaselineFunctions: DefineTemporalCategories")
temp <- DefineTemporalCategories(NULL)
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefineTemporalCategories useProcessData")
temp <- DefineTemporalCategories(NULL, useProcessData = T)
expect_true(is.null(temp))

context("test-StoxBaselineFunctions: DefineTemporalCategories Month")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Month")
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 12)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefineTemporalCategories non-seasonal")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Month", years=c(2015,2016))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 24)
expect_equal(ncol(temp), 4)
expect_false(any(is.na(temp$year)))


context("test-StoxBaselineFunctions: DefineTemporalCategories unrecognized category")
expect_error(DefineTemporalCategories(NULL, temporalCategory = "Something"), "Temporal category Something not recognized.")

context("test-StoxBaselineFunctions: DefineTemporalCategories Custom")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("05-02","15-09"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 2)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefineTemporalCategories Custom seasonal")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("05-02","15-09"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 2)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefineTemporalCategories Custom non-seasonal")
expect_error(DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("01-01","15-09","01-01"), years=c(2015, 2016)), "Need to provide unique periods.")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("01-01","15-09"), years=c(2015, 2016))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 4)
expect_false(any(is.na(temp$year)))



context("test-StoxBaselineFunctions: DefineAreaPosition")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_incl.txt", package="RstoxFDA")
areaPos <- DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
expect_true(data.table::is.data.table(areaPos))
expect_equal(nrow(areaPos), 21)
expect_equal(ncol(areaPos), 4)

context("test-StoxBaselineFunctions: DefineAreaPosition useProcessData")
nullPos <- DefineAreaPosition(NULL, FileName = regularfile, UseProcessData = T)
expect_true(is.null(nullPos))

context("test-StoxBaselineFunctions: DefineAreaPosition malformed")
errorfile <- system.file("testresources","areaPosError.txt", package="RstoxFDA")
expect_error(DefineAreaPosition(NULL, FileName = errorfile), "Malformed resource file. Some Area does not have coordinates defined for the case when location is missing.")


context("test-StoxBaselineFunctions: DefineAreaPosition stratumPolygon")
areaPos <- DefineAreaPosition(NULL, StratumPolygon = mainareaFdir2018, DefinitionMethod = "StratumPolygon")
expect_true(is.AreaPosition(areaPos))


context("test-StoxBaselineFunctions: DefineCarNeighbours")
carfile <- system.file("testresources","mainarea_neighbour.txt", package="RstoxFDA")
car <- DefineCarNeighbours(NULL, FileName = carfile)
expect_true(data.table::is.data.table(car))
expect_equal(nrow(car), 60)
expect_equal(ncol(car), 2)

context("test-StoxBaselineFunctions: DefineCarNeighbours useProcessData")
nullCar <- DefineCarNeighbours(NULL, FileName = carfile, UseProcessData = T)
expect_true(is.null(nullCar))

context("test-StoxBaselineFunctions: DefineCarNeighbours non-symmetric")
errorfile <- system.file("testresources","mainarea_error.txt", package="RstoxFDA")
expect_error(DefineCarNeighbours(NULL, FileName = errorfile), "Neighbour definition not symmetric. 1 is neighbour of 0 but not vice versa.")

context("test-StoxBaselineFunctions: DefineCarNeighbours repeated key")
errorfile <- system.file("testresources","mainarea_error2.txt", package="RstoxFDA")
expect_error(DefineCarNeighbours(NULL, FileName = errorfile), "Malformed resource file, Non-unique keys: repition in first column: 1")




context("test-StoxBaselineFunctions: DefineAgeErrorMatrix")
ageerorfile <- system.file("testresources","AgeErrorHirstEtAl2012.txt", package="RstoxFDA")
ageerror <- DefineAgeErrorMatrix(resourceFilePath = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 15)
expect_equal(ncol(ageerror), 16)

context("test-StoxBaselineFunctions: DefineAgeErrorMatrix useProcessData")
nullAE <- DefineAgeErrorMatrix(NULL, resourceFilePath = ageerorfile, useProcessData = T)
expect_true(is.null(nullAE))

context("test-StoxBaselineFunctions: DefineAgeErrorMatrix non-symmetric")
ageerorfile <- system.file("testresources","AgeNonSym.txt", package="RstoxFDA")
ageerror <- DefineAgeErrorMatrix(resourceFilePath = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 14)
expect_equal(ncol(ageerror), 16)

context("test-StoxBaselineFunctions: DefineAgeErrorMatrix malformed")
ageerorfile <- system.file("testresources","AgeMalformed.txt", package="RstoxFDA")
expect_error(DefineAgeErrorMatrix(resourceFilePath = ageerorfile),"Malformed resource file. All probabilities must be in >=0 and <=1.")

ageerorfile <- system.file("testresources","AgeMalformed2.txt", package="RstoxFDA")
expect_error(DefineAgeErrorMatrix(resourceFilePath = ageerorfile),"Malformed resource file. Columns must sum to 1.")




context("test-StoxBaselineFunctions: DefineClassificationError")
classerorfile <- system.file("testresources","classificationError.txt", package="RstoxFDA")
classerror <- DefineClassificationError(resourceFilePath = classerorfile)
expect_true(data.table::is.data.table(classerror))
expect_equal(nrow(classerror), 1)
expect_equal(ncol(classerror), 8)

context("test-StoxBaselineFunctions: DefineClassificationError useProcessdata")
classNULL <- DefineClassificationError(NULL, resourceFilePath = classerorfile, useProcessData = T)
expect_true(is.null(classNULL))




context("test-StoxBaselineFunctions: AppendGearStoxBiotic")
gearfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")
stoxBioticMock <- system.file("testresources","StoxBioticDataMock.txt", package="RstoxFDA")

gear <- DefineGear(NULL, resourceFilePath = gearfile)
stoxBioticPre <- readTabSepFile(stoxBioticMock, col_types = "cccc")

stoxBioticPost <- AppendGearStoxBiotic(stoxBioticPre, gear)
expect_true(data.table::is.data.table(stoxBioticPost))
expect_equal(ncol(stoxBioticPost), ncol(stoxBioticPre) + 1)
expect_false(any(is.na(stoxBioticPost$UnifiedGear)))

context("test-StoxBaselineFunctions: AppendGearStoxBiotic NA in gear")
sbError <- stoxBioticPre
sbError$gear[1] <- NA
expect_error(AppendGearStoxBiotic(sbError, gear))

context("test-StoxBaselineFunctions: AppendGearStoxBiotic unkown gear")
sbError <- stoxBioticPre
sbError$gear[1] <- "9999"
expect_error(AppendGearStoxBiotic(sbError, gear), "Conversion not defined for all codes. Missing for: 9999")

context("test-StoxBaselineFunctions: AppendGearStoxBiotic existing gear column")
expect_error(AppendGearStoxBiotic(stoxBioticPre, gear, "gear"), "Column with name 'gear' already exists.")




context("test-StoxBaselineFunctions: AppendGearStoxLanding")
gearfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")
stoxLandingXml <- system.file("testresources","landing.xml", package="RstoxFDA")

gear <- DefineGear(NULL, resourceFilePath = gearfile)
stoxLandingPre <- RstoxData::StoxLanding(RstoxData::ReadLanding(stoxLandingXml))
stoxLandingPost <- AppendGearStoxLanding(stoxLandingPre, gear)
expect_true(RstoxData::is.StoxLandingData(stoxLandingPost))
expect_equal(ncol(stoxLandingPost$landings), ncol(stoxLandingPre$landings) + 1)
expect_false(any(is.na(stoxLandingPost$UnifiedGear)))

#try on proper format as well.
landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
stoxLandingPost <- AppendGearStoxLanding(stoxLandingPre, gear)
expect_false(any(is.na(stoxLandingPost$UnifiedGear)))


context("test-StoxBaselineFunctions: appendTemporal")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("01-10","01-12"))
tabExampleFile <- system.file("testresources","startStopExample.txt", package="RstoxFDA")
tabExamplePre <- readTabSepFile(tabExampleFile, col_types = "ccccDD")
tabExamplePost <- appendTemporal(tabExamplePre, "period", temp, datecolumns = c("startD", "stopD"))
expect_equal(tabExamplePost$period[1], "[01-12, 01-10>")
expect_equal(tabExamplePost$period[2], "[01-12, 01-10>")
expect_equal(tabExamplePost$period[3], "[01-10, 01-12>")

tabExamplePost <- appendTemporal(tabExamplePre, "period", temp, datecolumns = c("stopD", "startD"))
expect_equal(tabExamplePost$period[1], "[01-10, 01-12>")
expect_equal(tabExamplePost$period[2], "[01-12, 01-10>")
expect_equal(tabExamplePost$period[3], "[01-10, 01-12>")

tabExampleMissing <- tabExamplePre
tabExampleMissing$stopD[2] <- NA
expect_error(appendTemporal(tabExampleMissing, "period", temp, datecolumns = c("stopD", "startD")), "NA for some dates")

tempMisspec <- temp
tempMisspec$year[1] <- 1993
expect_error(appendTemporal(tabExamplePre, "period", tempMisspec, datecolumns = c("stopD", "startD")), "Year is provided for some, but not all temporal definitions.")

tempYearspec <- temp
temp$year <- 2019
expect_error(appendTemporal(tabExamplePre, "period", temp, datecolumns = c("stopD", "startD")), "Some dates preced the first temporal category.")

my <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("01-10","01-12"), years = c(2019,2020))
tabMultiYear <- tabExamplePre
tabMultiYear$stopD[2] <- as.Date("2019-01-01")
expect_error(appendTemporal(tabMultiYear, "period", my, datecolumns = c("stopD", "startD")), "Some dates preced the first temporal category.")
tabMultiYear$stopD[2] <- as.Date("2019-10-01")
appendTemporal(tabMultiYear, "period", my, datecolumns = c("stopD", "startD"))

my <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("01-10","01-12"), years = c(2019))
tabMultiYear$stopD[2] <- as.Date("2020-10-01")
expect_error(appendTemporal(tabMultiYear, "period", my, datecolumns = c("stopD", "startD")),"Year is provided in temporal definitions, but does not contain definitions for all years in data.")


context("test-StoxBaselineFunctions: AppendTemporalStoxLanding")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Quarter")
landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
stoxLandingPost <- AppendTemporalStoxLanding(stoxLandingPre, temp)
expect_false(any(is.na(stoxLandingPost$TemporalCategory)))

context("test-StoxBaselineFunctions: AppendTemporalStoxLanding used colName")
expect_error(AppendTemporalStoxLanding(stoxLandingPre, temp, columnName = "CatchDate"), "s")


context("test-StoxBaselineFunctions: AppendPositionLanding missing")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_incl.txt", package="RstoxFDA")
areaPos <- DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
expect_error(AddAreaPositionStoxLanding(stoxLandingPre, areaPos, resolution = "Location"))
expect_error(AddAreaPositionStoxLanding(stoxLandingPre, areaPos))

context("test-StoxBaselineFunctions: AppendPositionLanding regular run")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_compl.txt", package="RstoxFDA")
areaPos <- DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
landingPost <- AddAreaPositionStoxLanding(stoxLandingPre, areaPos)
expect_true(all(c("Latitude", "Longitude") %in% names(landingPost$landings)))
expect_true(all(!is.na(landingPost$landings$Latitude)))
expect_true(all(!is.na(landingPost$landings$Longitude)))

lata <- min(landingPost$landing$Latitude[1])
landingPost <- AddAreaPositionStoxLanding(stoxLandingPre, areaPos, LocationCol = "Location")
expect_false(lata == min(landingPost$landings$Latitude[1]))

context("test-StoxBaselineFunctions: AppendPositionLanding used colName")
stoxLandingPre <- AddAreaPositionStoxLanding(stoxLandingPre, areaPos)
expect_error(AddAreaPositionStoxLanding(stoxLandingPre, areaPos), "Column Latitude already exists.")





context("test-StoxBaselineFunctions: AppendStratumStoxLanding")

strp <- mainareaFdir2018
sp::proj4string(strp) <- sp::CRS("+proj=longlat +datum=WGS84")

areafile <- system.file("testresources","mainarea_fdir_from_2018_compl.txt", package="RstoxFDA")
areaPos <- DefineAreaPosition(NULL, FileName = areafile, StratumPolygon = NULL)

landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
landingWpos <- AddAreaPositionStoxLanding(stoxLandingPre, areaPos)

landingPost <- AddStratumStoxLanding(landingWpos, strp)
expect_true(all(as.integer(landingPost$Stratum)==as.integer(landingPost$area)))

