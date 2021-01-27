

context("test-StoxBaselineFunctions: SetTimeBiotic")
bioticfiles <- c(f1=system.file("testresources","biotic_v3_example.xml", package="RstoxFDA"), f2=system.file("testresources","biotic_v3_example.xml", package="RstoxFDA"))
BioticData <- RstoxData::ReadBiotic(bioticfiles)
BioticData$biotic_v3_example.xml$fishstation$stationstartdate <- BioticData$biotic_v3_example.xml$fishstation$stationstopdate
StoxBioticPre <- RstoxData::StoxBiotic(BioticData)
BioticDataPost <- SetTimeBiotic(BioticData)
expect_true(all(!is.na(BioticDataPost$biotic_v3_example.xml$fishstation$stationstarttime)))
StoxBioticPost <- RstoxData::StoxBiotic(BioticDataPost)
expect_lt(sum(is.na(StoxBioticPost$Station$DateTime)), sum(is.na(StoxBioticPre$Station$DateTime)))

#test other time format
BioticDataPost <- SetTimeBiotic(BioticData, Time="21:00:01Z")
StoxBioticPost <- RstoxData::StoxBiotic(BioticDataPost)
expect_true(all(grepl("21:00:01", as.character(StoxBioticPost$Station$DateTime))))

#test missing Z
expect_error(SetTimeBiotic(BioticData, Time="21:00:01"), "Invalid time specification: 21:00:01. Provide as %H:%M:%SZ, e.g: 12:00:00Z")

#test wrong time format
expect_error(SetTimeBiotic(BioticData, Time="32:00:01Z"), "Invalid time specification: 32:00:01Z. Provide as %H:%M:%SZ, e.g: 12:00:00Z")
BioticData$biotic_v3_example.xml$fishstation$stationstarttime[1] <- "21:00:02"

#test overwrite
expect_equal(SetTimeBiotic(BioticData, Time="21:00:01Z")$biotic_v3_example.xml$fishstation$stationstarttime[1], "21:00:02")
expect_equal(SetTimeBiotic(BioticData, Time="21:00:01Z", OverWrite = T)$biotic_v3_example.xml$fishstation$stationstarttime[1], "21:00:01Z")

context("test-StoxBaselineFunctions: SetStartDateBiotic")
bioticfiles <- system.file("testresources","biotic_v3_example.xml", package="RstoxFDA")
BioticData <- RstoxData::ReadBiotic(bioticfiles)
StoxBioticPre <- RstoxData::StoxBiotic(BioticData)
BioticDataPost <- SetStartDateBiotic(BioticData)
expect_true(all(!is.na(BioticDataPost$biotic_v3_example.xml$fishstation$stationstartdate)))
expect_lt(sum(is.na(StoxBioticPost$Station$DateTime)), sum(is.na(StoxBioticPre$Station$DateTime)))

#test overwrite
BioticData$biotic_v3_example.xml$fishstation$stationstartdate <- "1982-09-15Z"
expect_equal(SetStartDateBiotic(BioticData)$biotic_v3_example.xml$fishstation$stationstartdate[1], "1982-09-15Z")
expect_equal(SetStartDateBiotic(BioticData, OverWrite = T)$biotic_v3_example.xml$fishstation$stationstartdate[1], "2018-04-04Z")

context("test-StoxBaselineFunctions: AddGearGroupStoxLanding")
gearDef <- RstoxData::DefineTranslation(NULL, F, "ResourceFile", NULL, system.file("testresources","geargroupsLandings.txt", package="RstoxFDA"))
landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
stoxLandingPost <- AddGearGroupStoxLanding(stoxLandingPre, gearDef)
expect_true("GearGroup" %in% names(stoxLandingPost$landings))
expect_true(all(!is.na(stoxLandingPost$landings$GearGroup)))

context("teadt-StoxBaselineFunctions: AddGearGroupStoxBiotic")
gearDef <- RstoxData::DefineTranslation(NULL, F, "ResourceFile", NULL, system.file("testresources","geargroupsBiotic.txt", package="RstoxFDA"))
stoxbiotic <- readRDS(system.file("testresources","StoxBioticData.rds", package="RstoxFDA"))
stoxbioticPost <- AddGearGroupStoxBiotic(stoxbiotic, gearDef)
expect_true("GearGroup" %in% names(stoxbioticPost$Haul))
expect_equal(sum(!is.na(stoxbioticPost$Haul$GearGroup)), sum(!is.na(stoxbiotic$Haul$Gear)))

context("test-StoxBaselineFunctions: DefinePeriod")
temp <- DefinePeriod(NULL)
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefinePeriod useProcessData")
temp <- DefinePeriod(NULL, UseProcessData = T)
expect_true(is.null(temp))

context("test-StoxBaselineFunctions: DefinePeriod Month")
temp <- DefinePeriod(NULL, TemporalCategory = "Month")
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 12)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefinePeriod non-seasonal")
temp <- DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("04-02-2018", "04-09-2018"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 4)
expect_false(any(is.na(temp$StartYear)))


context("test-StoxBaselineFunctions: DefinePeriod unrecognized category")
expect_error(DefinePeriod(NULL, TemporalCategory = "Something"), "Temporal category Something not recognized.")

context("test-StoxBaselineFunctions: DefinePeriod Custom")
temp <- DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("05-02","15-09"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 2)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefinePeriod Custom seasonal")
temp <- DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("05-02","15-09"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 2)
expect_equal(ncol(temp), 4)

context("test-StoxBaselineFunctions: DefinePeriod Custom non-seasonal")
expect_error(DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-01","15-09","01-01")), "Need to provide unique periods.")
temp2 <- DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-01-2016","15-09-2016"))
expect_true(data.table::is.data.table(temp))

expect_equal(nrow(temp2), 3)
expect_equal(ncol(temp2), 4)
expect_false(any(is.na(temp2$StartYear)))



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
ageerror <- DefineAgeErrorMatrix(FileName = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 15)
expect_equal(ncol(ageerror), 16)

context("test-StoxBaselineFunctions: DefineAgeErrorMatrix useProcessData")
nullAE <- DefineAgeErrorMatrix(NULL, FileName = ageerorfile, UseProcessData = T)
expect_true(is.null(nullAE))

context("test-StoxBaselineFunctions: DefineAgeErrorMatrix non-symmetric")
ageerorfile <- system.file("testresources","AgeNonSym.txt", package="RstoxFDA")
ageerror <- DefineAgeErrorMatrix(FileName = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 14)
expect_equal(ncol(ageerror), 16)

context("test-StoxBaselineFunctions: DefineAgeErrorMatrix malformed")
ageerorfile <- system.file("testresources","AgeMalformed.txt", package="RstoxFDA")
expect_error(DefineAgeErrorMatrix(FileName = ageerorfile),"Malformed resource file. All probabilities must be in >=0 and <=1.")

ageerorfile <- system.file("testresources","AgeMalformed2.txt", package="RstoxFDA")
expect_error(DefineAgeErrorMatrix(FileName = ageerorfile),"Malformed resource file. Columns must sum to 1.")




context("test-StoxBaselineFunctions: DefineClassificationError")
classerorfile <- system.file("testresources","classificationError.txt", package="RstoxFDA")
classerror <- DefineClassificationError(resourceFilePath = classerorfile)
expect_true(data.table::is.data.table(classerror))
expect_equal(nrow(classerror), 1)
expect_equal(ncol(classerror), 8)

context("test-StoxBaselineFunctions: DefineClassificationError useProcessdata")
classNULL <- DefineClassificationError(NULL, resourceFilePath = classerorfile, useProcessData = T)
expect_true(is.null(classNULL))


context("test-StoxBaselineFunctions: appendTemporal")
temp <- DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-10","01-12"))
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
tempMisspec$StartYear[1] <- 1993
expect_error(appendTemporal(tabExamplePre, "period", tempMisspec, datecolumns = c("stopD", "startD")), "Year is provided for some, but not all temporal definitions.")

tempYearspec <- temp
temp$StartYear <- 2019
expect_error(appendTemporal(tabExamplePre, "period", temp, datecolumns = c("stopD", "startD")), "Some dates preced the first temporal category.")

tabMonth  <- tabExamplePre

#test using month
monthCat <- DefinePeriod(NULL, TemporalCategory = "Month")
tabMonthPost <- appendTemporal(tabMonth, "period", monthCat, datecolumns = c("stopD", "startD"))
expect_equal(tabMonthPost$period[1], "October")
tabMonthPost <- appendTemporal(tabMonth, "period", monthCat, datecolumns = c("startD", "stopD"))
expect_equal(tabMonthPost$period[1], "September")

#test using quarter
monthCat <- DefinePeriod(NULL, TemporalCategory = "Quarter")
tabMonthPost <- appendTemporal(tabMonth, "period", monthCat, datecolumns = c("stopD", "startD"))
expect_equal(tabMonthPost$period, c("Q4", "Q1", "Q4"))


tabMultiYear <- tabExamplePre
my <- DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-10-2019","01-12-2019"))
tabMultiYear$stopD[2] <- as.Date("2020-10-01")
expect_error(appendTemporal(tabMultiYear, "period", my, datecolumns = c("stopD", "startD")),"Year is provided in temporal definitions, but does not contain definitions for all years in data.")

context("test-StoxBaselineFunctions: AddPeriodStoxBiotic")
stoxbiotic <- readRDS(system.file("testresources","StoxBioticData.rds", package="RstoxFDA"))
quart <- DefinePeriod(NULL, TemporalCategory = "Quarter")
stoxbioticPost <- AddPeriodStoxBiotic(stoxbiotic, quart)
expect_true("Period" %in% names(stoxbioticPost$Station))
expect_true(all(c("Q1", "Q2") %in% stoxbioticPost$Station$Period))

context("test-StoxBaselineFunctions: AddPeriodStoxLanding")
stoxlanding <- readRDS(system.file("testresources","StoxLandingData.rds", package="RstoxFDA"))
quart <- DefinePeriod(NULL, TemporalCategory = "Quarter")
stoxlandingPost <- AddPeriodStoxLanding(stoxlanding, quart)
expect_true("Period" %in% names(stoxlandingPost$landings))
expect_true(all(c("Q1", "Q2") %in% stoxlandingPost$landings$Period))

context("test-StoxBaselineFunctions: SetAreaPositionsBiotic")
areaPos <- DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
bioticfiles <- system.file("testresources","biotic_v3_example.xml", package="RstoxFDA")
BioticData <- RstoxData::ReadBiotic(bioticfiles)
BioticData$biotic_v3_example.xml$fishstation$area <- c("03", "02")
expect_error(SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="location", System="2", Overwrite = T), "Not all areas and locations in 'BioticData' are defined in 'AreaPosition. Missing: 03-35,02-27")
BioticDataPost <- SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="None", System="2", Overwrite = T)
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)>1))

#test when nothing needs writing
BioticDataPost <- SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="None", System="2")
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)<1e-10))
BioticDataPost <- SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="location", System="2")
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)<1e-10))

#test with location
BioticData$biotic_v3_example.xml$fishstation$location <- c("22", "08")
BioticDataPost <- SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="location", System="2", Overwrite = T)
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)>1))


context("test-StoxBaselineFunctions: AppendPositionLanding missing")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_incl.txt", package="RstoxFDA")
areaPos <- DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
expect_error(AddAreaPositionStoxLanding(stoxLandingPre, areaPos, LocationVariable = "Location"))
expect_error(AddAreaPositionStoxLanding(stoxLandingPre, areaPos))

context("test-StoxBaselineFunctions: AppendPositionLanding regular run")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_compl.txt", package="RstoxFDA")
areaPos <- DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
landingPost <- AddAreaPositionStoxLanding(stoxLandingPre, areaPos)
expect_true(all(c("Latitude", "Longitude") %in% names(landingPost$landings)))
expect_true(all(!is.na(landingPost$landings$Latitude)))
expect_true(all(!is.na(landingPost$landings$Longitude)))

lata <- min(landingPost$landings$Latitude[1])
landingPost <- AddAreaPositionStoxLanding(stoxLandingPre, areaPos, LocationVariable = "Location")
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

