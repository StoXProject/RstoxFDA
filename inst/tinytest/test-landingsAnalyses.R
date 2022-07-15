library(data.table)
#context("Test logbook against reference impl")
log <- readRDS(system.file("testresources", "logb_cod_2021_extract.rds", package="RstoxFDA"))
land <- readRDS(system.file("testresources", "land_cod_2021_extract.rds", package="RstoxFDA"))

log <- log[!is.na(log$LOKASJON_START) & !is.na(log$START_LG) & !is.na(log$START_LT) & !is.na(log$STARTTIDSPUNKT) & !is.na(log$FANGSTART_FAO),]

landAdjRef <- RstoxFDA:::adjustWithLogbookRefImpl(land, log, "COD", 51)
suppressWarnings(landAdj <- RstoxFDA::logbookAdjustment(land, log, "51", polygons = NULL))
comp <- land[,list(weightOrig=sum(Rundvekt)), by=list(omr=get("Hovedomr\u00E5de (kode)"), gear=get("Redskap (kode)"))]
comp <- merge(comp, landAdjRef[,list(weightRef=sum(Rundvekt)), by=list(omr=get("Hovedomr\u00E5de (kode)"), gear=get("Redskap (kode)"))])
comp <- merge(comp, landAdj[,list(weight=sum(Rundvekt)), by=list(omr=get("Hovedomr\u00E5de (kode)"), gear=get("Redskap (kode)"))])
comp$diff <- comp$weight - comp$weightRef
comp$rdiff <- comp$diff / comp$weightRef
expect_equal(nrow(comp), 2)
expect_true(all(abs(comp$diff) <1e-6))

#context("Test tabulate Fisheries")

ss <- RstoxFDA::tabulateFisheries(RstoxFDA::landings, cellCols = c("Area"))
expect_equal(nrow(ss), 15)
ss <- RstoxFDA::tabulateFisheries(RstoxFDA::landings)
expect_equal(nrow(ss), 193)

#context("Test make trip IDs")
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
tripIds <- RstoxFDA::makeTripIds(land)
expect_equal(names(tripIds), c("vesselId", "time", "tripId"))
expect_true(nrow(tripIds)>0)

#context("Test append trip landings")
landA <- RstoxFDA::appendTripIdLandings(land, tripIdCol = "tt")
expect_true(all(!is.na(landA$tt)))
expect_equal(substr(landA$tt,6,15)[1], substr(landA$`Siste fangstdato`,1,12)[1])


#context("Test append trip landings error: data frame")
expect_error(RstoxFDA::appendTripIdLandings(as.data.frame(land), tripIdCol = "tt"), "Parameter 'landings' must be a data table")

logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
land <- RstoxFDA::appendTripIdLandings(land, tripIdCol = "tt")
tripIds <- RstoxFDA::makeTripIds(land)
expect_warning(logbTI <- RstoxFDA::appendTripIdLogbooks(logb, tripIds, tripIdCol = "tt"))
logbTI <- logbTI[!is.na(logbTI$tt),]

#hack to force some redistr
logbTI$FANGSTART_FAO[1:3] <- rep("CAP",3)
logbTI$FANGSTART_FAO[4:8] <- rep("WHB",5)

logbTI$catchId <- 1:nrow(logbTI)
stopifnot(nrow(logbTI)==9)
stopifnot(nrow(land)==9)
expect_equal(logbTI$FANGSTART_FAO, c("CAP", "CAP", "CAP", "WHB", "WHB", "WHB", "WHB", "WHB", "POK"))
expect_equal(land$`Art FAO (kode)`, c("CAP", "CAP", "HER", "HER", "HER", "HER", "CAP", "CAP", "WHB"))

#context("Test imputeCatchesLandings")
expect_warning(imputedLandings <- RstoxFDA::imputeCatchesLandings(land, logbTI, tripIdCol = "tt", catchIdCol = "catchId"), "Not all species-trips")
expect_equal(sum(imputedLandings$Bruttovekt), sum(land$Bruttovekt))
expect_equal(sum(imputedLandings$Rundvekt), sum(land$Rundvekt))
expect_equal(sum(imputedLandings$Produktvekt), sum(land$Produktvekt))

#context("Test imputeCatchesLandings, not adjusting prod weight")
expect_warning(imputedLandings <- RstoxFDA::imputeCatchesLandings(land, logbTI, tripIdCol = "tt", catchIdCol = "catchId", valueColumns=c("Bruttovekt", "Rundvekt")), "Not all species-trips")
expect_equal(sum(imputedLandings$Bruttovekt), sum(land$Bruttovekt))
expect_equal(sum(imputedLandings$Rundvekt), sum(land$Rundvekt))
expect_true(sum(imputedLandings$Produktvekt) > sum(land$Produktvekt))

alteredLandings <- imputedLandings[imputedLandings$tt %in% logbTI$tt,]
nonAlteredLandings <- imputedLandings[!(imputedLandings$tt %in% logbTI$tt),]


#context("Test imputeCatchesLandings check that non-altered Landings are untouched")
comp <- merge(nonAlteredLandings[, .SD, .SDcol=c("tt", "Rundvekt")], land[, .SD, .SDcol=c("tt", "Rundvekt")], by=c( "tt"))
expect_true(all(comp$Rundvekt.x == comp$Rundvekt.y))
#context("Test imputeCatchesLandings check that altered Landings are altered")
comp <- merge(alteredLandings[, .SD, .SDcol=c("tt", "Rundvekt")], land[, .SD, .SDcol=c("tt", "Rundvekt")], by=c("tt"))
expect_true(any(comp$Rundvekt.x != comp$Rundvekt.y))

#context("Test imputeCatchesLandings check that altered Landings have proprotions equal to logb")
totalGroup <- alteredLandings[,list(Total=sum(get("Rundvekt"))), by=c("catchId", "Art FAO (kode)", "tt")]
totalTrip <- alteredLandings[,list(Total=sum(get("Rundvekt"))), by=c("Art FAO (kode)","tt")]
m <- merge(totalGroup, totalTrip, by=c("tt", "Art FAO (kode)"), suffix=c(".group", ".trip"))
m$fraction <- m$Total.group / m$Total.trip
m <- m[,c("tt", "catchId", "Art FAO (kode)", "fraction")]

totalGroup <- logbTI[,list(Total=sum(get("RUNDVEKT"))), by=c("STARTTIDSPUNKT", "FANGSTART_FAO", "tt")]
totalTrip <- logbTI[,list(Total=sum(get("RUNDVEKT"))), by=c("FANGSTART_FAO", "tt")]

d <- merge(totalGroup, totalTrip, by=c("tt", "FANGSTART_FAO"), suffix=c(".group", ".trip"))
d$fraction <- d$Total.group / d$Total.trip
d <- d[,c("tt", "STARTTIDSPUNKT", "FANGSTART_FAO", "fraction")]

names(m) <- c("tt", "STARTTIDSPUNKT", "FANGSTART_FAO", "fraction")
g<-merge(m,d, by=c("tt", "STARTTIDSPUNKT", "FANGSTART_FAO"))
expect_true(all(g$fraction.x == g$fraction.y))

#context("Test imputeCatchesLandings param error")
expect_error(RstoxFDA::imputeCatchesLandings(land, logbTI, tripIdCol = "tt", catchIdCol = "catchId", valueColumns=c("Bruttovekt", "Rundvek")), "Not all columns in 'valueColumns' are found in 'landings'")
expect_error(RstoxFDA::imputeCatchesLandings(land, logbTI, tripIdCol = "ttt", catchIdCol = "catchId", valueColumns=c("Bruttovekt", "Rundvekt")), "tripIdCol' is not found in 'landings'")

#context("Test logbookAdjustment")
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))

#hack logb for test
logb$FANGSTART_FAO[logb$RC=="RCRC"] <- "CAP"
logb <- logb[1:8,]
land[["Hovedomr\u00E5de (kode)"]][land$`Art FAO (kode)`=="CAP"] <- "12"
expect_warning(landAdj <- RstoxFDA::logbookAdjustment(land, logb))
expect_true(nrow(landAdj) > nrow(land))
expect_true(sum(is.na(landAdj[["Hovedomr\u00E5de"]])) > 0)
expect_equal(sum(landAdj$Rundvekt), sum(land$Rundvekt))
expect_equal(sum(duplicated(landAdj$Linjenummer)),0)
expect_equal(ncol(landAdj), ncol(land))
expect_true(!(all(landAdj[["Hovedomr\u00E5de (kode)"]][landAdj$`Art FAO (kode)`=="CAP"] %in% land[["Hovedomr\u00E5de (kode)"]][land$`Art FAO (kode)`=="CAP"])))

#context("Test logbookAdjustment add columns")
expect_warning(landAdj <- RstoxFDA::logbookAdjustment(land, logb, addColumns=c("START_LG", "START_LT", "MASKEVIDDE")))
expect_true(all(c("START_LG", "START_LT", "MASKEVIDDE") %in% names(landAdj)))
expect_true(!all(is.na(landAdj$START_LG)))

#context("Test logbookAdjustment filter gear")
landAdj <- RstoxFDA::logbookAdjustment(land, logb, gearCodes = c("53"))

landAdj <- RstoxFDA::logbookAdjustment(land, logb, gearCodes = c("11"))
expect_true(sum(landAdj$Rundvekt[landAdj$`Redskap (kode)`=="11" & landAdj[["Hovedomr\u00E5de (kode)"]]=="12"]) != sum(land$Rundvekt[land$`Redskap (kode)`=="11" & land[["Hovedomr\u00E5de (kode)"]]=="12"]))
expect_equal(sum(landAdj$Rundvekt[landAdj$`Redskap (kode)`!="11" & landAdj[["Hovedomr\u00E5de (kode)"]]=="12"]), sum(land$Rundvekt[land$`Redskap (kode)`!="11" & land[["Hovedomr\u00E5de (kode)"]]=="12"]))
expect_true(all(landAdj$Redskap != landAdj$`Redskap (kode)`))

#context("Test logbookAdjustment filter gear code type error")
expect_error(RstoxFDA::logbookAdjustment(land, logb, gearCodes = c(53)), "'gearCodes must be provided as character")
