
context("Test tabulate Fisheries")
data(landings)
ss <- tabulateFisheries(landings, cellCols = c("Area"))
expect_equal(nrow(ss), 15)
ss <- tabulateFisheries(landings)
expect_equal(nrow(ss), 193)

context("Test make trip IDs")
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
tripIds <- makeTripIds(land)
expect_equal(names(tripIds), c("vesselId", "time", "tripId"))
expect_gt(nrow(tripIds),0)

context("Test append trip landings")
landA <- appendTripIdLandings(land, tripIdCol = "tt")
expect_true(all(!is.na(landA$tt)))

context("Test append trip landings error: data frame")
expect_error(appendTripIdLandings(as.data.frame(land), tripIdCol = "tt"), "Parameter 'landings' must be a data table")



logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
land <- appendTripIdLandings(land, tripIdCol = "tt")
tripIds <- makeTripIds(land)
expect_warning(logbTI <-appendTripIdLogbooks(logb, tripIds, tripIdCol = "tt"))
logbTI <- logbTI[!is.na(logbTI$tt),]

#hack to force some redistr
logbTI$FANGSTART_FAO[logbTI$tt == "RCRC/2018-02-26" & logbTI$FANGSTART_FAO=="HER"] <- "CAP"
logbTI$FANGSTART_FAO[logbTI$tt == "RCRC/2018-04-11" & logbTI$FANGSTART_FAO=="COD"] <- "WHB"
logbTI$catchId <- 1:nrow(logbTI)

context("Test imputeCatchesLandings")
expect_warning(imputedLandings <- imputeCatchesLandings(land, logbTI, tripIdCol = "tt", catchIdCol = "catchId"), "Not all species-trips")
expect_equal(sum(imputedLandings$Bruttovekt), sum(land$Bruttovekt))
expect_equal(sum(imputedLandings$Rundvekt), sum(land$Rundvekt))
expect_equal(sum(imputedLandings$Produktvekt), sum(land$Produktvekt))

context("Test imputeCatchesLandings, not adjusting prod weight")
expect_warning(imputedLandings <- imputeCatchesLandings(land, logbTI, tripIdCol = "tt", catchIdCol = "catchId", valueColumns=c("Bruttovekt", "Rundvekt")), "Not all species-trips")
expect_equal(sum(imputedLandings$Bruttovekt), sum(land$Bruttovekt))
expect_equal(sum(imputedLandings$Rundvekt), sum(land$Rundvekt))
expect_true(sum(imputedLandings$Produktvekt) > sum(land$Produktvekt))
# fails on github

alteredLandings <- imputedLandings[imputedLandings$tt %in% logbTI$tt,]
nonAlteredLandings <- imputedLandings[!(imputedLandings$tt %in% logbTI$tt),]


context("Test imputeCatchesLandings check that non-altered Landings are untouched")
comp <- merge(nonAlteredLandings[, c("tt", "Rundvekt")], land[, c("tt", "Rundvekt")], by=c( "tt"))
expect_true(all(comp$Rundvekt.x == comp$Rundvekt.y))
context("Test imputeCatchesLandings check that altered Landings are altered")
comp <- merge(alteredLandings[, c("tt", "Rundvekt")], land[, c("tt", "Rundvekt")], by=c("tt"))
expect_true(any(comp$Rundvekt.x != comp$Rundvekt.y))
# fails on github

context("Test imputeCatchesLandings check that altered Landings have proprotions equal to logb")
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

context("Test imputeCatchesLandings param error")
expect_error(imputeCatchesLandings(land, logbTI, tripIdCol = "tt", catchIdCol = "catchId", valueColumns=c("Bruttovekt", "Rundvek")), "Not all columns in 'valueColumns' are found in 'landings'")
expect_error(imputeCatchesLandings(land, logbTI, tripIdCol = "ttt", catchIdCol = "catchId", valueColumns=c("Bruttovekt", "Rundvekt")), "tripIdCol' is not found in 'landings'")

context("Test logbookAdjustment")
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))

#hack logb for test
logb$FANGSTART_FAO[logb$RC=="RCRC"] <- "CAP"
logb <- logb[1:8,]
land[["Hovedomr\u00E5de (kode)"]][land$`Art FAO (kode)`=="CAP"] <- "12"
expect_warning(landAdj <- logbookAdjustment(land, logb))
expect_true(nrow(landAdj) > nrow(land))
expect_gt(sum(is.na(landAdj[["Hovedomr\u00E5de"]])), 0)
expect_equal(sum(landAdj$Rundvekt), sum(land$Rundvekt))
expect_equal(sum(duplicated(landAdj$Linjenummer)),0)
expect_equal(ncol(landAdj), ncol(land))
expect_true(!(all(landAdj[["Hovedomr\u00E5de (kode)"]][landAdj$`Art FAO (kode)`=="CAP"] %in% land[["Hovedomr\u00E5de (kode)"]][land$`Art FAO (kode)`=="CAP"])))

context("Test logbookAdjustment add columns")
expect_warning(landAdj <- logbookAdjustment(land, logb, addColumns=c("START_LG", "START_LT", "MASKEVIDDE")))
expect_true(all(c("START_LG", "START_LT", "MASKEVIDDE") %in% names(landAdj)))
expect_true(!all(is.na(landAdj$START_LG)))

context("Test logbookAdjustment filter gear")
landAdj <- logbookAdjustment(land, logb, gearCodes = c("53"))
expect_warning(logbookAdjustment(land, logb, gearCodes = c("11")))



