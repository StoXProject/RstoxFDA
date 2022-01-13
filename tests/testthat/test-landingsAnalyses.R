
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


context("Test logbook adjustment of landings")
logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
land <- appendTripIdLandings(land, tripIdCol = "tt")
tripIds <- makeTripIds(land)
expect_warning(logbTI <-appendTripIdLogbooks(logb, tripIds))

logbTIC <- logbTI[!is.na(logbTI$tripid)]
logbTIC$mainarea <- substring(logbTIC$LOKASJON_START, 1, 2)
logbTIC$quarter <- quarters(logbTIC$STARTTIDSPUNKT, abbreviate=T)
land$mainarea <- land$`Hovedområde (kode)`
land$quarter <- quarters(land$`Siste fangstdato`, abbreviate=T)
partition <- calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"))
# hack correspondance for test

partition$fractions$species[partition$fractions$tripid == "RCRC/2018-02-26" & partition$fractions$species=="HER"] <- "CAP"
partition$fractions$species[partition$fractions$tripid == "RCRC/2018-04-11" & partition$fractions$species=="COD"] <- "WHB"

expect_warning(imputedLandings <- imputeCatchesLandings(land, partition, tripIdCol = "tt"), "Not all species-trips")
expect_equal(sum(imputedLandings$Bruttovekt), sum(land$Bruttovekt))
expect_equal(sum(imputedLandings$Rundvekt), sum(land$Rundvekt))
expect_equal(sum(imputedLandings$Produktvekt), sum(land$Produktvekt))

context("Test logbook adjustment of Not adjusting prod weight")
expect_warning(imputedLandings <- imputeCatchesLandings(land, partition, tripIdCol = "tt", valueColumns=c("Bruttovekt", "Rundvekt")), "Not all species-trips")
expect_equal(sum(imputedLandings$Bruttovekt), sum(land$Bruttovekt))
expect_equal(sum(imputedLandings$Rundvekt), sum(land$Rundvekt))
expect_true(sum(imputedLandings$Produktvekt) > sum(land$Produktvekt))

alteredLandings <- imputedLandings[imputedLandings$tt %in% partition$fractions$tripid,]
nonAlteredLandings <- imputedLandings[!(imputedLandings$tt %in% partition$fractions$tripid),]


context("Test logbook adjustment check that non-altered Landings are untouched")
comp <- merge(nonAlteredLandings[, c("mainarea", "quarter", "tt", "Rundvekt")], imputedLandings[, c("mainarea", "quarter", "tt", "Rundvekt")], by=c("mainarea", "quarter", "tt"))
expect_true(all(comp$Rundvekt.x == comp$Rundvekt.y))

context("Test logbook adjustment check that altered Landings havae proprotions equal to logb")
totalGroup <- alteredLandings[,list(Total=sum(get("Rundvekt"))), by=c("mainarea", "quarter", "Art FAO (kode)", "tt")]
totalTrip <- alteredLandings[,list(Total=sum(get("Rundvekt"))), by=c("Art FAO (kode)","tt")]
m <- merge(totalGroup, totalTrip, by=c("tt", "Art FAO (kode)"), suffix=c(".group", ".trip"))
m$fraction <- m$Total.group / m$Total.trip
fracs <- merge(m[,c("tt", "Art FAO (kode)", "mainarea", "quarter", "fraction")], merge(partition$fractions, partition$groupDefinition, by="groupid")[,c("tripid", "species", "mainarea", "quarter", "fraction")], by.x=c("tt", "Art FAO (kode)", "mainarea", "quarter"), by.y=c("tripid", "species", "mainarea", "quarter"), suffix=c(".adj", "logb"))
expect_true(all(fracs$fraction.adj == fracs$fractionlogb))

context("Test logbook adjustment param error")
expect_error(imputeCatchesLandings(land, partition, tripIdCol = "tt", valueColumns=c("Bruttovekt", "Rundvek")), "Not all columns in 'valueColumns' are found in 'landings'")
expect_error(imputeCatchesLandings(land, partition, tripIdCol = "ttt", valueColumns=c("Bruttovekt", "Rundvekt")), "tripIdCol' is not found in 'landings'")
ll <- land
ll$quarter <- NULL
expect_error(imputeCatchesLandings(ll, partition, tripIdCol = "tt", valueColumns=c("Bruttovekt", "Rundvekt")), "Not all columns in group definition of ")

context("Test logbookAdjustment")
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
#hack logb for test
logb$FANGSTART_FAO[logb$RC=="RCRC"] <- "CAP"
expect_warning(landAdj <- logbookAdjustment(land, logb))
expect_true(nrow(landAdj) > nrow(land))
expect_equal(sum(landAdj$Rundvekt), sum(land$Rundvekt))
expect_equal(sum(duplicated(landAdj$Linjenummer)),0)
expect_equal(ncol(landAdj), ncol(land))
