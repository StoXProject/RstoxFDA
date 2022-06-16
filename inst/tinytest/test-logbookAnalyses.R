
#context("Test appendTripIdLogbooks")

logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))
land$`Siste fangstdato` <- as.POSIXct(substr(land$`Siste fangstdato`,1,10), tz="CET") #force CET for test

tripIds <- RstoxFDA::makeTripIds(land)
expect_warning(logbTI <- RstoxFDA::appendTripIdLogbooks(logb, tripIds))
expect_error(RstoxFDA::appendTripIdLogbooks(logb, tripIds, timeCol = "tt"), "The columns identified by 'timeCol' or 'vesselIdCol' are not found in 'logbooks'.")
expect_error(RstoxFDA::appendTripIdLogbooks(logb, tripIds, vesselIdCol = "tt"), "The columns identified by 'timeCol' or 'vesselIdCol' are not found in 'logbooks'.")
expect_error(logbTI <- RstoxFDA::appendTripIdLogbooks(logbTI, tripIds), "The column tripid already exist in 'logbooks'.")
expect_equal(sum(is.na(logbTI$tripid)),1)
expect_equal(ncol(logb), ncol(logbTI)-1)
expect_equal(nrow(logb), nrow(logbTI))


logbEmpty <- RstoxFDA::appendTripIdLogbooks(logb[rep(F, nrow(logb))], tripIds)
expect_equal(ncol(logbTI), ncol(logbEmpty))
expect_equal(nrow(logbEmpty), 0)

# check edge cases for date

logbCD <- logb
#catch on last catch date gets included in trip.
expect_true(!any(!is.na(logbTI$tripid) & substr(logbTI$tripid,6,15) == substr(logbTI$STARTTIDSPUNKT,1,10)))
logbCD$STARTTIDSPUNKT[substr(logbCD$STARTTIDSPUNKT,1,10)=="2018-03-03"] <- as.POSIXct("2018-04-12 21:59:59", tz="UTC")
expect_warning(logbTI <- RstoxFDA::appendTripIdLogbooks(logbCD, tripIds))
logbTI[logbTI$tripid == tripIds$tripId[1],c("tripid", "STARTTIDSPUNKT")]
expect_true(any(!is.na(logbTI$tripid) & substr(logbTI$tripid,6,15) == substr(logbTI$STARTTIDSPUNKT,1,10)))

#very late UTC does not match catch date in CEST.
logbCD$STARTTIDSPUNKT[substr(logbCD$STARTTIDSPUNKT,1,10)=="2018-04-12"] <- as.POSIXct("2018-04-12 23:59:59", tz="UTC")
expect_warning(logbTI <- RstoxFDA::appendTripIdLogbooks(logbCD, tripIds))
logbTI[logbTI$tripid == tripIds$tripId[1],c("tripid", "STARTTIDSPUNKT")]
expect_true(!any(!is.na(logbTI$tripid) & substr(logbTI$tripid,6,15) == substr(logbTI$STARTTIDSPUNKT,1,10)))

#context("Test calculateLogbookPartitionByTrip")
logbTIC <- logbTI[!is.na(logbTI$tripid)]
logbTIC$mainarea <- substring(logbTIC$LOKASJON_START, 1, 2)
logbTIC$quarter <- quarters(logbTIC$STARTTIDSPUNKT, abbreviate=T)
result <- RstoxFDA::calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"))
expect_error(RstoxFDA::calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarters")), "Not all columns in 'groupCols' found in 'logbooks'")
expect_error(RstoxFDA::calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"), tripCol = "tt"), "Column 'tripCol' not found in 'logbooks")
expect_error(RstoxFDA::calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"), speciesCol = "tt"), "Column 'speciesCol' not found in 'logbooks")
expect_error(RstoxFDA::calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"), weightCol = "tt"), "Column 'weightCol' not found in 'logbooks")

resultmerged <- merge(result$fractions, result$groupDefinition, by="groupid")
expect_equal(nrow(resultmerged), nrow(result$fractions))
trippagg <- aggregate(resultmerged$fraction~resultmerged$tripid+resultmerged$species, FUN=sum)
expect_true(all(trippagg$`resultmerged$fraction` == 1))

