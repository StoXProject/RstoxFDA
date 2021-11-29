
context("Test appendTripIdLogbooks")

logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))

tripIds <- makeTripIds(land)
expect_warning(logbTI <-appendTripIdLogbooks(logb, tripIds))
expect_error(appendTripIdLogbooks(logb, tripIds, timeCol = "tt"), "The columns identified by 'timeCol' or 'vesselIdCol' are not found in 'logbooks'.")
expect_error(appendTripIdLogbooks(logb, tripIds, vesselIdCol = "tt"), "The columns identified by 'timeCol' or 'vesselIdCol' are not found in 'logbooks'.")
expect_error(logbTI <-appendTripIdLogbooks(logbTI, tripIds), "The column tripid already exist in 'logbooks'.")
expect_equal(sum(is.na(logbTI$tripid)),1)
expect_equal(ncol(logb), ncol(logbTI)-1)
expect_equal(nrow(logb), nrow(logbTI))

context("Test calculateLogbookPartitionByTrip")
logbTIC <- logbTI[!is.na(logbTI$tripid)]
logbTIC$mainarea <- substring(logbTIC$LOKASJON_START, 1, 2)
logbTIC$quarter <- quarters(logbTIC$STARTTIDSPUNKT, abbreviate=T)
result <- calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"))
expect_error(calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarters")), "Not all columns in 'groupCols' found in 'logbooks'")
expect_error(calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"), tripCol = "tt"), "Column 'tripCol' not found in 'logbooks")
expect_error(calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"), speciesCol = "tt"), "Column 'speciesCol' not found in 'logbooks")
expect_error(calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"), weightCol = "tt"), "Column 'weightCol' not found in 'logbooks")
resultmerged <- merge(result$fractions, result$groupDefinition)
expect_equal(nrow(resultmerged), nrow(result$fractions))
trippagg <- aggregate(resultmerged$fraction~resultmerged$tripid+resultmerged$species, FUN=sum)
expect_true(all(trippagg$`resultmerged$fraction` == 1))
