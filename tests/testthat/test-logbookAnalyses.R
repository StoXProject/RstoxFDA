context("Test assignTripIdLogbooks")

logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))

tripIds <- makeTripIds(land)
expect_warning(logbTI <-assignTripIdLogbooks(logb, tripIds))
expect_equal(sum(is.na(logbTI$tripid)),1)
expect_equal(ncol(logb), ncol(logbTI)-1)
expect_equal(nrow(logb), nrow(logbTI))

context("Test calculateLogbookPartitionByTrip")
logbTIC <- logbTI[!is.na(logbTI$tripid)]
logbTIC$mainarea <- substring(logbTIC$LOKASJON_START, 1, 2)
logbTIC$quarter <- quarters(logbTIC$STARTTIDSPUNKT, abbreviate=T)
result <- calculateLogbookPartitionByTrip(logbTIC, groupCols = c("mainarea", "quarter"))
resultmerged <- merge(result$fractions, result$groupDefinition)
expect_equal(nrow(resultmerged), nrow(result$fractions))
trippagg <- aggregate(resultmerged$fraction~resultmerged$tripid+resultmerged$species, FUN=sum)
expect_true(all(trippagg$`resultmerged$fraction` == 1))
