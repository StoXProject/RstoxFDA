context("Test logbook analyses")

logb <- RstoxData::readErsFile(system.file("testresources","logbooks_mock_2018.psv", package="RstoxFDA"))
land <- RstoxData::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxFDA"))

tripIds <- makeTripIds(land)
expect_warning(logbTI <-assignTripIdLogbooks(logb, tripIds))
expect_equal(sum(is.na(logbTI$tripid)),1)
expect_equal(ncol(logb), ncol(logbTI)-1)
expect_equal(nrow(logb), nrow(logbTI))
