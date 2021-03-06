
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
