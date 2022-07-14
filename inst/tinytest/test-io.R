fdiropen <- system.file("testresources", "landingsvariants", "openfdir.2021.csv", package="RstoxFDA")
read <- RstoxFDA:::readFdirOpenLandings(fdiropen)
expect_equal(nrow(read), 9)

lss <- RstoxFDA:::convertToLssData(openFdirData=read)
landingdata <- RstoxData::convertToLandingData(lss)
expect_true(RstoxData::is.LandingData(landingdata))
expect_equal(nrow(landingdata$ConvertedData$Salgslagdata), 9)

fdir_archive <- system.file("testresources", "landingsvariants", "landings_csv.csv", package="RstoxFDA")
archive <- RstoxFDA:::readFdirLandingsArchive(fdir_archive)
expect_equal(ncol(archive), 18)
expect_true(sum(archive$UTBET)>0)
expect_true(sum(archive$VEKT)>0)