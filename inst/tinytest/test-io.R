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


lst_logb <- system.file("testresources", "logbookvariants", "logbook.lst", package="RstoxFDA")
logb <- RstoxFDA:::readLstFile(lst_logb)
expect_equal(ncol(logb), 27)
expect_true(sum(logb$VAR)>0)
expect_true(sum(logb$VEKT)>0)
expect_true(sum(logb$LENG)>0)
