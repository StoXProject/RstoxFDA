fdiropen <- system.file("testresources", "landingsvariants", "openfdir.2021.csv", package="RstoxFDA")
read <- RstoxFDA:::readFdirOpenLandings(fdiropen)
expect_equal(nrow(read), 9)

lss <- RstoxFDA:::convertToLssData(openFdirData=read)
landingdata <- RstoxData::convertToLandingData(lss)
expect_true(RstoxData::is.LandingData(landingdata))
expect_equal(nrow(landingdata$ConvertedData$Salgslagdata), 9)