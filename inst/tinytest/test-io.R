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

erslogb <- convertToErsData(logb)
expect_true(all(!is.na(erslogb$STARTTIDSPUNKT)))
expect_true(all(erslogb$REGM %in% logb$REGM))
expect_true(all(erslogb$RC %in% logb$RKAL))
expect_equal(sum(erslogb$RUNDVEKT), sum(logb$VEKT))
expect_equal(sum(erslogb$VARIGHET), sum(logb$VAR)*60)
expect_true(all(paste(logb$HO, logb$LO, sep="") %in% erslogb$LOKASJON_START))
expect_equal(nrow(logb), nrow(erslogb))

#test with NAs for HO or LO
logb$HO[1] <- NA
logb$LO[2] <- NA
logb$HO[3] <- NA
logb$LO[3] <- NA
logbwNA <- convertToErsData(logb)
expect_true(all(is.na(logbwNA$LOKASJON_START[1:3])))

#test variant wo header
lst_logb_woh <- system.file("testresources", "logbookvariants", "logbook_wo_header.lst", package="RstoxFDA")
expect_warning(logb <- RstoxFDA:::readLstFile(lst_logb_woh))
expect_equal(ncol(logb), 27)
expect_true(sum(logb$VAR)>0)
expect_true(sum(logb$VEKT)>0)
expect_true(sum(logb$LENG)>0)
