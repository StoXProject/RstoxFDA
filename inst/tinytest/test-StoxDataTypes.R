#context("Test is.Date")
expect_true(RstoxFDA:::is.Date(Sys.Date()))
expect_false(RstoxFDA:::is.Date(as.POSIXct(Sys.Date())))

#context("Test is.POSIXct")
expect_false(RstoxFDA:::is.POSIXct(Sys.Date()))
expect_true(RstoxFDA:::is.POSIXct(as.POSIXct(Sys.Date())))


#context("Test is.RecaPrediction")
expect_true(RstoxFDA:::is.RecaPrediction(RstoxFDA::recaPrediction))

#context("Test is.RecaResult")
ex<-RstoxFDA::RunRecaEstimate(RstoxFDA::recaDataExample, 100,100)
expect_true(RstoxFDA:::is.RecaResult(ex))
