context("Test is.Date")
expect_true(is.Date(Sys.Date()))
expect_false(is.Date(as.POSIXct(Sys.Date())))

context("Test is.POSIXct")
expect_false(is.POSIXct(Sys.Date()))
expect_true(is.POSIXct(as.POSIXct(Sys.Date())))


context("Test is.RecaPrediction")
expect_true(is.RecaPrediction(RstoxFDA::recaPrediction))

context("Test is.RecaResult")
ex<-RstoxFDA::RunRecaEstimate(RstoxFDA::recaDataExample, 100,100)
expect_true(is.RecaResult(ex))