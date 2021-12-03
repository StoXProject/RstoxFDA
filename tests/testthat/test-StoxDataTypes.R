context("Test is.Date")
expect_true(is.Date(Sys.Date()))
expect_false(is.Date(as.POSIXct(Sys.Date())))

context("Test is.RecaPrediction")
expect_true(is.RecaPrediction(RstoxFDA::recaPrediction))

context("Test is.RecaResult")
ex<-RstoxFDA::RunRecaEstimate(RstoxFDA::recaDataExample, 100,100)
expect_true(is.RecaResult(ex))