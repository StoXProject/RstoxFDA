#
# Test report Catch At Age
#

PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
                                      RstoxFDA::CatchLotterySamplingExample, 
                                      RstoxFDA::CatchLotteryExample, 
                                      "serialnumber", "Haul", "MissingAtRandom")
individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
                                      RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))
                                      
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                      individualSamplingParameters, 
                                      c("IndividualRoundWeight"), c("IndividualAge"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)

caaReportPG <- RstoxFDA:::ReportAnalyticalCatchAtAge(popEst, PlusGroup = 9, Decimals = 0, Unit = "individuals", IntervalWidth = .9)
RstoxFDA:::is.ReportFdaData(caaReportPG)

caaReport <- RstoxFDA:::ReportAnalyticalCatchAtAge(popEst, Decimals = 0, Unit = "individuals", IntervalWidth = .9)

#check that report is ordered by age
expect_true(all(order(caaReport$NbyAge$Age) == 1:max(caaReport$NbyAge$Age)))
RstoxFDA:::is.ReportFdaData(caaReport)

diff <- sum(caaReportPG$NbyAge$CatchAtAge) - sum(caaReportPG$NbyAge$CatchAtAge)
expect_true(abs(diff) < 1e-6)


# test without IndividualAge as variable
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight"), c("IndividualSex"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
expect_error(RstoxFDA:::ReportAnalyticalCatchAtAge(popEst, Decimals = 0, Unit = "individuals", IntervalWidth = .9), "Catch-at-age reporting, requires the StoxBiotic variable 'IndividualAge' to be among the domain variables of 'AnalyticalPopulationEstimateData'.")

#
# Test report Mean Weight At Age
#

PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
  RstoxFDA::CatchLotterySamplingExample, 
  RstoxFDA::CatchLotteryExample, 
  "serialnumber", "Haul", "MissingAtRandom")
individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
  RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight"), c("IndividualAge"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
mwaReportPG <- RstoxFDA:::ReportAnalyticalWeightAtAge(popEst, PlusGroup = 9, Decimals = 0, Unit = "g", IntervalWidth = .9)
expect_true(RstoxFDA:::is.ReportFdaData(mwaReportPG))

ex <- RstoxFDA::CatchLotteryExample
ex$Individual$IndividualAge[!is.na(ex$Individual$IndividualAge) & ex$Individual$IndividualAge>8] <- 9
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight", "IndividualTotalLength"), c("IndividualAge"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
mwaReportnoPG <- RstoxFDA:::ReportAnalyticalWeightAtAge(popEst, Decimals = 0, Unit = "g", IntervalWidth = .9)
expect_true(RstoxFDA:::is.ReportFdaData(mwaReportnoPG))
expect_equal(mwaReportnoPG$MeanWeightByAge$MeanIndividualWeight[[9]], mwaReportPG$MeanWeightByAge$MeanIndividualWeight[[9]])
expect_true(mwaReportnoPG$MeanWeightByAge$SD[[9]] <= mwaReportPG$MeanWeightByAge$SD[[9]])

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight"), c("IndividualAge", "IndividualSex"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
mwaReportPG9 <- RstoxFDA:::ReportAnalyticalWeightAtAge(popEst, PlusGroup = 9, Unit="kg", Decimals = 3, IntervalWidth = .9)
expect_true(RstoxFDA:::is.ReportFdaData(mwaReportPG9))
expect_true("IndividualSex" %in% mwaReportPG9$GroupingVariables$GroupingVariables)

