PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
                                      RstoxFDA::CatchLotterySamplingExample, 
                                      RstoxFDA::CatchLotteryExample, 
                                      "serialnumber", "Haul", "MissingAtRandom")
individualSamplingParameters <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, 
                                      RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))
                                      
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                      individualSamplingParameters, 
                                      c("IndividualRoundWeight"), c("IndividualAge"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)

caaReportPG <- RstoxFDA:::ReportAnalyticalCatchAtAge(popEst, PlusGroup = 9)
RstoxFDA:::is.ReportFdaData(caaReportPG)

caaReport <- RstoxFDA:::ReportAnalyticalCatchAtAge(popEst)

#check that report is ordered by age
expect_true(all(order(caaReport$NbyAge$Age) == 1:max(caaReport$NbyAge$Age)))
RstoxFDA:::is.ReportFdaData(caaReport)

diff <- sum(caaReportPG$NbyAge$CatchAtAge) - sum(caaReportPG$NbyAge$CatchAtAge)
expect_true(abs(diff) < 1e-6)