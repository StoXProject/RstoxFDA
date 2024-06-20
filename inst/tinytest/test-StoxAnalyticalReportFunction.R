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
RstoxFDA::PlotCatchAtAgeTotals(caaReportPG)
caaReport <- RstoxFDA:::ReportAnalyticalCatchAtAge(popEst)

diff <- sum(caaReportPG$NbyAge$CatchAtAge) - sum(caaReportPG$NbyAge$CatchAtAge)
expect_true(abs(diff) < 1e-6)