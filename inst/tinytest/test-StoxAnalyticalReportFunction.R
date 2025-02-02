#
# Test plusgroup function
#

PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
  RstoxFDA::CatchLotterySamplingExample, 
  RstoxFDA::CatchLotteryExample, 
  "serialnumber", "Haul", "MissingAtRandom")
individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
  RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                           individualSamplingParameters, 
                                           c("IndividualTotalLength"), c("IndividualAge", "IndividualSex"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)

plusGr <- RstoxFDA:::makePlusGroupAnalytical(popEst, PlusGroup = 9, AgeDomainVar = "IndividualAge")
comp <- merge(popEst$VariablesCovariance, plusGr$VariablesCovariance, by=c("Stratum", "Domain1", "Domain2", "Variable1", "Variable2"))
expect_true(all(abs(comp$TotalCovariance.x-comp$TotalCovariance.y)/comp$TotalCovariance.y < 1e-3))
expect_true(sum(abs(comp$MeanCovariance.x-comp$MeanCovariance.y)/comp$MeanCovariance.y,na.rm=T) < 1e-3)
expect_equal(sum(is.nan(comp$MeanCovariance.x)), sum(is.nan(comp$MeanCovariance.y)))

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
mwaReportnoPGFull <- RstoxFDA:::ReportAnalyticalWeightAtAge(popEst, Decimals = 0, Unit = "g", IntervalWidth = .9)
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
#check that approximate variance of plusgr is reasonable
expect_true(abs(mwaReportnoPG$MeanWeightByAge$SD[[9]] - mwaReportPG$MeanWeightByAge$SD[[9]])/ mwaReportnoPG$MeanWeightByAge$SD[[9]] < .20)

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight"), c("IndividualAge", "IndividualSex"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
mwaReportPG9 <- RstoxFDA:::ReportAnalyticalWeightAtAge(popEst, PlusGroup = 9, Unit="kg", Decimals = 3, IntervalWidth = .9)
expect_true(RstoxFDA:::is.ReportFdaData(mwaReportPG9))
expect_true("IndividualSex" %in% mwaReportPG9$GroupingVariables$GroupingVariables)


#
# Test report Mean Length At Age
#

PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
  RstoxFDA::CatchLotterySamplingExample, 
  RstoxFDA::CatchLotteryExample, 
  "serialnumber", "Haul", "MissingAtRandom")
individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
  RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                           individualSamplingParameters, 
                                           c("IndividualTotalLength"), c("IndividualAge"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
mwaReportPG <- RstoxFDA:::ReportAnalyticalLengthAtAge(popEst, PlusGroup = 9, Decimals = 2, Unit = "cm", IntervalWidth = .9)
expect_true(RstoxFDA:::is.ReportFdaData(mwaReportPG))

ex <- RstoxFDA::CatchLotteryExample
ex$Individual$IndividualAge[!is.na(ex$Individual$IndividualAge) & ex$Individual$IndividualAge>8] <- 9
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight", "IndividualTotalLength"), c("IndividualAge"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
mwaReportnoPG <- RstoxFDA:::ReportAnalyticalLengthAtAge(popEst, Decimals = 2, Unit = "cm", IntervalWidth = .9)
expect_true(RstoxFDA:::is.ReportFdaData(mwaReportnoPG))
expect_equal(mwaReportnoPG$MeanLengthByAge$MeanIndividualLength[[9]], mwaReportPG$MeanLengthByAge$MeanIndividualLength[[9]])
#check that approximate variance of plusgr is reasonable
expect_true(abs(mwaReportnoPG$MeanLengthByAge$SD[[9]] - mwaReportPG$MeanLengthByAge$SD[[9]])/mwaReportnoPG$MeanLengthByAge$SD[[9]] < .30 )

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(RstoxFDA::CatchLotteryExample, 
                                           individualSamplingParameters, 
                                           c("IndividualTotalLength"), c("IndividualAge", "IndividualSex"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
mwaReportPG9 <- RstoxFDA:::ReportAnalyticalLengthAtAge(popEst, PlusGroup = 9, Unit="mm", Decimals = 3, IntervalWidth = .9)
expect_true(RstoxFDA:::is.ReportFdaData(mwaReportPG9))
expect_true("IndividualSex" %in% mwaReportPG9$GroupingVariables$GroupingVariables)

#
# Test length group reports
#
ex <- RstoxFDA:::AddLengthGroupStoxBiotic(RstoxFDA::CatchLotteryExample, LengthInterval = 5, LengthGroupVariable = "LengthGroup", LeftOpen = FALSE)
PSUsamplingParameters <- RstoxFDA::AssignPSUSamplingParameters(
  RstoxFDA::CatchLotterySamplingExample, 
  RstoxFDA::CatchLotteryExample, 
  "serialnumber", "Haul", "MissingAtRandom")
individualSamplingParameters <-  RstoxFDA:::ComputeIndividualSamplingParameters(
  RstoxFDA::CatchLotteryExample, "SRS", c("IndividualAge"))

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight"), c("LengthGroup"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
cal <- RstoxFDA:::ReportAnalyticalCatchAtLength(popEst, LengthGroupVariable = "LengthGroup", Decimals = 1, IntervalWidth = .9, Unit = "individuals")
expect_true(abs(sum(cal$NbyLength$CatchAtLength) - sum(caaReport$NbyAge$CatchAtAge)) / sum(cal$NbyLength$CatchAtLength) < 1e-5)

#
# test with additional domain variables
#

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, 
                                           individualSamplingParameters, 
                                           c("IndividualRoundWeight"), c("LengthGroup"), c("Gear"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(PSUsamplingParameters, psuEst)
cal <- RstoxFDA:::ReportAnalyticalCatchAtLength(popEst, LengthGroupVariable = "LengthGroup", Decimals = 1, IntervalWidth = .9, Unit = "individuals")
expect_true(abs(sum(cal$NbyLength$CatchAtLength) - sum(caaReport$NbyAge$CatchAtAge)) / sum(cal$NbyLength$CatchAtLength) < 1e-5)
expect_true("Gear" %in% names(cal$NbyLength))
expect_true("Gear" %in% names(cal$GroupingVariables$GroupingVariables))
browser()