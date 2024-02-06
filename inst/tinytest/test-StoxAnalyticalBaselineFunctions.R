designParamsFile <- system.file("testresources", "lotteryParameters", "lotteryDesignNSH.txt", package="RstoxFDA")

#regular read:
designParams <- RstoxFDA::DefinePSUSamplingParameters(NULL, "ResourceFile", designParamsFile)
expect_true(RstoxFDA:::is.PSUSamplingParametersData(designParams))
expect_equal(nrow(designParams$SelectionTable), 64)
expect_equal(nrow(designParams$SampleTable), 1)
expect_equal(ncol(designParams$StratificationVariables), 1)
expect_equal(nrow(designParams$StratificationVariables), 1)
expect_equal(sum(designParams$SelectionTable$HTsamplingWeight), 1)
expect_equal(sum(designParams$SelectionTable$HHsamplingWeight), 1)

# test assignment to data
expect_error(RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "MissingAtRandom"))
designParamsCorrected <- RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "HaulKey", "MissingAtRandom")
expect_equal(sum(designParamsCorrected$SelectionTable$HTsamplingWeight),1)
expect_equal(sum(designParamsCorrected$SelectionTable$HHsamplingWeight),1)
#HT should be approximately the same after non-response correction
expect_true(abs((sum(1/designParamsCorrected$SelectionTable$InclusionProbability)-sum(1/designParams$SelectionTable$InclusionProbability))/sum(1/designParamsCorrected$SelectionTable$InclusionProbability))<0.1)
#HH should be approximately the same after non-response correction
expect_true(abs((mean(1/designParamsCorrected$SelectionTable$InclusionProbability)-mean(1/designParams$SelectionTable$InclusionProbability))/sum(1/designParamsCorrected$SelectionTable$InclusionProbability))<0.1)

#define from data
suppressWarnings(StoxBioticData <- RstoxData::StoxBiotic(RstoxData::ReadBiotic(system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA"))))
designParamsSB <- RstoxFDA::DefinePSUSamplingParameters(NULL, "AdHocStoxBiotic", StoxBioticData=StoxBioticData, SamplingUnitId = "Individual", StratificationColumns = c("SpeciesCategoryKey"))
expect_true(RstoxFDA:::is.PSUSamplingParametersData(designParamsSB))
#compare names of output with stratification variables to output without

expect_true(all(names(designParamsSB$SampleTable) == names(designParams$SampleTable)))
expect_true(all(names(designParamsSB$SelectionTable) == names(designParams$SelectionTable)))
expect_equal(nrow(designParamsSB$SelectionTable), 75)
expect_equal(nrow(designParamsSB$SampleTable), 2)
expect_equal(ncol(designParamsSB$StratificationVariables), 2)
expect_equal(nrow(designParamsSB$StratificationVariables), 2)

#
# Prepare dataset with sub-sampled parameters
#
ds <- RstoxFDA::StoxBioticDataExample
ds$Individual$IndividualAge[rep(c(TRUE,FALSE), nrow(ds$Individual)/2)] <- as.numeric(NA)
ds$Individual$IndividualRoundWeight[rep(c(TRUE,FALSE), nrow(ds$Individual)/2)] <- as.numeric(NA)
ds$Sample$CatchFractionNumber[is.na(ds$Sample$CatchFractionNumber)] <- 1000

#Define Individual design, SRS
expect_error(DefineIndividualSamplingParameters(NULL, ds, "SRS"))
srs <- DefineIndividualSamplingParameters(NULL, ds, "SRS", c("IndividualAge", "IndividualTotalLength", "IndividualRoundWeight"))
expect_true(RstoxFDA:::is.IndividualSamplingParametersData(srs))
weights <- srs$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))
#Define Individual design, Length stratified
expect_error(DefineIndividualSamplingParameters(NULL, ds, "LengthStratified", c("IndividualAge", "IndividualTotalLength", "IndividualRoundWeight"), LengthInterval = 5), "'IndividualTotalLength' may not be among the variables in 'Parameters' for length-stratified sampling.")
ls<-DefineIndividualSamplingParameters(NULL, ds, "LengthStratified", c("IndividualAge", "IndividualRoundWeight"), LengthInterval = 5)
expect_true(RstoxFDA:::is.IndividualSamplingParametersData(ls))
weights <- ls$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

#Define Individual design, stratified, setting strata by length as in Length stratified
bioStrat <- ds
bioStrat$Individual$LStrat <- as.character(cut(bioStrat$Individual$IndividualTotalLength, seq(0,max(bioStrat$Individual$IndividualTotalLength)+5,5), right = F))
ss<-DefineIndividualSamplingParameters(NULL, bioStrat, "Stratified", c("IndividualAge", "IndividualRoundWeight"), StratificationColumns = c("LStrat"))
expect_true(RstoxFDA:::is.IndividualSamplingParametersData(ss))
weights <- ss$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

#check that length stratified and stratified is consistent.
expect_equal(ss$SelectionTable$InclusionProbability[[4]], ls$SelectionTable$InclusionProbability[[4]])
expect_true(srs$SelectionTable$InclusionProbability[[4]] != ls$SelectionTable$InclusionProbability[[4]])
expect_equal(nrow(ss$SampleTable), nrow(ls$SampleTable))

#test estimate with HansenHurwitzDomainEstimate
data <- RstoxFDA::CatchLotteryExample
indSampling <- RstoxFDA::DefineIndividualSamplingParameters(NULL, data, "SRS", c("IndividualAge"))
psuSampling <- RstoxFDA::CatchLotterySamplingExample

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(data, indSampling, "IndividualRoundWeight")
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(data, indSampling, "IndividualRoundWeight", c("IndividualAge", "IndividualSex"))

browser()
stop("Implement test for AnalyticalPSUEstimate and document.")
stop("Expose collapseStrata and test")
stop("Implement AnalyticalPopulationEstimate")
