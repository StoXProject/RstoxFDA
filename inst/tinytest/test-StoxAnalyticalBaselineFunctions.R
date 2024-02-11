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
expect_error(RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "MissingAtRandom"), "argument \"DataRecordId\" is missing, with no default")
expect_error(RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "Haul", "Sample", "MissingAtRandom"), "The column provided for 'DataRecordId' ")
expect_error(RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "HaulKey", "Haul", "MissingAtRandom"), "The 'SamplingUnitId' ")
ex <- RstoxFDA::CatchLotteryExample
ex$Haul$serialnumber <- RstoxFDA::CatchLotteryExample$Haul$HaulKey
designParamsCorrected <- RstoxFDA::AssignPSUSamplingParameters(designParams, ex, "serialnumber", "Haul", "MissingAtRandom")
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
#compare names of output with stratification variables to output withoutsss

expect_true(all(!is.na(designParamsSB$SelectionTable$HHsamplingWeight)))
expect_true(all(designParamsSB$SampleTable$SelectionMethod=="FSWR"))
#some check that method and incprob is reasonable.

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
expect_error(RstoxFDA:::DefineIndividualSamplingParameters(NULL, ds, "SRS"))
srs <- RstoxFDA:::DefineIndividualSamplingParameters(NULL, ds, "SRS", c("IndividualAge", "IndividualTotalLength", "IndividualRoundWeight"))
expect_true(RstoxFDA:::is.IndividualSamplingParametersData(srs))
weights <- srs$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

#Define Individual design, Length stratified
expect_error(RstoxFDA:::DefineIndividualSamplingParameters(NULL, ds, "LengthStratified", c("IndividualAge", "IndividualTotalLength", "IndividualRoundWeight"), LengthInterval = 5), "'IndividualTotalLength' may not be among the variables in 'Parameters' for length-stratified sampling.")
ls<-RstoxFDA:::DefineIndividualSamplingParameters(NULL, ds, "LengthStratified", c("IndividualAge", "IndividualRoundWeight"), LengthInterval = 5)

expect_true(RstoxFDA:::is.IndividualSamplingParametersData(ls))
weights <- ls$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

expect_error(RstoxFDA:::collapseStrataIndividualDesignParamaters(ls, "LengthStratum"), "Cannot collapse unsampled strata. Unsampled strata exists for samples:")

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

#
# Test collapse strata
#

ss <- ds
ss$Individual$IndividualSex[is.na(ss$Individual$IndividualSex)] <- "Unkown"
ls <- RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "Stratified", c("IndividualTotalLength"), StratificationColumns = "IndividualSex")
weights <- ls$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))


lsCol <- RstoxFDA:::collapseStrataIndividualDesignParamaters(ls, c("SpeciesCategory", "IndividualSex"))
expect_true(nrow(lsCol$SampleTable)<nrow(ls$SampleTable))
expect_true(nrow(lsCol$SelectionTable)==nrow(ls$SelectionTable))
weights <- lsCol$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

lsCol <- RstoxFDA:::collapseStrataIndividualDesignParamaters(ls, c("IndividualSex"))
weights <- lsCol$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))


#test estimate with HansenHurwitzDomainEstimate
data <- RstoxFDA::CatchLotteryExample
indSampling <- RstoxFDA::DefineIndividualSamplingParameters(NULL, data, "SRS", c("IndividualAge"))
psuSampling <- RstoxFDA::CatchLotterySamplingExample

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(data, indSampling, "IndividualRoundWeight")
expect_true(all(psuEst$Abundance$Abundance*psuEst$Variables$Mean - psuEst$Variables$Total < 1e-6))
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(data, indSampling, "IndividualRoundWeight", c("IndividualAge", "IndividualSex"))
expect_true(all(psuEst$Abundance$Abundance*psuEst$Variables$Mean - psuEst$Variables$Total < 1e-6))

#
# Test domain estimates
#

ss <- ds
ss$Individual$IndividualSex[is.na(ss$Individual$IndividualSex)] <- "Unkown"
ls <- RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "SRS", c("IndividualTotalLength"))

psuEstDom <- RstoxFDA:::AnalyticalPSUEstimate(ss, ls, "IndividualTotalLength", c("IndividualSex", "IndividualAge"))
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, ls, "IndividualTotalLength")
expect_true(nrow(psuEstDom$Abundance)>nrow(psuEst$Abundance))
expect_true(nrow(psuEstDom$Variables)>nrow(psuEst$Variables))
expect_true(ncol(psuEstDom$DomainVariables)==3)

expect_true(all(psuEst$Abundance$Frequency-1<1e-6))
expect_true(mean(psuEstDom$Abundance$Frequency)>1-2)

totalBySampleDom <- psuEstDom$Abundance[,list(total=sum(Abundance)), by="SampleId"]
totalBySampleTot <- psuEst$Abundance[,list(total=sum(Abundance)), by="SampleId"]
tot <- merge(totalBySampleDom, totalBySampleTot, by="SampleId")
expect_true(all(abs(tot$total.x-tot$total.y)/tot$total.x<1e-6))

totalLengthBySampleDom <- psuEstDom$Variables[Variable=="IndividualTotalLength",list(total=sum(Total)), by="SampleId"]
totalLengthBySampleTot <- psuEst$Variables[Variable=="IndividualTotalLength",list(total=sum(Total)), by="SampleId"]
tot <- merge(totalLengthBySampleDom, totalLengthBySampleTot, by="SampleId")
expect_true(all(abs(tot$total.x-tot$total.y)/tot$total.x<1e-6))

#
# Test stratified estimates
#

ls <- RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "Stratified", c("IndividualTotalLength"), StratificationColumns = "IndividualSex")
lsCol <- RstoxFDA:::collapseStrataIndividualDesignParamaters(ls, c("SpeciesCategory", "IndividualSex"))

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, lsCol, "IndividualTotalLength", c("IndividualSex"))
expect_true(nrow(psuEst$DomainVariables)==3)
expect_true(ncol(psuEst$DomainVariables)==2)
expect_true(sum(is.na(psuEst$Abundance$Abundance))==0)
expect_true(sum(!is.na(psuEst$Abundance$Abundance))>0)
expect_true(sum(is.na(psuEst$Abundance$Frequency))==0)
expect_true(sum(!is.na(psuEst$Abundance$Frequency))>0)
expect_true(sum(is.na(psuEst$Variables$Total))==0)
expect_true(sum(!is.na(psuEst$Variables$Mean))>0)

#check that unsampled strata gives NAs
lengthStratMissingStrata <-  RstoxFDA::DefineIndividualSamplingParameters(NULL, ss, "LengthStratified", c("IndividualAge"), LengthInterval = 5)
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, lengthStratMissingStrata, "IndividualRoundWeight", c("IndividualAge"))

expect_true(nrow(psuEst$DomainVariables)==length(unique(ss$Individual$IndividualAge)))
expect_true("NA" %in% psuEst$DomainVariables$Domain)
expect_true(sum(is.na(psuEst$DomainVariables$IndividualAge))==1)
expect_true(ncol(psuEst$DomainVariables)==2)

expect_true(sum(is.na(psuEst$Abundance$Abundance))>0)
expect_true(sum(!is.na(psuEst$Abundance$Abundance))>0)
expect_true(sum(is.na(psuEst$Abundance$Frequency))>0)
expect_true(sum(!is.na(psuEst$Abundance$Frequency))>0)
expect_true(sum(is.na(psuEst$Variables$Total))>0)
expect_true(sum(!is.na(psuEst$Variables$Mean))>0)

#
# Test AnalyticalPopulationEstimate
#

stationDesign <- RstoxFDA:::DefinePSUSamplingParameters(NULL, "AdHocStoxBiotic", StoxBioticData = ss, SamplingUnitId = "Haul", StratificationColumns = "Gear")
sexStrat <-  RstoxFDA::DefineIndividualSamplingParameters(NULL, ss, "Stratified", c("IndividualAge"), StratificationColumns = "IndividualSex")
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, sexStrat, "IndividualRoundWeight", c("IndividualSex"))

popEst <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst)
popEstMeanOfMeans <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst, MeanOfMeans = T)

#Test that Abundance and Frequency are NA for unsampled strata (Domain Sex is Unsampled for strata unkown sex)
expect_true(nrow(popEst$Abundance[is.na(Domain)])>0)
expect_true(all(is.na(popEst$Abundance[is.na(Domain)]$Abundance)))
expect_true(all(is.na(popEst$Abundance[is.na(Domain)]$Frequency)))

#Test that Mean and Total NA for unsampled strata
expect_true(nrow(popEst$Variables[is.na(Domain)])>0)
expect_true(all(is.na(popEst$Variables[is.na(Domain)]$Total)))
expect_true(all(is.na(popEst$Variables[is.na(Domain)]$Mean)))

#Test that Mean is NaN and Total is 0 for zero-abundance domains
zeroAbund <- popEstMeanOfMeans$Abundance[popEstMeanOfMeans$Abundance$Frequency==0]
NaNMeans <- popEstMeanOfMeans$Variables[is.nan(popEstMeanOfMeans$Variables$Mean)]
zeroTotals <- popEstMeanOfMeans$Variables[popEstMeanOfMeans$Variables$Total==0]
expect_true(nrow(zeroAbund)>0)
expect_true(nrow(zeroAbund)==nrow(NaNMeans))


# Add Correctness test for minimal example
# Add variances

stop()
#build herring example here. Need to connect SampleUnitId (Haul) to SampleId (Sample)
stationDesign <- RstoxFDA::CatchLotterySamplingExample
ex <- RstoxFDA::CatchLotteryExample
ex$Haul$serialnumber <- ex$Haul$HaulKey
stationDesign <- RstoxFDA::AssignPSUSamplingParameters(stationDesign, ex, "serialnumber", "Haul", "MissingAtRandom")
srs <-  RstoxFDA::DefineIndividualSamplingParameters(NULL, ex, "SRS", c("IndividualAge"))
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight", "IndividualTotalLength"), c("IndividualAge"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst)
popEstMeanOfMeans <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst, MeanOfMeans = T)

browser()
#stop("Fix HH in PSU designs")
#stop("Document AnalyticalPSUEstimate.")
#stop("Expose collapseStrata and test")
#stop("Test collapseStrata with both HH and HT")
#stop("Document AnalyticalPopulationEstimate")
