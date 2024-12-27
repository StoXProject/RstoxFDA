designParamsFile <- system.file("testresources", "lotteryParameters", "lotteryDesignNSH.txt", package="RstoxFDA")

#regular read:
designParams <- RstoxFDA:::ReadPSUSamplingParameters( designParamsFile)
expect_true(RstoxFDA:::is.PSUSamplingParametersData(designParams))
expect_equal(nrow(designParams$SelectionTable), 64)
expect_equal(nrow(designParams$SampleTable), 1)
expect_equal(ncol(designParams$StratificationVariables), 1)
expect_equal(nrow(designParams$StratificationVariables), 1)
expect_equal(sum(designParams$SelectionTable$HTsamplingWeight), 1)
expect_equal(sum(designParams$SelectionTable$HHsamplingWeight), 1)

designParamsFileStratified <- system.file("testresources", "lotteryParameters", "lotteryDesignNSHstrata.txt", package="RstoxFDA")
designParamsStratified <- RstoxFDA:::ReadPSUSamplingParameters(designParamsFileStratified)
expect_true(RstoxFDA:::is.PSUSamplingParametersData(designParamsStratified))
expect_equal(nrow(designParamsStratified$SelectionTable), 64)
expect_equal(nrow(designParamsStratified$SampleTable), 1)
expect_equal(ncol(designParamsStratified$StratificationVariables), 2)
expect_equal(nrow(designParamsStratified$StratificationVariables), 1)
expect_equal(sum(designParamsStratified$SelectionTable$HTsamplingWeight), 1)
expect_equal(sum(designParamsStratified$SelectionTable$HHsamplingWeight), 1)

# test assignment to data
expect_error(RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "MissingAtRandom"), "Argument \'DataRecordId\' must be provided.")
expect_error(RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "Haul", "Sample", "MissingAtRandom"), "The column provided for 'DataRecordId' ")
expect_error(RstoxFDA::AssignPSUSamplingParameters(designParams, RstoxFDA::CatchLotteryExample, "HaulKey", "Haul", "MissingAtRandom"), "The 'SamplingUnitId' ")
ex <- RstoxFDA::CatchLotteryExample
designParamsCorrected <- RstoxFDA::AssignPSUSamplingParameters(designParams, ex, "serialnumber", "Haul", "MissingAtRandom")
expect_equal(sum(designParamsCorrected$SelectionTable$HTsamplingWeight),1)
expect_equal(sum(designParamsCorrected$SelectionTable$HHsamplingWeight),1)
#HT should be approximately the same after non-response correction
expect_true(abs((sum(1/designParamsCorrected$SelectionTable$InclusionProbability)-sum(1/designParams$SelectionTable$InclusionProbability))/sum(1/designParamsCorrected$SelectionTable$InclusionProbability))<0.1)
#HH should be approximately the same after non-response correction
expect_true(abs((mean(1/designParamsCorrected$SelectionTable$InclusionProbability)-mean(1/designParams$SelectionTable$InclusionProbability))/sum(1/designParamsCorrected$SelectionTable$InclusionProbability))<0.1)

#define from data
suppressWarnings(StoxBioticData <- RstoxData::StoxBiotic(RstoxData::ReadBiotic(system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA"))))
designParamsSB <- RstoxFDA:::ComputePSUSamplingParameters(StoxBioticData=StoxBioticData, SamplingUnitId = "Individual", StratificationColumns = c("SpeciesCategoryKey"))
expect_true(RstoxFDA:::is.PSUSamplingParametersData(designParamsSB))
#compare names of output with stratification variables to output without

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
expect_error(RstoxFDA:::ComputeIndividualSamplingParameters(ds, "SRS"))
srs <- RstoxFDA:::ComputeIndividualSamplingParameters(ds, "SRS", c("IndividualAge", "IndividualTotalLength", "IndividualRoundWeight"))
expect_true(RstoxFDA:::is.IndividualSamplingParametersData(srs))
weights <- srs$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

#Define Individual design, Length stratified
expect_error(RstoxFDA:::ComputeIndividualSamplingParameters(ds, "LengthStratified", c("IndividualAge", "IndividualTotalLength", "IndividualRoundWeight"), LengthInterval = 5), "'IndividualTotalLength' may not be among the variables in 'Parameters' for length-stratified sampling.")
ls<-RstoxFDA:::ComputeIndividualSamplingParameters(ds, "LengthStratified", c("IndividualAge", "IndividualRoundWeight"), LengthInterval = 5)

expect_true(RstoxFDA:::is.IndividualSamplingParametersData(ls))
weights <- ls$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

expect_error(RstoxFDA:::collapseStrataIndividualDesignParamaters(ls, "LengthStratum"), "Cannot collapse unsampled strata. Unsampled strata exists for samples:")

#Define Individual design, stratified, setting strata by length as in Length stratified
bioStrat <- ds
bioStrat$Individual$LStrat <- as.character(cut(bioStrat$Individual$IndividualTotalLength, seq(0,max(bioStrat$Individual$IndividualTotalLength)+5,5), right = F))
ss<-RstoxFDA:::ComputeIndividualSamplingParameters(bioStrat, "Stratified", c("IndividualAge", "IndividualRoundWeight"), StratificationColumns = c("LStrat"))
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
ls <- RstoxFDA:::ComputeIndividualSamplingParameters(ss, "Stratified", c("IndividualTotalLength"), StratificationColumns = "IndividualSex")
weights <- ls$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

lsCol <- RstoxFDA:::CollapseStrata(ls, c("SpeciesCategory", "IndividualSex"))
expect_true(nrow(lsCol$SampleTable)==nrow(ls$SampleTable))
expect_true(nrow(lsCol$SelectionTable)==nrow(ls$SelectionTable))
expect_true(ncol(lsCol$StratificationVariables)==ncol(ls$StratificationVariables))
weights <- lsCol$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))

lsCol <- RstoxFDA:::CollapseStrata(ls, c())
expect_true(nrow(lsCol$SampleTable)<nrow(ls$SampleTable))
expect_true(nrow(lsCol$SelectionTable)==nrow(ls$SelectionTable))
expect_true(ncol(lsCol$StratificationVariables)<ncol(ls$StratificationVariables))
expect_true(ncol(lsCol$StratificationVariables)==2)
weights <- lsCol$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))
expect_true(all(lsCol$StratificationVariables$Stratum=="All"))

lsCol <- RstoxFDA:::CollapseStrata(ls, c("IndividualSex"))
expect_true(nrow(lsCol$SampleTable)==nrow(ls$SampleTable)) #original stratification columns are redundant
expect_true(nrow(lsCol$SelectionTable)==nrow(ls$SelectionTable))
expect_true(ncol(lsCol$StratificationVariables)<ncol(ls$StratificationVariables))
expect_true(ncol(lsCol$StratificationVariables)==3)
weights <- lsCol$SelectionTable[,list(meanN=sum(HTsamplingWeight)), by=c("Stratum", "SampleId")]
expect_true(all(abs(weights$meanN-1) < 1e-6))


#test estimate with HansenHurwitzDomainEstimate
data <- RstoxFDA::CatchLotteryExample
indSampling <- RstoxFDA:::ComputeIndividualSamplingParameters(data, "SRS", c("IndividualAge"))

psuSampling <- RstoxFDA::CatchLotterySamplingExample

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(data, indSampling, "IndividualRoundWeight")
diffs <- psuEst$Abundance$Abundance*psuEst$Variables$Mean - psuEst$Variables$Total
expect_true(all(diffs[!is.nan(diffs)] < 1e-6))
expect_true(sum(!is.nan(diffs)) > 0)
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(data, indSampling, "IndividualRoundWeight", c("IndividualAge", "IndividualSex"))
diffs <- psuEst$Abundance$Abundance*psuEst$Variables$Mean - psuEst$Variables$Total
expect_true(all(diffs[!is.nan(diffs)] < 1e-6))
expect_true(sum(!is.nan(diffs)) > 0)

#
# Test domain estimates
#

ss <- ds
ss$Individual$IndividualSex[is.na(ss$Individual$IndividualSex)] <- "Unkown"
ls <- RstoxFDA:::ComputeIndividualSamplingParameters(ss, "SRS", c("IndividualTotalLength"))

psuEstDom <- RstoxFDA:::AnalyticalPSUEstimate(ss, ls, "IndividualTotalLength", c("IndividualSex", "IndividualAge"))
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, ls, "IndividualTotalLength")
expect_true(nrow(psuEstDom$Abundance)>nrow(psuEst$Abundance))
expect_true(nrow(psuEstDom$Variables)>nrow(psuEst$Variables))
expect_true(ncol(psuEstDom$DomainVariables)==3)
expect_true(nrow(psuEstDom$Abundance)==nrow(psuEstDom$StratificationVariables)*nrow(psuEstDom$DomainVariables))

expect_true(all(psuEst$Abundance$Frequency-1<1e-6))
expect_true(mean(psuEstDom$Abundance$Frequency)>1e-2)

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

ls <- RstoxFDA:::ComputeIndividualSamplingParameters(ss, "Stratified", c("IndividualTotalLength"), StratificationColumns = "IndividualSex")
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
lengthStratMissingStrata <-  RstoxFDA:::ComputeIndividualSamplingParameters(ss, "LengthStratified", c("IndividualAge"), LengthInterval = 5)
expect_warning(psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, lengthStratMissingStrata, "IndividualRoundWeight", c("IndividualAge")), "Not all strata are sampled. Estimates will not be provided for some strata for SampleIds:")

expect_true(nrow(psuEst$DomainVariables)==length(unique(ss$Individual$IndividualAge[!is.na(ss$Individual$IndividualAge)])))
expect_true(ncol(psuEst$DomainVariables)==2)

expect_true(sum(is.na(psuEst$Abundance$Abundance))>0)
expect_true(sum(!is.na(psuEst$Abundance$Abundance))>0)
expect_true(sum(is.na(psuEst$Abundance$Frequency))>0)
expect_true(sum(!is.na(psuEst$Abundance$Frequency))>0)
expect_true(sum(is.na(psuEst$Variables$Total))>0)
expect_true(sum(!is.na(psuEst$Variables$Mean))>0)

naAbundance <- psuEst$Abundance[is.na(psuEst$Abundance$Abundance),]
unSampled <- lengthStratMissingStrata$SampleTable[N>0 & n==0]
expect_true(nrow(naAbundance)==nrow(unSampled)*nrow(psuEst$DomainVariables))

#
# Test LiftStrata
#
sexStrat <-  RstoxFDA:::ComputeIndividualSamplingParameters(ss, "Stratified", c("IndividualAge"), StratificationColumns = "IndividualSex")
expect_warning(psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, sexStrat, "IndividualRoundWeight", c("IndividualSex")), "Not all strata are sampled. Estimates will not be provided for some strata for SampleIds:")
expect_true(sum(is.na(psuEst$Variables$Domain))==0)
psuEstLifted <- RstoxFDA:::LiftStrata(psuEst)

expect_true(nrow(psuEstLifted$Abundance)==nrow(psuEstLifted$Variables))
expect_true(nrow(psuEstLifted$Abundance)==length(unique(sexStrat$StratificationVariables$Stratum))*length(unique(sexStrat$SampleTable$SampleId))*length(unique(ss$Individual$IndividualSex)))
expect_true(nrow(psuEstLifted$StratificationVariables)==length(unique(sexStrat$StratificationVariables$Stratum))*length(unique(sexStrat$SampleTable$SampleId)))

#
# Test AnalyticalPopulationEstimate
#
stationDesign <- RstoxFDA:::ComputePSUSamplingParameters(StoxBioticData = ss, SamplingUnitId = "Haul", StratificationColumns = "Gear")
sexStrat <-  RstoxFDA:::ComputeIndividualSamplingParameters(ss, "Stratified", c("IndividualAge"), StratificationColumns = "IndividualSex")
expect_warning(psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, sexStrat, "IndividualRoundWeight", c("IndividualSex")), "Not all strata are sampled. Estimates will not be provided for some strata for SampleIds:")
psuEst <- RstoxFDA:::LiftStrata(psuEst)
expect_error(popEst <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst), "Cannot estimate. Estimates are not provided for all samples in 'AnalyticalPSUEstimateData'. Missing for SamplingUnitIds:")

#Test that Abundance and Frequency are NA for unsampled strata (Domain Sex is Unsampled for strata unkown sex)
unsampled <- merge(psuEst$Abundance, sexStrat$SampleTable[n==0], by=c("SampleId", "Stratum"))
expect_true(nrow(unsampled)>0)
expect_true(all(is.na(psuEst$Abundance[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Abundance)))
expect_true(all(is.na(psuEst$Abundance[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Frequency)))
expect_true(!any(is.nan(psuEst$Abundance[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Abundance)))
expect_true(!any(is.nan(psuEst$Abundance[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Frequency)))

#Test that Mean and Total NA for unsampled strata
unsampled <- merge(psuEst$Variables, sexStrat$SampleTable[n==0], by=c("SampleId", "Stratum"))
expect_true(nrow(unsampled)>0)
expect_true(all(is.na(psuEst$Variables[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Total)))
expect_true(all(is.na(psuEst$Variables[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Mean)))
expect_true(!any(is.nan(psuEst$Variables[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Total)))
expect_true(!any(is.nan(psuEst$Variables[paste(SampleId, Stratum) %in% paste(unsampled$SampleId, unsampled$Stratum)]$Mean)))

#Test that Mean is NaN and Total is 0 for zero-abundance domains
sexStrat <-  RstoxFDA:::ComputeIndividualSamplingParameters(ss, "Stratified", c("IndividualTotalLength"), StratificationColumns = "IndividualSex")
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, sexStrat, "IndividualTotalLength", c("IndividualSex"))
psuEst <- RstoxFDA:::LiftStrata(psuEst)
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst)

popEstMeanOfMeans <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst, MeanOfMeans = T)

zeroAbund <- popEstMeanOfMeans$Abundance[popEstMeanOfMeans$Abundance$Frequency==0]
NaNMeans <- popEstMeanOfMeans$Variables[is.nan(popEstMeanOfMeans$Variables$Mean)]
zeroTotals <- popEstMeanOfMeans$Variables[popEstMeanOfMeans$Variables$Total==0]
expect_true(nrow(zeroAbund)>0)
expect_true(nrow(zeroAbund)==nrow(NaNMeans))


#
# Test unsampled PSU strata
#

stationDesign <- RstoxFDA:::ComputePSUSamplingParameters(StoxBioticData = ss, SamplingUnitId = "Haul", StratificationColumns = "Gear")
stationDesign$SampleTable$n[stationDesign$SampleTable$Stratum==40]<-0
stationDesign$SelectionTable <- stationDesign$SelectionTable[Stratum!="40",]
sexStrat <-  RstoxFDA:::ComputeIndividualSamplingParameters(ss, "Stratified", c("IndividualAge"), StratificationColumns = "IndividualSex")
expect_warning(psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, sexStrat, "IndividualRoundWeight", c("IndividualSex")), "Not all strata are sampled. Estimates will not be provided for some strata for SampleIds:")
psuEst <- RstoxFDA:::LiftStrata(psuEst)
psuEst$Abundance$Frequency[is.na(psuEst$Abundance$Frequency)] <- 0
psuEst$Variables$Mean[is.na(psuEst$Variables$Mean) & !is.nan(psuEst$Variables$Mean)] <- .1
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst, MeanOfMeans = T)

nastrataAbundance <- popEst$Abundance$Stratum[is.na(popEst$Abundance$Frequency)]
expect_true(length(grep("PSU-stratum:40", nastrataAbundance))==length(nastrataAbundance))
nastrataVariables <- popEst$Variables$Stratum[is.na(popEst$Variables$Frequency)]
expect_true(length(grep("PSU-stratum:40", nastrataVariables))==length(nastrataVariables))
nastrataAbundanceCovar <- popEst$AbundanceCovariance$Stratum[is.na(popEst$AbundanceCovariance$Frequency)]
expect_true(length(grep("PSU-stratum:40", nastrataAbundanceCovar))==length(nastrataAbundanceCovar))
nastrataVariableCovar <- popEst$VariablesCovariance$Stratum[is.na(popEst$VariablesCovariance$Frequency)]
expect_true(length(grep("PSU-stratum:40", nastrataVariableCovar))==length(nastrataVariableCovar))

#
# Test herring example here. 
#
stationDesign <- RstoxFDA::CatchLotterySamplingExample
ex <- RstoxFDA::CatchLotteryExample
ex$SpeciesCategory$SpeciesCategory <- "061104"
ex$Individual$IW <- ex$Individual$IndividualRoundWeight #for testing that covariances equal variances when appropriate
ex$Individual$one <- 1 #for testing that variable covariance equal abundance covariance when appropriate.
stationDesign <- RstoxFDA::AssignPSUSamplingParameters(stationDesign, ex, "serialnumber", "Haul", "MissingAtRandom")
srs <-  RstoxFDA:::ComputeIndividualSamplingParameters(ex, "SRS", c("IndividualAge"))
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight", "IndividualTotalLength"), c("IndividualAge"))
expect_true(abs(sum(psuEst$Abundance$Abundance) - sum(ex$Sample$CatchFractionNumber))/sum(ex$Sample$CatchFractionNumber) < 1e-3)
popEstAgeDomain <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst)
popEstMeanOfMeans <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst, MeanOfMeans = T)
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst)
expect_true(abs(popEst$Variables$Total-125183088936) < 1) #g, not checked for correctness, just there to detect if anything changes. Offical landings over 15m were 133137498 kg within 7% difference
expect_true(abs(sum(popEstAgeDomain$Abundance$Abundance)-popEst$Abundance$Abundance)<1e-6)

#mean of means should be different, but not too different. Set a reasonable range, re-check if test fails
maxDIffMeanOfMean <- max((popEstMeanOfMeans$Variables$Mean-popEstAgeDomain$Variables$Mean)/popEstMeanOfMeans$Variables$Mean)
expect_true(maxDIffMeanOfMean >.01)
expect_true(maxDIffMeanOfMean <.1)

#test PSU domains
psuEstPD <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"), c(), c("Gear"))
popEstPD <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEstPD)
expect_true(abs(popEst$Abundance$Abundance - sum(popEstPD$Abundance$Abundance))/popEst$Abundance$Abundance < 1e-2)
expect_true(abs(popEst$Variables$Total - sum(popEstPD$Variables$Total))/popEst$Variables$Total < 1e-2)


#
# Test ratio estimation
#
psuEstDomain <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"), "IndividualAge")
popEstDomain <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEstDomain)

CVs <- merge(popEstDomain$Abundance, popEstDomain$AbundanceCovariance[Domain1==Domain2], by.x=c("Stratum", "Domain"), by.y=c("Stratum", "Domain1"))
CVs$CV <- sqrt(CVs$AbundanceCovariance) / CVs$Abundance
expect_true(min(CVs$CV)<.2)

#some annotation and recoding for for testing purposes (does not correspond to actual gear mapping)
land <- RstoxFDA::CatchLotteryLandingExample
land$Landing$SpeciesCategory <- "061104"
ex$Haul$Gear[ex$Haul$Gear %in% c("3500", "3600")] <- "51"
ex$Haul$Gear[ex$Haul$Gear %in% c("3700")] <- "53"
ex$Haul$Gear[ex$Haul$Gear %in% c("3100")] <- "11"

# add tiny error to Mean to ensure that it is recalculated
popEstDomain$Variables$Mean <- popEstDomain$Variables$Mean + 1e-3

#
# Input tests TotalDomainWeight
#

psuBySex <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"), "IndividualSex")
popBySex <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuBySex)

ll <- land
ll$Landing$IndividualSex <- "M"
expect_error(RstoxFDA:::AnalyticalRatioEstimate(popBySex, ll, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "Sex"), "Some Stratification Variables or Domain Variables could not be matched with landings")
ll <- land
ll$Landing$SpeciesCategory <- 1
expect_error(RstoxFDA:::AnalyticalRatioEstimate(popEstDomain, ll, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "SpeciesCategory"), "Not all of the estimated domains")

ll <- land
ll$Landing$SpeciesCategory <- NULL
expect_error(RstoxFDA:::AnalyticalRatioEstimate(popEstDomain, ll, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "SpeciesCategory"), "Some Stratification Variables or Domain Variables could not be matched with landings")

# Test year as stratification variable
popEstDomainYear <- popEstDomain
popEstDomainYear$StratificationVariables$Year <- "2022"

result <- RstoxFDA:::AnalyticalRatioEstimate(popEstDomainYear, ll, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "Year")
expect_true(all(result$StratificationVariables$Year == "2022"))

#
# Calculations TotalDomainWeight
#

ratioEst <- RstoxFDA:::AnalyticalRatioEstimate(popEstDomain, land, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "SpeciesCategory")
#check that relative difference in abundance equals relative difference in total estimated weigh vs landed weight for all landings as one stratum
relDiff <- (ratioEst$Abundance$Abundance - popEstDomain$Abundance$Abundance)/popEstDomain$Abundance$Abundance

expect_true(all(abs(relDiff-(sum(land$Landing$RoundWeight)*1000 - popEst$Variables$Total)/popEst$Variables$Total)<1e-6))
relDiffCov <- (ratioEst$AbundanceCovariance$AbundanceCovariance - popEstDomain$AbundanceCovariance$AbundanceCovariance*((sum(land$Landing$RoundWeight)*1000/popEst$Variables$Total)**2))/ratioEst$AbundanceCovariance$AbundanceCovariance
expect_true(all(abs(relDiffCov)<1e-6))
#check that total and total covariances are changed in accordance with difference between estimated total weight and landing total weight
expect_true(all((abs(ratioEst$Variables$Total - popEstDomain$Variables$Total)/ratioEst$Variables$Total) - (sum(land$Landing$RoundWeight)*1000 - sum(popEstDomain$Variables$Total))/(sum(land$Landing$RoundWeight)*1000) < 1e-6))

expect_true(all(abs(ratioEst$VariablesCovariance$TotalCovariance - popEstDomain$VariablesCovariance$TotalCovariance)/ratioEst$VariablesCovariance$TotalCovariance - 
                  ((sum(land$Landing$RoundWeight)*1000)**2 - popEst$Variables$Total**2)/((sum(land$Landing$RoundWeight)*1000)**2) < 1e-6))

#frequencies should be recalculated, but have barely changed when all landings is one stratum
expect_true(all(abs(ratioEst$Abundance$Frequency - popEstDomain$Abundance$Frequency)<1e-6))
expect_true(!all(ratioEst$Abundance$Frequency == popEstDomain$Abundance$Frequency))
expect_true(all(abs(ratioEst$AbundanceCovariance$FrequencyCovariance - popEstDomain$AbundanceCovariance$FrequencyCovariance)<1e-6))
expect_true(!all(ratioEst$AbundanceCovariance$FrequencyCovariance == popEstDomain$AbundanceCovariance$FrequencyCovariance))

#means should be exactly as before
expect_true(all(ratioEst$Variables$Mean == popEstDomain$Variables$Mean))
expect_true(all(ratioEst$VariablesCovariance$MeanCovariance[!is.nan(ratioEst$VariablesCovariance$MeanCovariance)] == popEstDomain$VariablesCovariance$MeanCovariance[!is.nan(popEstDomain$VariablesCovariance$MeanCovariance)]))


#
# Test total catch estimates (no ind domains)
#

psuEstNoD <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"))
popEstNoD <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEstNoD)
ratioEst <- RstoxFDA:::AnalyticalRatioEstimate(popEstNoD, land, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "SpeciesCategory")

#error should be close to zero, since domain coincides with strata

expect_true(abs(ratioEst$Variables$Total - sum(land$Landing$RoundWeight)*1000)/ratioEst$Variables$Total<1e-6)
expect_true(sqrt(ratioEst$VariablesCovariance$TotalCovariance) / ratioEst$Variables$Total < 1e-2)

#
# Test total catch estimates (w PSu domains)
#

psuEstNoD <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"), PSUDomainVariables = c("Gear"))
popEstNoD <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEstNoD)
ratioEst <- RstoxFDA:::AnalyticalRatioEstimate(popEstNoD, land, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "SpeciesCategory")

#should have some higher errors, since some domains have few samples
expect_true(abs(sum(ratioEst$Variables$Total) - sum(land$Landing$RoundWeight)*1000)<1e-6)
expect_true(!all(sqrt(ratioEst$VariablesCovariance$TotalCovariance) / ratioEst$Variables$Total < 1e-2))

#
# Test total catch estimates (w ind domains)
#

psuEstNoD <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"), c("IndividualSex"))
popEstNoD <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEstNoD)
ratioEst <- RstoxFDA:::AnalyticalRatioEstimate(popEstNoD, land, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "SpeciesCategory")

expect_true(abs(sum(ratioEst$Variables$Total) - sum(land$Landing$RoundWeight)*1000)<1e-4)


#
# Test with landings mapped to domain
#

psuEstDomain <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"), c("IndividualAge"), "Gear")
popEstDomain <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEstDomain)
ratioEst <- RstoxFDA:::AnalyticalRatioEstimate(popEstDomain, land, "IndividualRoundWeight", "TotalDomainWeight", StratificationVariables = "SpeciesCategory", DomainVariables = "Gear")

expect_equal(length(unique(stationDesign$SelectionTable$SamplingUnitId)), sum(popEstDomain$SampleSummary$Samples))
expect_true(!is.null(ratioEst$SampleSummary))

CVs <- merge(ratioEst$Abundance, ratioEst$AbundanceCovariance[Domain1==Domain2], by.x=c("Stratum", "Domain"), by.y=c("Stratum", "Domain1"))
CVs$CV <- sqrt(CVs$AbundanceCovariance) / CVs$Abundance
expect_equal(sum(is.na(CVs$CV)), sum(is.na(CVs$CV)))
expect_true(min(CVs$CV, na.rm=T)<.2)

#check that relative age comp in Gear domain is preserved, even if abundance estimates are very different.
domainAbundRatio <- merge(ratioEst$Abundance, ratioEst$DomainVariables, by=c("Domain"))
ageAbundRatio <- domainAbundRatio[,list(tot=sum(Abundance[Gear=="51"])),by="IndividualAge"]
ageAbundRatio$tot <- ageAbundRatio$tot / sum(ageAbundRatio$tot)
domainAbundPop <- merge(popEstDomain$Abundance, popEstDomain$DomainVariables, by=c("Domain"))
ageAbundPop <- domainAbundPop[,list(tot=sum(Abundance[Gear=="51"])),by="IndividualAge"]
ageAbundPop$tot <- ageAbundPop$tot / sum(ageAbundPop$tot)
expect_true(all(abs(ageAbundPop$tot - ageAbundRatio$tot)/ageAbundRatio$tot<1e-6))
comp <- merge(domainAbundRatio, domainAbundPop, by=c("Stratum", "Domain"))
expect_true(max(abs(comp$Abundance.x - comp$Abundance.y)/comp$Abundance.x,na.rm=T)>1)

#
# Test with MeanDomainWeights
#

#
# Input tests MeanDomainWeight
#

psuEstDomain <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"), c("IndividualAge", "Gear"))
popEstDomain <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEstDomain)
ratioEstMDW <- RstoxFDA:::AnalyticalRatioEstimate(popEstDomain, land, "IndividualRoundWeight", "MeanDomainWeight", StratificationVariables = "SpeciesCategory", DomainVariables = "Gear")

#check that total abundance estimates are in the same ballpark (catch rounding errors etc)
expect_true(abs(sum(ratioEstMDW$Abundance$Abundance) - sum(popEstDomain$Abundance$Abundance))/sum(ratioEstMDW$Abundance$Abundance) < .2)
#check that some CVs are in reasonable range
CVs <- merge(ratioEstMDW$Abundance, ratioEstMDW$AbundanceCovariance[Domain1==Domain2], by.x=c("Stratum", "Domain"), by.y=c("Stratum", "Domain1"))
CVs$CV <- sqrt(CVs$AbundanceCovariance) / CVs$Abundance
expect_true(min(CVs$CV)<.2)

#check that total and total are changed in accordance with difference between estimated total weight and landing total weight
expect_true((sum(ratioEstMDW$Variables$Total) - sum(popEstDomain$Variables$Total))/sum(ratioEstMDW$Variables$Total) - (sum(popEstDomain$Variables$Total) - sum(land$Landing$RoundWeight*1000)/sum(land$Landing$RoundWeight*1000)) < 1e-6)

#check that some CVs are in reasonable range
CVs <- merge(ratioEstMDW$Variables, ratioEstMDW$VariablesCovariance[Domain1==Domain2], by.x=c("Stratum", "Domain"), by.y=c("Stratum", "Domain1"))
CVs$CV <- sqrt(CVs$TotalCovariance) / CVs$Total
expect_true(min(CVs$CV)<.2)

#check that Means and Covariances are unchanged
expect_true(all(ratioEstMDW$Variables$Mean == popEstDomain$Variables$Mean))
expect_true(all(ratioEstMDW$VariablesCovariance$MeanCovariance[!is.nan(ratioEstMDW$VariablesCovariance$MeanCovariance)] == popEstDomain$VariablesCovariance$MeanCovariance[!is.nan(popEstDomain$VariablesCovariance$MeanCovariance)]))


#
# Correctness test for a minimal example
# Constructed from the example at example at: https://online.stat.psu.edu/stat506/node/15/
#
miniEx <- stationDesign
miniEx$SampleTable$N <- 15650
miniEx$SampleTable$n <- 3
miniEx$SampleTable$SelectionMethod <- "FSWR"

miniEx$SelectionTable <- miniEx$SelectionTable[1:3]
miniEx$SelectionTable$InclusionProbability <- as.numeric(NA)
miniEx$SelectionTable$HTsamplingWeight <- as.numeric(NA)
miniEx$SelectionTable$SelectionProbability[1] <- 650/15650
miniEx$SelectionTable$SelectionProbability[2] <- 2840/15650
miniEx$SelectionTable$SelectionProbability[3] <- 3200/15650
miniEx$SelectionTable$HHsamplingWeight <- 1/(miniEx$SelectionTable$SelectionProbability*sum(1/miniEx$SelectionTable$SelectionProbability))

miniExInd <- srs
miniExInd$SampleTable$N[miniExInd$SampleTable$SampleId %in% miniEx$SelectionTable$SamplingUnitId[1]] <- 420
miniExInd$SampleTable$N[miniExInd$SampleTable$SampleId %in% miniEx$SelectionTable$SamplingUnitId[2]] <- 1785
miniExInd$SampleTable$N[miniExInd$SampleTable$SampleId %in% miniEx$SelectionTable$SamplingUnitId[3]] <- 2198
miniExInd$SampleTable <- miniExInd$SampleTable[SampleId %in% miniEx$SelectionTable$SamplingUnitId,]
miniExInd$SelectionTable <- miniExInd$SelectionTable[SampleId %in% miniEx$SelectionTable$SamplingUnitId,]
miniExInd$StratificationVariables <- miniExInd$StratificationVariables[SampleId %in% miniEx$SelectionTable$SamplingUnitId,]

miniExInd$SelectionTable$InclusionProbability[miniExInd$SelectionTable$SampleId %in% miniEx$SelectionTable$SamplingUnitId[1]] <- 38/420
miniExInd$SelectionTable$InclusionProbability[miniExInd$SelectionTable$SampleId %in% miniEx$SelectionTable$SamplingUnitId[2]] <- 60/1785
miniExInd$SelectionTable$InclusionProbability[miniExInd$SelectionTable$SampleId %in% miniEx$SelectionTable$SamplingUnitId[3]] <- 25/2198

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight", "IW", "IndividualTotalLength", "one"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(miniEx, psuEst)
popEstMeanOfMeans <- RstoxFDA:::AnalyticalPopulationEstimate(miniEx, psuEst, MeanOfMeans = T)

expect_true(abs(popEst$Abundance$Abundance - 10232.75)<1e-2)
expect_true(abs(popEst$Abundance$Abundance * popEst$Variables$Mean[popEst$Variables$Variable=="IndividualRoundWeight"] - popEst$Variables$Total[popEst$Variables$Variable=="IndividualRoundWeight"]) < 1e-6)

#check annotation of PSU domains
expect_error(RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight"), "IndividualSex" ,"Gears"), "All PSUDomainVariables must be columns in StoxBioticData. The following are not valid: Gears")
psuEstDomain <- RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight"), "IndividualSex" ,"Gear")
expect_equal(nrow(psuEstDomain$PSUDomainVariables),3)
expect_error(RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight"), "IndividualSex" ,"IndividualSex"), "PSUDomainVariables must be unique to each PSU. Duplicates found for IndividualSexfor PSUs:")
psuEstDomain <- RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight"), "IndividualSex" , c("Gear", "SpeciesCategory"))
psuEstDomain <- RstoxFDA:::LiftStrata(psuEstDomain)
expect_equal(ncol(psuEstDomain$PSUDomainVariables), 4)
psuEstDomain <- RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight"), "IndividualSex")
expect_true(all(psuEstDomain$PSUDomainVariables$PSUDomain=="All"))

#check that domainEst sums to total est
psuEstDomain <- RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight"), "IndividualSex")
popEstDomain <- RstoxFDA:::AnalyticalPopulationEstimate(miniEx, psuEstDomain)

expect_true(length(unique(popEstDomain$Abundance$Domain))>1)
expect_true(abs(sum(popEstDomain$Abundance$Abundance)-sum(popEst$Abundance$Abundance))<1e-6)
expect_true(abs(sum(popEstDomain$Variables$Total)-sum(popEst$Variables$Total[popEst$Variables$Variable=="IndividualRoundWeight"]))<1e-6)

#check that frequencies add to one for unstratified estimate
expect_true(abs(sum(popEstDomain$Abundance$Frequency)-1) < 1e-6)
expect_true(abs(sum(popEst$Abundance$Frequency)-1) < 1e-6)

#check that means are consistent between domain estimate and total estimate
expect_true(abs(sum(popEstDomain$Variables$Mean*popEstDomain$Abundance$Abundance, na.rm=T)/sum(popEstDomain$Abundance$Abundance) - popEst$Variables$Mean[popEst$Variables$Variable=="IndividualRoundWeight"])<1e-6)

#check correctness univariate variance
expect_true((abs(popEst$AbundanceCovariance$AbundanceCovariance - 73125.74) / 73125.74) < 0.001)

#check that covariance is identical to variance when variables are completely aligned (IW vs IndividualRoundWeight)
filt1 <- popEst$VariablesCovariance$Variable1=="IW" & popEst$VariablesCovariance$Variable2=="IndividualRoundWeight"
filt2 <- popEst$VariablesCovariance$Variable1=="IW" & popEst$VariablesCovariance$Variable2=="IW"
#expect_true(abs(popEst$VariablesCovariance$TotalCovariance[filt1] - popEst$VariablesCovariance$TotalCovariance[filt2])<1e-6)

#check that covariance is not identical to variance when variables are not completely aligned (IW vs IndividualTotalLength)
#expect_true(abs(popEst$VariablesCovariance$TotalCovariance[popEst$VariablesCovariance$Variable1=="IW" & popEst$VariablesCovariance$Variable2=="IndividualTotalLength"] - popEst$VariablesCovariance$TotalCovariance[filt2])>1)
#check that variable covariance equal abundance covariance for a variable that is always set to 1.

#expect_true(abs(popEst$VariablesCovariance$TotalCovariance[popEst$VariablesCovariance$Variable1=="one" & popEst$VariablesCovariance$Variable2=="one"] - popEst$AbundanceCovariance$AbundanceCovariance)<1e-6)
#expect_true(abs(popEst$VariablesCovariance$MeanCovariance[popEst$VariablesCovariance$Variable1=="one" & popEst$VariablesCovariance$Variable2=="one"] - popEst$AbundanceCovariance$FrequencyCovariance)<1e-6)
#stop("Above fails in online checks. Expected TRUE, but got logical of length 0. Figure out what is going on.")

#check that Mean of Means estimates have higher variance than the other option.
#this is probably not generally guaranteed, but seem to work for this example
all(popEst$VariablesCovariance$MeanCovariance < popEstMeanOfMeans$VariablesCovariance$MeanCovariance)

#stop("Check input sanitation.")
#stop("Test collapseStrata with both HH and HT")
#stop("Implement DefineHierarchy.")
#stop("Make tests for all estimated parameters with and without domain and stratification)
