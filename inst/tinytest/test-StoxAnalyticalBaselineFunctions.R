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
ss<-RstoxFDA:::DefineIndividualSamplingParameters(NULL, bioStrat, "Stratified", c("IndividualAge", "IndividualRoundWeight"), StratificationColumns = c("LStrat"))
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
indSampling <- RstoxFDA:::DefineIndividualSamplingParameters(NULL, data, "SRS", c("IndividualAge"))
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
ls <- RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "SRS", c("IndividualTotalLength"))

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
lengthStratMissingStrata <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "LengthStratified", c("IndividualAge"), LengthInterval = 5)
expect_warning(psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, lengthStratMissingStrata, "IndividualRoundWeight", c("IndividualAge")), "Not all strata are sampled. Estimates will not be provided for some strata for SampleIds:")

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

naAbundance <- psuEst$Abundance[is.na(psuEst$Abundance$Abundance),]
unSampled <- lengthStratMissingStrata$SampleTable[N>0 & n==0]
expect_true(nrow(naAbundance)==nrow(unSampled)*nrow(psuEst$DomainVariables))

#
# Test LiftStrata
#
sexStrat <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "Stratified", c("IndividualAge"), StratificationColumns = "IndividualSex")
expect_warning(psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ss, sexStrat, "IndividualRoundWeight", c("IndividualSex")), "Not all strata are sampled. Estimates will not be provided for some strata for SampleIds:")
expect_true(sum(is.na(psuEst$Variables$Domain))==0)
psuEstLifted <- RstoxFDA:::LiftStrata(psuEst)

expect_true(nrow(psuEstLifted$Abundance)==nrow(psuEstLifted$Variables))
expect_true(nrow(psuEstLifted$Abundance)==length(unique(sexStrat$StratificationVariables$Stratum))*length(unique(sexStrat$SampleTable$SampleId))*length(unique(ss$Individual$IndividualSex)))
expect_true(nrow(psuEstLifted$StratificationVariables)==length(unique(sexStrat$StratificationVariables$Stratum))*length(unique(sexStrat$SampleTable$SampleId)))

#
# Test AnalyticalPopulationEstimate
#
stationDesign <- RstoxFDA:::DefinePSUSamplingParameters(NULL, "AdHocStoxBiotic", StoxBioticData = ss, SamplingUnitId = "Haul", StratificationColumns = "Gear")
sexStrat <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "Stratified", c("IndividualAge"), StratificationColumns = "IndividualSex")
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
sexStrat <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "Stratified", c("IndividualTotalLength"), StratificationColumns = "IndividualSex")
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

stationDesign <- RstoxFDA:::DefinePSUSamplingParameters(NULL, "AdHocStoxBiotic", StoxBioticData = ss, SamplingUnitId = "Haul", StratificationColumns = "Gear")
stationDesign$SampleTable$n[stationDesign$SampleTable$Stratum==40]<-0
stationDesign$SelectionTable <- stationDesign$SelectionTable[Stratum!="40",]
sexStrat <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, ss, "Stratified", c("IndividualAge"), StratificationColumns = "IndividualSex")
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
ex$Haul$serialnumber <- ex$Haul$HaulKey
ex$Individual$IW <- ex$Individual$IndividualRoundWeight
stationDesign <- RstoxFDA::AssignPSUSamplingParameters(stationDesign, ex, "serialnumber", "Haul", "MissingAtRandom")
srs <-  RstoxFDA:::DefineIndividualSamplingParameters(NULL, ex, "SRS", c("IndividualAge"))
psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight", "IndividualTotalLength"), c("IndividualAge"))
popEstAgeDomain <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst)
popEstMeanOfMeans <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst, MeanOfMeans = T)

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, srs, c("IndividualRoundWeight"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(stationDesign, psuEst)

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

psuEst <- RstoxFDA:::AnalyticalPSUEstimate(ex, miniExInd, c("IndividualRoundWeight", "IW", "IndividualTotalLength"))
popEst <- RstoxFDA:::AnalyticalPopulationEstimate(miniEx, psuEst)

expect_true(abs(popEst$Abundance$Abundance - 10232.75)<1e-2)
expect_true(abs(popEst$Abundance$Abundance * popEst$Variables$Mean[popEst$Variables$Variable=="IndividualRoundWeight"] - popEst$Variables$Total[popEst$Variables$Variable=="IndividualRoundWeight"]) < 1e-6)

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
expect_true(abs(popEst$VariablesCovariance[Variable1=="IW" & Variable2=="IndividualRoundWeight"][["TotalCovariance"]] - popEst$VariablesCovariance[Variable1=="IW" & Variable2=="IW"][["TotalCovariance"]])<1e-6)
#check that covariance is not identical to variance when variables are not completely aligned (IW vs IndividualTotalLength)
expect_true(abs(popEst$VariablesCovariance[Variable1=="IW" & Variable2=="IndividualTotalLength"][["TotalCovariance"]] - popEst$VariablesCovariance[Variable1=="IW" & Variable2=="IW"][["TotalCovariance"]])>1)

browser()
# Fix issue with variances for Variables
#chekc covars
# Test all variances. Abundance, Total, Mean and Frequency, and check def for Mean of Means
# Redocument AnalyticalPopulationEstimate to include covariatestructures
#stop("Document AnalyticalPSUEstimate.")
#stop("Expose collapseStrata and test")
#stop("Test collapseStrata with both HH and HT")
#stop("Document AnalyticalPopulationEstimate")
