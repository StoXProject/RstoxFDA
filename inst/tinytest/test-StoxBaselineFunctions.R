library(RstoxData) #necessary, probably because of how xsdObjects is referred internally in RstoxData (see https://github.com/StoXProject/RstoxData/issues/253)
fdiropen <- system.file("testresources", "landingsvariants", "openfdir.2021.csv", package="RstoxFDA")
xmllandings <- system.file("testresources", "landingsvariants", "landing.xml", package="RstoxFDA")
lss1 <- system.file("testresources", "landingsvariants", "lss_2005.psv", package="RstoxFDA")
lss2 <- system.file("testresources", "landingsvariants", "lss_2017.psv", package="RstoxFDA")
lss3 <- system.file("testresources", "landingsvariants", "lss_2018.psv", package="RstoxFDA")
xmlread <- RstoxFDA:::ReadLandingFDA(xmllandings)
expect_true(RstoxData::is.LandingData(xmlread))
expect_error(RstoxFDA:::ReadLandingFDA(lss1))
lss1read <- RstoxFDA:::ReadLandingFDA(lss1, Format="lss")
expect_true(RstoxData::is.LandingData(lss1read))
sd <- RstoxData::StoxLanding(lss1read)
expect_true(RstoxData::is.StoxLandingData(sd))
lss2read <- RstoxFDA:::ReadLandingFDA(lss2, Format="lss")
expect_true(RstoxData::is.LandingData(lss2read))
sd <- RstoxData::StoxLanding(lss2read)
expect_true(RstoxData::is.StoxLandingData(sd))
lss3read <- RstoxFDA:::ReadLandingFDA(lss3, Format="lss")
expect_true(RstoxData::is.LandingData(lss3read))
sd <- RstoxData::StoxLanding(lss3read)
expect_true(RstoxData::is.StoxLandingData(sd))
openFdirRead <- RstoxFDA:::ReadLandingFDA(fdiropen, Format="FDIR.2021")
expect_true(RstoxData::is.LandingData(openFdirRead))
sd <- RstoxData::StoxLanding(openFdirRead)
expect_true(RstoxData::is.StoxLandingData(sd))

bioticfile <- system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA")
nmdbiotic <- RstoxData::ReadBiotic(bioticfile)
nmdbioticPost <- RstoxFDA::SetShortGearBiotic(nmdbiotic)
expect_true(all(nmdbioticPost$biotic_v3_example.xml$fishstation$gear=="41"))
#check that codes that are not 4 chars long are not changed
nmdbiotic$biotic_v3_example.xml$fishstation$gear[1]<-"21"
nmdbioticPost <- RstoxFDA::SetShortGearBiotic(nmdbiotic)
expect_true(all(nmdbioticPost$biotic_v3_example.xml$fishstation$gear==c("21","41")))
#check that handles NA
nmdbiotic$biotic_v3_example.xml$fishstation$gear[1]<-NA
nmdbioticPost <- RstoxFDA::SetShortGearBiotic(nmdbiotic)
expect_true(is.na(nmdbioticPost$biotic_v3_example.xml$fishstation$gear[1]))
expect_true(nmdbioticPost$biotic_v3_example.xml$fishstation$gear[2]=="41")




#context("test-StoxBaselineFunctions: LoadFdaStratumPolygon")
p <- RstoxFDA::LoadFdaStratumPolygon("sss", "FDIR.2017", UseProcessData = T)
expect_equal(p, "sss")
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "FDIR.2017")
expect_equal(nrow(p), 60)
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "FDIR.2018")
expect_equal(nrow(p), 60)
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "ICES.2018")
expect_equal(nrow(p), 66)
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "NAFO")
expect_equal(nrow(p), 27)
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "NAFO.FDIR.2017")
expect_equal(nrow(p), 87)
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "NAFO.FDIR.2018")
expect_equal(nrow(p), 87)
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "ICES.SubArea.2018")
expect_equal(nrow(p), length(unique(RstoxFDA::ICESareas$SubArea)))
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "ICES.Division.2018")
expect_equal(nrow(p), length(unique(paste(RstoxFDA::ICESareas$SubArea, RstoxFDA::ICESareas$Division, sep="."))))
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "ICES.SubDivision.2018")
expect_equal(nrow(p), length(unique(paste(RstoxFDA::ICESareas$SubArea[!is.na(RstoxFDA::ICESareas$SubDivision)], RstoxFDA::ICESareas$Division[!is.na(RstoxFDA::ICESareas$SubDivision)], RstoxFDA::ICESareas$SubDivision[!is.na(RstoxFDA::ICESareas$SubDivision)], sep="."))))
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "ICES.Unit.2018")
expect_equal(nrow(p), length(unique(paste(RstoxFDA::ICESareas$SubArea[!is.na(RstoxFDA::ICESareas$Unit)], RstoxFDA::ICESareas$Division[!is.na(RstoxFDA::ICESareas$Unit)], RstoxFDA::ICESareas$SubDivision[!is.na(RstoxFDA::ICESareas$Unit)], RstoxFDA::ICESareas$Unit[!is.na(RstoxFDA::ICESareas$Unit)], sep="."))))
p <- RstoxFDA::LoadFdaStratumPolygon(NULL, "ICES.Rectangles.2018")
expect_true(all(nchar(p$StratumName)==4))
expect_error(RstoxFDA::LoadFdaStratumPolygon(NULL, "NAFO.FDIR.201"), "Does not recognize option")



#context("test-StoxBaselineFunctions: ListBioticDifference")

bioticfile <- system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA")
nmdbiotic <- RstoxData::ReadBiotic(bioticfile)
nmdbiotic$biotic_v3_example.xml$fishstation$stationstartdate <- nmdbiotic$biotic_v3_example.xml$fishstation$stationstopdate
expect_warning(nmdbiotic <- RstoxFDA::SetTimeBiotic(nmdbiotic))
nmdbiotic$biotic_v3_example.xml$individual$individualproducttype <- 1
nmdbiotic$biotic_v3_example.xml$catchsample <- nmdbiotic$biotic_v3_example.xml$catchsample[nmdbiotic$biotic_v3_example.xml$catchsample$lengthmeasurement == "E",]
nmdbiotic$biotic_v3_example.xml$catchsample$sampleproducttype <- 1
nmdbiotic$biotic_v3_example.xml$catchsample$catchproducttype <- 1
StoxBiotic <- RstoxData::StoxBiotic(nmdbiotic)

data <- RstoxFDA::ListBioticDifference(StoxBiotic, nmdbiotic)
expect_equal(nrow(data$fishstation),0)

#context("test-StoxBaselineFunctions: FilterAgeLengthOutliersStoxBiotic")
filterExpression <- list()
filterExpression$SpeciesCategory <- c(
  'SpeciesCategory == "torsk/164712/126436/NA"'
)
StoxBioticCod <- RstoxData::FilterStoxBiotic(StoxBiotic, FilterExpression = filterExpression)
filt <- RstoxFDA::FilterAgeLengthOutliersStoxBiotic(StoxBioticCod, Linf = 232.98028344, K=0.05284384, sigma=0.16180306, kAl=4)

#test that filter does not set fractional age in output
remainder<-round(filt$Individual$IndividualAge) -filt$Individual$IndividualAge
expect_true(all(remainder[!is.na(remainder)]==0))

expect_equal(nrow(filt$Individual), nrow(StoxBioticCod$Individual))
StoxBioticCod$Individual <- StoxBioticCod$Individual[!is.na(StoxBioticCod$Individual$IndividualAge),]
filt <- RstoxFDA::FilterAgeLengthOutliersStoxBiotic(StoxBioticCod, Linf = 232.98028344, K=0.05284384, sigma=0.16180306, kAl=1)
expect_true(nrow(filt$Individual) < nrow(StoxBioticCod$Individual))
expect_equal(nrow(filt$Station), nrow(StoxBioticCod$Station))
filt <- RstoxFDA::FilterAgeLengthOutliersStoxBiotic(StoxBioticCod, Linf = 232.98028344, K=0.05284384, sigma=0.16180306, kAl=.01, FilterUpwards = T)
expect_true(nrow(filt$Individual) < nrow(StoxBioticCod$Individual))
expect_true(nrow(filt$Station) < nrow(StoxBioticCod$Station))


#context("test-StoxBaselineFunctions: FilterWeightLengthOutliersStoxBiotic")
filterExpression <- list()
filterExpression$SpeciesCategory <- c(
  'SpeciesCategory == "torsk/164712/126436/NA"'
)
StoxBioticCod <- RstoxData::FilterStoxBiotic(StoxBiotic, FilterExpression = filterExpression)
filt <- RstoxFDA::FilterWeightLengthOutliersStoxBiotic(StoxBioticCod, logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl=4)
expect_equal(nrow(filt$Individual), nrow(StoxBioticCod$Individual))

StoxBioticCod$Individual <- StoxBioticCod$Individual[!is.na(StoxBioticCod$Individual$IndividualRoundWeight),]
filt <- RstoxFDA::FilterWeightLengthOutliersStoxBiotic(StoxBioticCod, logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl=1)
expect_true(nrow(filt$Individual) < nrow(StoxBioticCod$Individual))
expect_equal(nrow(filt$Station), nrow(StoxBioticCod$Station))
filt <- RstoxFDA::FilterWeightLengthOutliersStoxBiotic(StoxBioticCod, logalfa = -5.0061, beta = 3.0716, sigma = 0.1454,, kAl=.01, FilterUpwards = T)
expect_true(nrow(filt$Individual) < nrow(StoxBioticCod$Individual))
expect_true(nrow(filt$Station) < nrow(StoxBioticCod$Station))

#context("test-StoxBaselineFunctions: DefineLengthConversionParameters")
conversionfile <- system.file("testresources","lengthConversion.txt", package="RstoxFDA")
tab <- RstoxFDA::DefineLengthConversionParameters(FileName=conversionfile)
expect_true(RstoxFDA::is.LengthConversionTable(tab))

expect_equal(sum(!is.na(tab$Alpha)), 2)
suppressWarnings(expect_error(RstoxFDA::DefineLengthConversionParameters(FileName=system.file("testresources","geargroupsLandings.txt", package="RstoxFDA")), "Resource file does not have required columns: Description, Species, MeasurementType, Alpha, Beta"))
expect_error(RstoxFDA::DefineLengthConversionParameters(FileName=system.file("testresources","lengthConversionDup.txt", package="RstoxFDA")), "File contains duplicate definitions ")

#context("test-StoxBaselineFunctions: convertLengthBiotic")
bioticfile <- system.file("testresources","biotic_v3_lengthmeasurements.xml", package="RstoxFDA")
conversionfile <- system.file("testresources","lengthConversion.txt", package="RstoxFDA")
tab <- RstoxFDA::DefineLengthConversionParameters(FileName=conversionfile)
BioticData <- RstoxData::ReadBiotic(bioticfile)

expect_warning(expect_error(RstoxFDA::ConvertLengthBiotic(BioticData, tab, "E"),"Could not convert length for unkown length measurement for individuals."))
BioticData$biotic_v3_lengthmeasurements.xml$catchsample$lengthmeasurement[2]<-"E"
expect_warning(expect_error(RstoxFDA::ConvertLengthBiotic(BioticData, tab, "E"),"Conversion parameters not found for length measurement K for species 164744"))
BioticData$biotic_v3_lengthmeasurements.xml$catchsample$lengthmeasurement[12] <- "E"
BioticDataPost <- RstoxFDA::ConvertLengthBiotic(BioticData, tab, "E")
nonNaLmPost <- BioticDataPost$biotic_v3_lengthmeasurements.xml$catchsample$lengthmeasurement[!is.na(BioticDataPost$biotic_v3_lengthmeasurements.xml$catchsample$lengthmeasurement)]
expect_true(all(nonNaLmPost == "E"))

#check that lengthmeasuremnt NAs are preserved
expect_equal(is.na(BioticData$biotic_v3_lengthmeasurements.xml$catchsample$lengthmeasurement), is.na(BioticDataPost$biotic_v3_lengthmeasurements.xml$catchsample$lengthmeasurement))

#check conversion
expect_equal(BioticDataPost$biotic_v3_lengthmeasurements.xml$individual$length[1],
             BioticData$biotic_v3_lengthmeasurements.xml$individual$length[1]*3.65 + .03)

#context("test-StoxBaselineFunctions: DefineWeightConversionFactor")
conversionfile <- system.file("testresources","conversionFactors.txt", package="RstoxFDA")
tab <- RstoxFDA::DefineWeightConversionFactor(FileName=conversionfile)
expect_true(RstoxFDA::is.WeightConversionTable(tab))
expect_true(is.na(tab$WeightFactor[7]))
expect_equal(sum(!is.na(tab$WeightFactor)), 6)
suppressWarnings(expect_error(RstoxFDA::DefineWeightConversionFactor(FileName=system.file("testresources","geargroupsLandings.txt", package="RstoxFDA"))))
expect_error(RstoxFDA::DefineWeightConversionFactor(FileName=system.file("testresources","conversionFactorsDuplicates.txt", package="RstoxFDA")), "File contains duplicate definitions ")

#context("test-StoxBaselineFunctions: ConvertWeightBiotic")
bioticfile <- system.file("testresources","biotic_v3_producttypes.xml", package="RstoxFDA")
conversionfile <- system.file("testresources","conversionFactors.txt", package="RstoxFDA")
BioticData <- RstoxData::ReadBiotic(bioticfile)
tab <- RstoxFDA::DefineWeightConversionFactor(FileName=conversionfile)
BioticDataPost <- RstoxFDA::ConvertWeightBiotic(BioticData, WeightConversionTable = tab, TargetProductType = "1")


#check error message
Bd <- BioticData
Bd$biotic_v3_producttypes.xml$catchsample$sampleproducttype[9]<-NA
expect_error(RstoxFDA::ConvertWeightBiotic(Bd, WeightConversionTable = tab, TargetProductType = "1"), "Not all necessary conversion factors found for species 164712. Missing for product types: NA")
Bd <- BioticData
Bd$biotic_v3_producttypes.xml$catchsample$catchproducttype[9]<-NA
expect_error(RstoxFDA::ConvertWeightBiotic(Bd, WeightConversionTable = tab, TargetProductType = "1"), "Not all necessary conversion factors found for species 164712. Missing for product types: NA")
Bd <- BioticData
Bd$biotic_v3_producttypes.xml$individual$individualproducttype<-NA
expect_error(RstoxFDA::ConvertWeightBiotic(Bd, WeightConversionTable = tab, TargetProductType = "1"), "Not all necessary conversion factors found for species 164712. Missing for product types: NA")




#check that producttype NAs are preserved
expect_equal(is.na(BioticData$biotic_v3_producttypes.xml$catchsample$sampleproducttype), is.na(BioticDataPost$biotic_v3_producttypes.xml$catchsample$sampleproducttype))
expect_equal(is.na(BioticData$biotic_v3_producttypes.xml$individual$individualproducttype), is.na(BioticDataPost$biotic_v3_producttypes.xml$individual$individualproducttype))

#check that producttype is otherwise uniform
expect_true(all(BioticDataPost$biotic_v3_producttypes.xml$catchsample$catchproducttype[!is.na(BioticData$biotic_v3_producttypes.xml$catchsample$catchproducttype)]=="1"))
expect_true(all(BioticDataPost$biotic_v3_producttypes.xml$catchsample$sampleproducttype[!is.na(BioticData$biotic_v3_producttypes.xml$catchsample$sampleproducttype)]=="1"))
expect_true(all(BioticDataPost$biotic_v3_producttypes.xml$individual$individualproducttype[!is.na(BioticData$biotic_v3_producttypes.xml$individual$individualproducttype)]=="1"))

#check official conversion factors
fdirtab <- RstoxFDA::DefineWeightConversionFactor(DefinitionMethod = "FDIR.VIII.2022")
expect_true(all(fdirtab$WeightFactor[fdirtab$ProductType==3] < fdirtab$WeightFactor[fdirtab$ProductType==4]))
expect_equal(sum(duplicated(fdirtab$Species)), nrow(fdirtab)/2)

#
# checks on catchweightconversion
#
#context("test-StoxBaselineFunctions: ConvertWeightBiotic catchweightconversion")
shouldBeConverted <- BioticData$biotic_v3_producttypes.xml$catchsample$commonname %in% c("torsk", "sei", "hyse") &
  !is.na(BioticData$biotic_v3_producttypes.xml$catchsample$catchweight) &
  BioticData$biotic_v3_producttypes.xml$catchsample$catchproducttype != "1"
expect_equal(sum(shouldBeConverted), 2)

wasConverted <- !is.na(BioticDataPost$biotic_v3_producttypes.xml$catchsample$catchweight) &
  BioticDataPost$biotic_v3_producttypes.xml$catchsample$catchweight - BioticData$biotic_v3_producttypes.xml$catchsample$catchweight > 1e-10

# check that all that was to be converted was converted
expect_true(all(wasConverted == shouldBeConverted))

# check that producttype codes was converted
expect_true(all(BioticDataPost$biotic_v3_producttypes.xml$catchsample$catchproducttype[wasConverted]=="1"))
expect_true(all(BioticData$biotic_v3_producttypes.xml$catchsample$catchproducttype[wasConverted]!="1"))

#check that species conforms to expectation
expect_equal(c("torsk","hyse"), BioticDataPost$biotic_v3_producttypes.xml$catchsample$commonname[wasConverted])

# check that correct factor was applied (refer to testresources/conversionFactors.txt)
postweightCod <- BioticDataPost$biotic_v3_producttypes.xml$catchsample[wasConverted,][1,"catchweight"]
preweightCod <- BioticData$biotic_v3_producttypes.xml$catchsample[wasConverted,][1,"catchweight"]
expect_equal(unlist(postweightCod / preweightCod)[[1]], 1.5) #COD prodtype 3

postweightHad <- BioticDataPost$biotic_v3_producttypes.xml$catchsample[wasConverted,][2,"catchweight"]
preweightHad <- BioticData$biotic_v3_producttypes.xml$catchsample[wasConverted,][2,"catchweight"]
expect_equal(unlist(postweightHad / preweightHad)[[1]], 2) #hadd prodtype 4



#
# checks on sampleweightconversion
#
#context("test-StoxBaselineFunctions: ConvertWeightBiotic sampleweightconversion")

shouldBeConverted <- BioticData$biotic_v3_producttypes.xml$catchsample$commonname %in% c("torsk", "sei", "hyse") &
  !is.na(BioticData$biotic_v3_producttypes.xml$catchsample$lengthsampleweight) &
  BioticData$biotic_v3_producttypes.xml$catchsample$sampleproducttype != "1"
expect_equal(sum(shouldBeConverted), 2)

wasConverted <- !is.na(BioticDataPost$biotic_v3_producttypes.xml$catchsample$lengthsampleweight) &
  BioticDataPost$biotic_v3_producttypes.xml$catchsample$lengthsampleweight - BioticData$biotic_v3_producttypes.xml$catchsample$lengthsampleweight > 1e-10

# check that all that was to be converted was converted
expect_true(all(wasConverted == shouldBeConverted))


# check that producttype codes was converted
expect_true(all(BioticDataPost$biotic_v3_producttypes.xml$catchsample$sampleproducttype[wasConverted]=="1"))
expect_true(all(BioticData$biotic_v3_producttypes.xml$catchsample$sampleproducttype[wasConverted]!="1"))

#check that species conforms to expectation
expect_equal(c("torsk","hyse"), BioticDataPost$biotic_v3_producttypes.xml$catchsample$commonname[wasConverted])

# check that correct factor was applied (refer to testresources/conversionFactors.txt)
postweightCod <- BioticDataPost$biotic_v3_producttypes.xml$catchsample[wasConverted,][1,"lengthsampleweight"]
preweightCod <- BioticData$biotic_v3_producttypes.xml$catchsample[wasConverted,][1,"lengthsampleweight"]
expect_equal(unlist(postweightCod / preweightCod)[[1]], 1.18)

postweightHad <- BioticDataPost$biotic_v3_producttypes.xml$catchsample[wasConverted,][2,"lengthsampleweight"]
preweightHad <- BioticData$biotic_v3_producttypes.xml$catchsample[wasConverted,][2,"lengthsampleweight"]
expect_equal(unlist(postweightHad / preweightHad)[[1]], 2)



#
# checks on individualweightconversion
#

#context("test-StoxBaselineFunctions: ConvertWeightBiotic individualweightconversion")

# not confirming species
shouldBeConverted <- !is.na(BioticData$biotic_v3_producttypes.xml$individual$individualweight) &
  BioticData$biotic_v3_producttypes.xml$individual$individualproducttype != "1"
wasConverted <- !is.na(BioticDataPost$biotic_v3_producttypes.xml$individual$individualweight) &
  BioticDataPost$biotic_v3_producttypes.xml$individual$individualweight - BioticData$biotic_v3_producttypes.xml$individual$individualweight > 1e-10

# check that producttype codes was converted
expect_true(all(BioticDataPost$biotic_v3_producttypes.xml$individual$individualproducttype[wasConverted]=="1"))
expect_true(all(BioticData$biotic_v3_producttypes.xml$individual$individualproducttype[wasConverted]!="1"))

#check that factors correspond to 1 saithe: type 4, rest haddock: type 4
postweight <- BioticDataPost$biotic_v3_producttypes.xml$individual$individualweight[wasConverted]
preweight <- BioticData$biotic_v3_producttypes.xml$individual$individualweight[wasConverted]
ratio <- postweight / preweight
expect_equal(sum(ratio==4), 1)
expect_equal(sum(ratio==2), sum(wasConverted)-1)

#context("test-StoxBaselineFunctions: IndividualLevel only")
BioticDataPostInd <- RstoxFDA::ConvertWeightBiotic(BioticData, ConversionType = "IndividualWeight", WeightConversionTable = tab, TargetProductType = "1")
# not confirming species
shouldBeConverted <- !is.na(BioticData$biotic_v3_producttypes.xml$individual$individualweight) &
  BioticData$biotic_v3_producttypes.xml$individual$individualproducttype != "1"
wasConverted <- !is.na(BioticDataPostInd$biotic_v3_producttypes.xml$individual$individualweight) &
  BioticDataPostInd$biotic_v3_producttypes.xml$individual$individualweight - BioticData$biotic_v3_producttypes.xml$individual$individualweight > 1e-10

# check that producttype codes was converted
expect_true(all(BioticDataPostInd$biotic_v3_producttypes.xml$individual$individualproducttype[wasConverted]=="1"))
expect_true(all(BioticData$biotic_v3_producttypes.xml$individual$individualproducttype[wasConverted]!="1"))

#check that factors correspond to 1 saithe: type 4, rest haddock: type 4
postweight <- BioticDataPostInd$biotic_v3_producttypes.xml$individual$individualweight[wasConverted]
preweight <- BioticData$biotic_v3_producttypes.xml$individual$individualweight[wasConverted]
ratio <- postweight / preweight
expect_equal(sum(ratio==4), 1)
expect_equal(sum(ratio==2), sum(wasConverted)-1)

#check that catchsample producttypes are not changed
expect_true(all(c("1","4") %in% BioticDataPostInd$biotic_v3_producttypes.xml$catchsample$sampleproducttype))
expect_true(all(c("1","3","4") %in% BioticDataPostInd$biotic_v3_producttypes.xml$catchsample$catchproducttype))
expect_equal(BioticDataPostInd$biotic_v3_producttypes.xml$catchsample$catchweight, BioticData$biotic_v3_producttypes.xml$catchsample$catchweight)
expect_equal(BioticDataPostInd$biotic_v3_producttypes.xml$catchsample$lengthsampleweight, BioticData$biotic_v3_producttypes.xml$catchsample$lengthsampleweight)

#context("test-StoxBaselineFunctions: SetTimeBiotic")
bioticfiles <- c(f1=system.file("testresources","biotic_v3_example.xml", package="RstoxFDA"))
BioticData <- RstoxData::ReadBiotic(bioticfiles)
BioticData$biotic_v3_example.xml$individual$individualproducttype <- 1
BioticData$biotic_v3_example.xml$catchsample$lengthmeasurement <- "E"
BioticData$biotic_v3_example.xml$catchsample$sampleproducttype <- 1
BioticData$biotic_v3_example.xml$catchsample$catchproducttype <- 1
BioticData$biotic_v3_example.xml$fishstation$stationstartdate <- BioticData$biotic_v3_example.xml$fishstation$stationstopdate
StoxBioticPre <- RstoxData::StoxBiotic(BioticData)
expect_warning(BioticDataPost <- RstoxFDA::SetTimeBiotic(BioticData))

expect_true(all(!is.na(BioticDataPost$biotic_v3_example.xml$fishstation$stationstarttime)))
StoxBioticPost <- RstoxData::StoxBiotic(BioticDataPost)
expect_true(sum(is.na(StoxBioticPost$Station$DateTime)) < sum(is.na(StoxBioticPre$Station$DateTime)))

#test other time format
expect_warning(BioticDataPost <- RstoxFDA::SetTimeBiotic(BioticData, Time="21:00:01Z"))
StoxBioticPost <- RstoxData::StoxBiotic(BioticDataPost)
expect_true(all(grepl("21:00:01", as.character(StoxBioticPost$Station$DateTime))))

#test missing Z
suppressWarnings(expect_error(RstoxFDA::SetTimeBiotic(BioticData, Time="21:00:01"), "Invalid time specification: 21:00:01. Provide as %H:%M:%SZ, e.g: 12:00:00Z"))

#test wrong time format
suppressWarnings(expect_error(RstoxFDA::SetTimeBiotic(BioticData, Time="32:00:01Z"), "Invalid time specification: 32:00:01Z. Provide as %H:%M:%SZ, e.g: 12:00:00Z"))
BioticData$biotic_v3_example.xml$fishstation$stationstarttime[1] <- "21:00:02"

#test overwrite
suppressWarnings(expect_equal(RstoxFDA::SetTimeBiotic(BioticData, Time="21:00:01Z")$biotic_v3_example.xml$fishstation$stationstarttime[1], "21:00:02"))
suppressWarnings(expect_equal(RstoxFDA::SetTimeBiotic(BioticData, Time="21:00:01Z", Overwrite = T)$biotic_v3_example.xml$fishstation$stationstarttime[1], "21:00:01Z"))

#context("test-StoxBaselineFunctions: SetStartDateBiotic")
bioticfiles <- system.file("testresources","biotic_v3_example.xml", package="RstoxFDA")
BioticData <- RstoxData::ReadBiotic(bioticfiles)
BioticData$biotic_v3_example.xml$individual$individualproducttype <- 1
BioticData$biotic_v3_example.xml$catchsample$lengthmeasurement <- "E"
BioticData$biotic_v3_example.xml$catchsample$sampleproducttype <- 1
BioticData$biotic_v3_example.xml$catchsample$catchproducttype <- 1
StoxBioticPre <- RstoxData::StoxBiotic(BioticData)
BioticDataPost <- RstoxFDA::SetStartDateBiotic(BioticData)
expect_true(all(!is.na(BioticDataPost$biotic_v3_example.xml$fishstation$stationstartdate)))
expect_true(sum(is.na(StoxBioticPost$Station$DateTime)) < sum(is.na(StoxBioticPre$Station$DateTime)))

#test overwrite
BioticData$biotic_v3_example.xml$fishstation$stationstartdate <- "1982-09-15Z"
expect_equal(RstoxFDA::SetStartDateBiotic(BioticData)$biotic_v3_example.xml$fishstation$stationstartdate[1], "1982-09-15Z")
expect_equal(RstoxFDA::SetStartDateBiotic(BioticData, Overwrite = T)$biotic_v3_example.xml$fishstation$stationstartdate[1], "2018-04-04Z")

#context("test-StoxBaselineFunctions: AddGearGroupStoxLanding")
gearDef <- RstoxData::DefineTranslation(DefinitionMethod = "ResourceFile", FileName=system.file("testresources","geargroupsLandings.txt", package="RstoxFDA"), ValueColumn = "Value", NewValueColumn = "NewValue", VariableName = "Gear")
landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
stoxLandingPost <- RstoxFDA::AddGearGroupStoxLanding(stoxLandingPre, gearDef)
expect_true("GearGroup" %in% names(stoxLandingPost$Landing))
expect_true(all(!is.na(stoxLandingPost$Landing$GearGroup)))

#context("test-StoxBaselineFunctions: AddGearGroupStoxBiotic")
gearDef <- RstoxData::DefineTranslation(DefinitionMethod = "ResourceFile", FileName=system.file("testresources","geargroupsBiotic.txt", package="RstoxFDA"), ValueColumn = "Value", NewValueColumn = "NewValue", VariableName = "Gear")
stoxbiotic <- readRDS(system.file("testresources","StoxBioticData.rds", package="RstoxFDA"))
stoxbioticPost <- RstoxFDA::AddGearGroupStoxBiotic(stoxbiotic, gearDef)
expect_true("GearGroup" %in% names(stoxbioticPost$Haul))
expect_equal(sum(!is.na(stoxbioticPost$Haul$GearGroup)), sum(!is.na(stoxbiotic$Haul$Gear)))

#context("test-StoxBaselineFunctions: AddGearGroupStoxBiotic missing gear codes")
stoxbiotic$Haul$Gear[1] <- NA
expect_error(suppressWarnings(RstoxFDA::AddGearGroupStoxBiotic(stoxbiotic, gearDef)), "'StoxBioticData' has missing values for the variable 'Gear' on the table 'Haul'")

#context("test-StoxBaselineFunctions: DefinePeriod")
temp <- RstoxFDA::DefinePeriod(NULL)
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 3)

#context("test-StoxBaselineFunctions: DefinePeriod useProcessData")
temp <- RstoxFDA::DefinePeriod(NULL, UseProcessData = T)
expect_true(is.null(temp))

#context("test-StoxBaselineFunctions: DefinePeriod Month")
temp <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Month")
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 12)
expect_equal(ncol(temp), 3)

temp <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Quarter")
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 3)

#context("test-StoxBaselineFunctions: DefinePeriod non-seasonal")
temp <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("04-02-2018", "04-09-2018"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 4)
expect_false(any(is.na(temp$StartYear)))


#context("test-StoxBaselineFunctions: DefinePeriod unrecognized category")
expect_error(RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Something"), "Does not recognize option")

#context("test-StoxBaselineFunctions: DefinePeriod Custom seasonal")
temp <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("05-02","15-09"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 2)
expect_equal(ncol(temp), 3)

#context("test-StoxBaselineFunctions: DefinePeriod Custom non-seasonal")
expect_error(RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-01","15-09","01-01")), "Need to provide unique periods.")
temp2 <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-01-2016","15-09-2016"))
expect_true(data.table::is.data.table(temp))

expect_equal(nrow(temp2), 3)
expect_equal(ncol(temp2), 4)
expect_false(any(is.na(temp2$StartYear)))



#context("test-StoxBaselineFunctions: DefineAreaPosition")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_incl.txt", package="RstoxFDA")
areaPos <- RstoxFDA::DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
expect_true(data.table::is.data.table(areaPos))
expect_equal(nrow(areaPos), 21)
expect_equal(ncol(areaPos), 4)

#context("test-StoxBaselineFunctions: DefineAreaPosition useProcessData")
nullPos <- RstoxFDA::DefineAreaPosition(NULL, FileName = regularfile, UseProcessData = T)
expect_true(is.null(nullPos))

#context("test-StoxBaselineFunctions: DefineAreaPosition malformed")
errorfile <- system.file("testresources","areaPosError.txt", package="RstoxFDA")
expect_error(RstoxFDA::DefineAreaPosition(NULL, FileName = errorfile), "Malformed resource file. Some Area does not have coordinates defined for the case when location is missing.")


#context("test-StoxBaselineFunctions: DefineAreaPosition stratumPolygon")
areaPos <- RstoxFDA::DefineAreaPosition(NULL, StratumPolygon = RstoxFDA::mainareaFdir2018, DefinitionMethod = "StratumPolygon")
expect_true(RstoxFDA::is.AreaPosition(areaPos))


#context("test-StoxBaselineFunctions: DefineCarNeighbours")
carfile <- system.file("testresources","mainarea_neighbour.txt", package="RstoxFDA")
car <- RstoxFDA::DefineCarNeighbours(NULL, FileName = carfile)
expect_true(data.table::is.data.table(car))
expect_equal(nrow(car), 60)
expect_equal(ncol(car), 2)

#context("test-StoxBaselineFunctions: DefineCarNeighbours useProcessData")
nullCar <- RstoxFDA::DefineCarNeighbours(NULL, FileName = carfile, UseProcessData = T)
expect_true(is.null(nullCar))

#context("test-StoxBaselineFunctions: DefineCarNeighbours non-symmetric")
errorfile <- system.file("testresources","mainarea_error.txt", package="RstoxFDA")
expect_error(RstoxFDA::DefineCarNeighbours(NULL, FileName = errorfile), "Neighbour definition not symmetric. 1 is neighbour of 0 but not vice versa.")

#context("test-StoxBaselineFunctions: DefineCarNeighbours repeated key")
errorfile <- system.file("testresources","mainarea_error2.txt", package="RstoxFDA")
expect_error(RstoxFDA::DefineCarNeighbours(NULL, FileName = errorfile), "Malformed resource file, Non-unique keys: repition in first column: 1")


#context("test-StoxBaselineFunctions: DefineCarNeighbours StratumPolygon")
car <- RstoxFDA::DefineCarNeighbours(NULL, DefinitionMethod = "StratumPolygon", StratumPolygon = RstoxFDA::mainareaFdir2018)

neighbours44 <- strsplit(car$Neighbours[car$CarValues=="44"], ",")[[1]]
expect_true(all(c("43", "45", "49") %in% neighbours44))
expect_equal(length(neighbours44), 3)
neighbours08 <- strsplit(car$Neighbours[car$CarValues=="08"], ",")[[1]]
expect_true(all(c("09", "28", "41", "42") %in% neighbours08))
expect_equal(length(neighbours08), 4)
neighbours62 <- strsplit(car$Neighbours[car$CarValues=="62"], ",")[[1]]
expect_true(all(c("26", "38", "35", "50", "56", "55", "54", "53", "61") %in% neighbours62))
expect_equal(length(neighbours62), 9)


#context("test-StoxBaselineFunctions: DefineAgeErrorMatrix")
ageerorfile <- system.file("testresources","AgeErrorHirstEtAl2012.txt", package="RstoxFDA")
ageerror <- RstoxFDA::DefineAgeErrorMatrix(FileName = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 15)
expect_equal(ncol(ageerror), 16)

#context("test-StoxBaselineFunctions: DefineAgeErrorMatrix useProcessData")
nullAE <- RstoxFDA::DefineAgeErrorMatrix(NULL, FileName = ageerorfile, UseProcessData = T)
expect_true(is.null(nullAE))

#context("test-StoxBaselineFunctions: DefineAgeErrorMatrix non-symmetric")
ageerorfile <- system.file("testresources","AgeNonSym.txt", package="RstoxFDA")
ageerror <- RstoxFDA::DefineAgeErrorMatrix(FileName = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 14)
expect_equal(ncol(ageerror), 16)

#context("test-StoxBaselineFunctions: DefineAgeErrorMatrix malformed")
ageerorfile <- system.file("testresources","AgeMalformed.txt", package="RstoxFDA")
expect_error(RstoxFDA::DefineAgeErrorMatrix(FileName = ageerorfile),"Malformed resource file. All probabilities must be in >=0 and <=1.")

ageerorfile <- system.file("testresources","AgeMalformed2.txt", package="RstoxFDA")
expect_error(RstoxFDA::DefineAgeErrorMatrix(FileName = ageerorfile),"Malformed resource file. Columns must sum to 1.")




#context("test-StoxBaselineFunctions: DefineStockSplittingParameters")
classerorfile <- system.file("testresources","classificationError.txt", package="RstoxFDA")
classerror <- RstoxFDA::DefineStockSplittingParameters(FileName = classerorfile)

expect_true(RstoxFDA::is.StockSplittingParameters(classerror))
expect_equal(nrow(classerror), 1)
expect_equal(ncol(classerror), 10)

#context("test-StoxBaselineFunctions: DefineClassificationError useProcessdata")
classNULL <- RstoxFDA::DefineStockSplittingParameters(NULL, FileName = classerorfile, UseProcessData = T)
expect_true(is.null(classNULL))

#context("test-StoxBaselineFunctions: DefineClassificationError functionparamters")
manual <- RstoxFDA::DefineStockSplittingParameters(DefinitionMethod = "FunctionParameters",
                               StockNameCC="S1", StockNameS="S2", ProbabilityType1As1=.8,
                               ProbabilityType1As5=.2, ProbabilityType2As2=.6,
                               ProbabilityType2As4=.4,	ProbabilityType4As2=.4,
                               ProbabilityType4As4=.6,	ProbabilityType5As1=.2,
                               ProbabilityType5As5=.8)
expect_true(RstoxFDA::is.StockSplittingParameters(manual))

#context("test-StoxBaselineFunctions: appendTemporal")
temp <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-10","01-12"))
tabExampleFile <- system.file("testresources","startStopExample.txt", package="RstoxFDA")

tabExamplePre <- RstoxFDA:::readTabSepFile(tabExampleFile, col_classes = c("character", "character", "character", "character", "POSIXct", "POSIXct"))
tabExamplePost <- RstoxFDA:::appendTemporal(tabExamplePre, "period", temp, datecolumns = c("startD", "stopD"))

expect_equal(tabExamplePost$period[1], "[01-12, 01-10>")
expect_equal(tabExamplePost$period[2], "[01-12, 01-10>")
expect_equal(tabExamplePost$period[3], "[01-10, 01-12>")

tabExamplePost <- RstoxFDA:::appendTemporal(tabExamplePre, "period", temp, datecolumns = c("stopD", "startD"))
expect_equal(tabExamplePost$period[1], "[01-10, 01-12>")
expect_equal(tabExamplePost$period[2], "[01-12, 01-10>")
expect_equal(tabExamplePost$period[3], "[01-10, 01-12>")

tabExampleMissing <- tabExamplePre
tabExampleMissing$stopD[2] <- NA
expect_error(RstoxFDA:::appendTemporal(tabExampleMissing, "period", temp, datecolumns = c("stopD", "startD")), "NA for some dates")

tempMisspec <- temp
tempMisspec$StartYear <- NA
tempMisspec$StartYear[1] <- 1993
expect_error(RstoxFDA:::appendTemporal(tabExamplePre, "period", tempMisspec, datecolumns = c("stopD", "startD")), "Year is provided for some, but not all temporal definitions.")

tempYearspec <- temp
temp$StartYear <- 2019
expect_error(RstoxFDA:::appendTemporal(tabExamplePre, "period", temp, datecolumns = c("stopD", "startD")), "Some dates preced the first temporal category.")

tabMonth  <- tabExamplePre

#test using month
monthCat <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Month")
tabMonthPost <- RstoxFDA:::appendTemporal(tabMonth, "period", monthCat, datecolumns = c("stopD", "startD"))
expect_equal(tabMonthPost$period[1], "October")
tabMonthPost <- RstoxFDA:::appendTemporal(tabMonth, "period", monthCat, datecolumns = c("startD", "stopD"))
expect_equal(tabMonthPost$period[1], "September")

#test using quarter
monthCat <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Quarter")
tabMonthPost <- RstoxFDA:::appendTemporal(tabMonth, "period", monthCat, datecolumns = c("stopD", "startD"))
expect_equal(tabMonthPost$period, c("Q4", "Q1", "Q4"))

tabMultiYear <- tabExamplePre
my <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Custom", CustomPeriods = c("01-10-2019","01-12-2019"))
tabMultiYear$stopD[2] <- as.Date("2020-10-01")
expect_error(RstoxFDA:::appendTemporal(tabMultiYear, "period", my, datecolumns = c("stopD", "startD")),"Year is provided in temporal definitions, but does not contain definitions for all years in data.")

#context("test-StoxBaselineFunctions: AddPeriodStoxBiotic")
stoxbiotic <- readRDS(system.file("testresources","StoxBioticData.rds", package="RstoxFDA"))
quart <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Quarter")
stoxbioticPost <- RstoxFDA::AddPeriodStoxBiotic(stoxbiotic, quart)
expect_true("Period" %in% names(stoxbioticPost$Station))
expect_true(all(c("Q1", "Q2") %in% stoxbioticPost$Station$Period))

#context("test-StoxBaselineFunctions: AddPeriodStoxLanding")
stoxlanding <- readRDS(system.file("testresources","StoxLandingData.rds", package="RstoxFDA"))
quart <- RstoxFDA::DefinePeriod(NULL, TemporalCategory = "Quarter")
stoxlandingPost <- RstoxFDA::AddPeriodStoxLanding(stoxlanding, quart)
expect_true("Period" %in% names(stoxlandingPost$Landing))
expect_true(all(c("Q1", "Q2") %in% stoxlandingPost$Landing$Period))

stoxlandingPost <- RstoxFDA::AddPeriodStoxLanding(stoxlanding, quart, ColumnName = "ReportPeriod")
expect_true("ReportPeriod" %in% names(stoxlandingPost$Landing))
expect_true(all(c("Q1", "Q2") %in% stoxlandingPost$Landing$ReportPeriod))

#context("test-StoxBaselineFunctions: SetAreaPositionsBiotic")
areaPos <- RstoxFDA::DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
bioticfiles <- system.file("testresources","biotic_v3_example.xml", package="RstoxFDA")
BioticData <- RstoxData::ReadBiotic(bioticfiles)
BioticData$biotic_v3_example.xml$fishstation$area <- c("03", "02")
expect_error(RstoxFDA::SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="location", System="2", Overwrite = T), "Not all areas and locations in 'BioticData' are defined in 'AreaPosition. Missing: 03-35,02-27")
BioticDataPost <- RstoxFDA::SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="None", System="2", Overwrite = T)
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)>1))

#test when nothing needs writing
BioticDataPost <- RstoxFDA::SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="None", System="2")
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)<1e-10))
BioticDataPost <- RstoxFDA::SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="location", System="2")
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)<1e-10))

#test with location
BioticData$biotic_v3_example.xml$fishstation$location <- c("22", "08")
BioticDataPost <- RstoxFDA::SetAreaPositionsBiotic(BioticData, areaPos, LocationVariable="location", System="2", Overwrite = T)
expect_true(all(abs(BioticDataPost$biotic_v3_example.xml$fishstation$latitudestart - BioticData$biotic_v3_example.xml$fishstation$latitudestart)>1))


#context("test-StoxBaselineFunctions: AppendPositionLanding missing")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_incl.txt", package="RstoxFDA")
areaPos <- RstoxFDA::DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
expect_error(RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos, LocationVariable = "Location"))
expect_error(RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos, LocationVariable = NULL), "Argument 'LocationVariable' must be provided.")
expect_error(RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos))

#context("test-StoxBaselineFunctions: AppendPositionLanding regular run")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_compl.txt", package="RstoxFDA")
areaPos <- RstoxFDA::DefineAreaPosition(NULL, FileName = regularfile, StratumPolygon = NULL)
landingPost <- RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos)
expect_true(all(c("Latitude", "Longitude") %in% names(landingPost$Landing)))
expect_true(all(!is.na(landingPost$Landing$Latitude)))
expect_true(all(!is.na(landingPost$Landing$Longitude)))

lata <- min(landingPost$Landing$Latitude[1])
landingPost <- RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos, LocationVariable = "Location")
expect_false(lata == min(landingPost$Landing$Latitude[1]))

#context("test-StoxBaselineFunctions: AppendPositionLanding used colName")
stoxLandingPre <- RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos)
expect_error(RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos), "Column Latitude already exists.")





#context("test-StoxBaselineFunctions: AppendStratumStoxLanding")

strp <- RstoxFDA::mainareaFdir2018
#sp::proj4string(strp) <- sp::CRS("+proj=longlat +datum=WGS84")

areafile <- system.file("testresources","mainarea_fdir_from_2018_compl.txt", package="RstoxFDA")
areaPos <- RstoxFDA::DefineAreaPosition(NULL, FileName = areafile, StratumPolygon = NULL)

landingH <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxFDA"))
stoxLandingPre <- RstoxData:::StoxLanding(landingH)
landingWpos <- RstoxFDA::AddAreaPositionStoxLanding(stoxLandingPre, areaPos)

landingPost <- RstoxFDA::AddStratumStoxLanding(landingWpos, strp)
expect_true(all(as.integer(landingPost$Landing$Stratum)==as.integer(landingPost$area)))
expect_equal(ncol(landingWpos$Landing)+1, ncol(landingPost$Landing))

expect_warning(landingPost <- RstoxFDA::AddStratumStoxLanding(landingWpos, strp, ColumnName = "Area"))
expect_true(all(as.integer(landingPost$Area)==as.integer(landingPost$area)))
expect_equal(ncol(landingWpos$Landing), ncol(landingPost$Landing))


#context("test-StoxBaselineFunctions: AppendStratumStoxBiotic")
strp <- RstoxFDA::mainareaFdir2018
bioticfiles <- system.file("testresources","biotic_v3_example.xml", package="RstoxFDA")
BioticData <- RstoxData::ReadBiotic(bioticfiles)
BioticData$biotic_v3_example.xml$individual$individualproducttype <- 1
BioticData$biotic_v3_example.xml$catchsample$lengthmeasurement <- "E"
BioticData$biotic_v3_example.xml$catchsample$sampleproducttype <- 1
BioticData$biotic_v3_example.xml$catchsample$catchproducttype <- 1
StoxBioticPre <- RstoxData::StoxBiotic(BioticData)
StoxBioticPost <- RstoxFDA::AddStratumStoxBiotic(StoxBioticPre, strp)
expect_true("Stratum" %in% names(StoxBioticPost$Station))
expect_true(!any(is.na(StoxBioticPost$Station$Stratum)))
expect_equal(ncol(StoxBioticPost$Station), ncol(StoxBioticPost$Station))
