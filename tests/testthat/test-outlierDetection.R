context("Test von B outlier detection")

bioticfile <- system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA")
nmdbiotic <- RstoxData::ReadBiotic(bioticfile)
StoxBiotic <- RstoxData::StoxBiotic(nmdbiotic)
indCod <- StoxBiotic$Individual[StoxBiotic$Individual$SpeciesCategoryKey=="torsk/164712/126436/NA",]

# check that common default of K=4 retains all data
s<-filterVonBsigma(indCod,
            Linf = 232.98028344, K=0.05284384, sigma=0.16180306, kAl=4)
expect_equal(nrow(s), nrow(indCod))

s<-filterVonBsigma(indCod,
                   Linf = 232.98028344, K=0.05284384, sigma=0.16180306, kAl=1)
expect_true(nrow(s) < nrow(indCod))

#check that stricter upper gives lower mean length by age
s <-filterVonBsigma(indCod,
                   Linf = 232.98028344, K=0.05284384, sigma=0.16180306, kAl=4, kAu = 1)

sMeanByAge <- s[,list(meanLength=mean(get("IndividualTotalLength"), na.rm=T)), by=list(age=get("IndividualAge"))]
indCodMeanByAge <- indCod[,list(meanLength=mean(get("IndividualTotalLength"), na.rm=T)), by=list(age=get("IndividualAge"))]
comb <- merge(sMeanByAge, indCodMeanByAge, by="age", suffix=c(".s", ".ind"))
expect_true(all(comb$meanLength.s <= comb$meanLength.ind))
expect_true(any(comb$meanLength.s < comb$meanLength.ind))

#check that stricter lower gives higher mean length by age
s <-filterVonBsigma(indCod,
                    Linf = 232.98028344, K=0.05284384, sigma=0.16180306, kAl=1, kAu = 4)

sMeanByAge <- s[,list(meanLength=mean(get("IndividualTotalLength"), na.rm=T)), by=list(age=get("IndividualAge"))]
indCodMeanByAge <- indCod[,list(meanLength=mean(get("IndividualTotalLength"), na.rm=T)), by=list(age=get("IndividualAge"))]
comb <- merge(sMeanByAge, indCodMeanByAge, by="age", suffix=c(".s", ".ind"))
expect_true(all(comb$meanLength.s >= comb$meanLength.ind))
expect_true(any(comb$meanLength.s > comb$meanLength.ind))

#
# Cant find documentation for the parameters from eca 3.x paramter files
# but seems to make more sense with weights in g and lengths in cm.
# From SPD-handbook 2010 this also seems to be the common way to record individual measurements of fish
#
# check that common default of K=4 retains all data
s <- filterLogLinear(indCod,
                     logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl = 4)
expect_equal(nrow(s), nrow(indCod))

s <- filterLogLinear(indCod,
                     logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl = 1)
expect_true(nrow(s) < nrow(indCod))

#check that mean weight/length ratio drops when upper criterion is stricter
s <- filterLogLinear(indCod,
                     logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl = 4, kAu = 1)
expect_true(nrow(s) < nrow(indCod))
expect_true(mean(s$IndividualRoundWeight / s$IndividualTotalLength, na.rm=T) < mean(indCod$IndividualRoundWeight / indCod$IndividualTotalLength, na.rm=T))

#check that mean weight/length ratio increases when lower criterion is stricter
s <- filterLogLinear(indCod,
                     logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl = .01, kAu = 4)
expect_true(nrow(s) < nrow(indCod))
expect_true(mean(s$IndividualRoundWeight / s$IndividualTotalLength, na.rm=T) > mean(indCod$IndividualRoundWeight / indCod$IndividualTotalLength, na.rm=T))

#test that NAs are removed
s <- filterLogLinearMask(indCod,
                     logalfa = -5.0061, beta = 3.0716, sigma = 0.1454, kAl = 1)
expect_true(sum(is.na(s)))
