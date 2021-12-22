context("Test von B outlier detection")

bioticfile <- system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA")
nmdbiotic <- RstoxData::ReadBiotic(bioticfile)
StoxBiotic <- RstoxData::StoxBiotic(nmdbiotic)
indCod <- StoxBiotic$Individual[StoxBiotic$Individual$SpeciesCategoryKey=="torsk/164712/126436/NA",]

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

