context("Test bioticdiff")

bioticfile <- system.file("testresources", "biotic_v3_example.xml", package="RstoxFDA")
nmdbiotic <- RstoxData::ReadBiotic(bioticfile)
StoxBiotic <- RstoxData::StoxBiotic(nmdbiotic)


diff <- bioticDiff(nmdbiotic, StoxBiotic)
expect_equal(nrow(diff$mission), 0)
expect_equal(nrow(diff$fishstation), 0)
expect_equal(nrow(diff$catchsample), 0)
expect_equal(nrow(diff$individual), 0)

StoxBioticStation <- StoxBiotic
StoxBioticStation$Haul <- StoxBioticStation$Haul[2,]
diff <- bioticDiff(nmdbiotic, StoxBioticStation)
expect_equal(nrow(diff$mission), 0)
expect_equal(nrow(diff$fishstation), 1)
expect_equal(nrow(diff$catchsample), 0)
expect_equal(nrow(diff$individual), 0)

StoxBioticCatchsample <- StoxBiotic
StoxBioticCatchsample$Sample <- StoxBioticCatchsample$Sample[c(1,3,5),]
diff <- bioticDiff(nmdbiotic, StoxBioticCatchsample)
expect_equal(nrow(diff$mission), 0)
expect_equal(nrow(diff$fishstation), 0)
expect_equal(nrow(diff$catchsample), nrow(StoxBiotic$Sample)-3)
expect_equal(nrow(diff$individual), 0)

StoxBioticIndividual <- StoxBiotic
StoxBioticIndividual$Individual <- StoxBioticIndividual$Individual[5:25,]
diff <- bioticDiff(nmdbiotic, StoxBioticIndividual)
expect_equal(nrow(diff$mission), 0)
expect_equal(nrow(diff$fishstation), 0)
expect_equal(nrow(diff$catchsample), 0)
expect_equal(nrow(diff$individual), nrow(StoxBiotic$Individual)-21)

