# ECA tests are only run if Reca is installed.

if (nchar(system.file(package="Reca"))>0){
  
fishdata <- data.table::as.data.table(readRDS(system.file(package = "RstoxFDA", "testresources", "fishdata.rda")))
landings <- data.table::as.data.table(readRDS(system.file(package = "RstoxFDA", "testresources", "landings.rda")))

nFish <- fishdata[1000:nrow(fishdata),c("sampleId", "SAtotalWtLive", "Weight")]
nFish$count <- nFish$SAtotalWtLive/mean(nFish$Weight, na.rm=T)
nFish$Weight <- NULL
nFish$SAtotalWtLive <- NULL
nFish <- unique(nFish)

nFishAll <- fishdata[,c("sampleId", "SAtotalWtLive", "Weight")]
nFishAll$count <- nFishAll$SAtotalWtLive/mean(nFishAll$Weight, na.rm=T)
nFishAll$Weight <- NULL
nFishAll$SAtotalWtLive <- NULL
nFishAll <- unique(nFishAll)


#context("test prepRECA: minimal run")

minRobj <- RstoxFDA:::prepRECA(fishdata[1:1000,], landings, NULL, NULL, NULL, month=landings$Month)
expect_true("constant" %in% names(minRobj$AgeLength$CovariateMatrix))
expect_true("constant" %in% names(minRobj$Landings$AgeLengthCov))
expect_true("constant" %in% names(minRobj$WeightLength$CovariateMatrix))
expect_true("constant" %in% names(minRobj$Landings$WeightLengthCov))
expect_equal(max(minRobj$AgeLength$DataMatrix$samplingID), nrow(minRobj$AgeLength$CovariateMatrix))
expect_equal(max(minRobj$WeightLength$DataMatrix$samplingID), nrow(minRobj$WeightLength$CovariateMatrix))
expect_error(RstoxFDA:::prepRECA(fishdata[1:1000,], landings, c("Metier5"), c("vessel"), NULL, month=landings$Month)) #fixed effect issue
expect_error(RstoxFDA:::prepRECA(fishdata[1:1000,], landings, c("Metier5", "quarter"), c("vessel"), NULL, month=landings$Month)) #fixed effect issue

#check with sampled cells not in landings
stopifnot("Q2" %in% fishdata[1:1000,]$quarter)
expect_error(RstoxFDA:::prepRECA(fishdata[1:1000,], landings[landings$Quarter < 2,], c("quarter"), c("vessel"), NULL, month=landings[landings$Quarter < 2,][["Month"]]))

#check that nFish is sanitized
nf <- nFishAll
nf$extraColumn <- nf$sampleId
expect_error(RstoxFDA:::prepRECA(fishdata, landings, NULL, NULL, NULL, month=landings$Month, nFish = nf), "The parameter nFish must have exactly two columns: 'sampleId' and 'count'")

minRobj <- RstoxFDA:::prepRECA(fishdata, landings, NULL, NULL, NULL, month=landings$Month, nFish = nFishAll)
expect_equal(max(minRobj$AgeLength$DataMatrix$samplingID), nrow(minRobj$AgeLength$CovariateMatrix))
expect_equal(max(minRobj$WeightLength$DataMatrix$samplingID), nrow(minRobj$WeightLength$CovariateMatrix))
expect_true(all(!is.na(minRobj$AgeLength$DataMatrix$partcount)))
expect_true(all(!is.na(minRobj$WeightLength$DataMatrix$partcount)))

minRobj <- RstoxFDA:::prepRECA(fishdata, landings, NULL, c("Metier5"), NULL, month=landings$Month, nFish = nFishAll)
expect_equal(max(minRobj$AgeLength$DataMatrix$samplingID), nrow(minRobj$AgeLength$CovariateMatrix))
expect_equal(max(minRobj$WeightLength$DataMatrix$samplingID), nrow(minRobj$WeightLength$CovariateMatrix))
expect_true(all(!is.na(minRobj$AgeLength$DataMatrix$partcount)))
expect_true(all(!is.na(minRobj$WeightLength$DataMatrix$partcount)))

#context("test prepRECA: missing column random effect")
expect_error(RstoxFDA:::prepRECA(fishdata, landings, NULL, c("gear"), NULL, month=landings$Month))

#context("test prepRECA: missing column fixed effect")
expect_error(RstoxFDA:::prepRECA(fishdata, landings, c("gear"), NULL, NULL, month=landings$Month))

#context("test rEcaDataReport: minimal run")
RstoxFDA:::rEcaDataReport(fishdata, landings, c("Metier5", "vessel"))

#context("test rEcaDataReport: no covariates")
expect_error(RstoxFDA:::rEcaDataReport(fsmin, lmin))

#context("tets getCovariateMap: simple run")
map <- RstoxFDA:::getCovariateMap(c("Metier5"), fishdata, landings)
expect_equal(length(map), length(unique(c(fishdata$Metier5, landings$Metier5))))
expect_true(map[[1]] %in% landings$Metier5)

#context("tets getCovariateMap: not in sample")
expect_error(RstoxFDA:::getCovariateMap(c("non"), fishdata, landings), "Covariate non not in samples")

#context("tets getInfoMatrix: simple run")
infom <- RstoxFDA:::getInfoMatrix(fishdata, landings, c("Metier5"), c("vessel"), NULL)
expect_equal(nrow(infom), 3)
expect_true(all(c("constant", "Metier5", "vessel") %in% rownames(infom)))
expect_true(all(c("random", "CAR", "nlev") %in% colnames(infom)))

expect_error(RstoxFDA:::getInfoMatrix(fishdata, landings, c("Metier5"), c("Metier5"), NULL), "some effects are specified more than once")

#context("tets getInfoMatrix: interaction")
infom <- RstoxFDA:::getInfoMatrix(fishdata, landings, c("Metier5"), c("vessel"), NULL, c("Metier5"))
expect_equal(infom["Metier5", "interaction"], 1)
expect_error(RstoxFDA:::getInfoMatrix(fishdata, landings, c("Metier5"), c("vessel"), NULL, c("vessel")), "Effect vessel is specified in interaction, but is not found in landings")
expect_error(RstoxFDA:::getInfoMatrix(fishdata, landings, c("Metier5"), c("vessel"), NULL, c("v")), "The effects specified in 'interaction' must be provided as either fixedEffects, randomEffects, or carEffect")
expect_error(RstoxFDA:::getInfoMatrix(fishdata, landings, c("Metier5"), c("OSid"), NULL, c("OSid")), "Effect OSid is specified in interaction, but is not found in landings")

#context("tets getDataMatrixAgeLength: simple run")
dmAgeLength <- RstoxFDA:::getDataMatrixAgeLength(fishdata[1:10,], NULL)
expect_true(all(dmAgeLength$DataMatrix$part.year > 0))
expect_true(all(dmAgeLength$DataMatrix$part.year <= 1))
expect_equal(max(dmAgeLength$DataMatrix$samplingID), length(unique(fishdata[1:10,"catchId"])))


# test getDataMatrixAgeLength: leap-year
fd <- fishdata[1:10,]
fd$date <- rep(as.POSIXct("2020-01-01"), nrow(fd))
fd$date[1] <- as.POSIXct("2020-12-31 CET")

dmAgeLength <- RstoxFDA:::getDataMatrixAgeLength(fd, NULL)
expect_equal(dmAgeLength$DataMatrix$part.year[1], 1)
expect_equal(dmAgeLength$DataMatrix$part.year[2], 1/366)
expect_true(all(dmAgeLength$DataMatrix$part.year > 0))
expect_true(all(dmAgeLength$DataMatrix$part.year <= 1))
expect_equal(max(dmAgeLength$DataMatrix$samplingID), length(unique(fishdata[1:10,"catchId"])))

#context("tets getDataMatrixAgeLength: nFish error")
expect_error(RstoxFDA:::getDataMatrixAgeLength(fishdata, NULL)) #delprøve on some sample
nfe <- nFishAll
nfe[1,"count"] <- NA
expect_error(RstoxFDA:::prepRECA(fishdata[1:1000], landings, NULL, NULL, NULL, month=landings$Month, nFish=nfe))
nfe <- nFishAll
names(nfe)[2] <- "counts"
expect_error(RstoxFDA:::prepRECA(fishdata[1:1000], landings, NULL, NULL, NULL, month=landings$Month, nFish=nfe))

#context("tets getDataMatrixWeightLength: simple run")
dmWeightLength <- RstoxFDA:::getDataMatrixWeightLength(fishdata[1:10,], NULL)
expect_equal(max(dmWeightLength$DataMatrix$samplingID), length(unique(fishdata[1:10,"catchId"])))

#context("tets getDataMatrixWeightLength: nFish error")
expect_error(RstoxFDA:::getDataMatrixWeightLength(fishdata, NULL)) #delprøve on some sample

#context("tets getDataMatrixWeightLength: with nFish")
expect_silent(dm <- RstoxFDA:::getDataMatrixWeightLength(fishdata, nFish))
expect_equal(nrow(dm$DataMatrix), nrow(fishdata))
expect_true(sum(!is.na(dm$DataMatrix$partcount))> 0)
expect_true(sum(is.na(dm$DataMatrix$partcount))> 0)

#context("tets CovariateMatrix: simple run")
cv <- RstoxFDA:::getCovariateMatrix(fishdata, c(), NULL, NULL)
expect_equal(nrow(cv), length(unique(fishdata$catchId)))
expect_equal(ncol(cv),1)

#context("tets getCovariateMatrix: one covariate")
covariateMaps <- list()
covariateMaps[["vessel"]] <- RstoxFDA:::getCovariateMap("vessel", fishdata, landings)
cv <- RstoxFDA:::getCovariateMatrix(fishdata, c("vessel"), NULL, covariateMaps)
expect_equal(nrow(cv), length(unique(fishdata$catchId)))
expect_equal(ncol(cv),2)
expect_true(all(c("vessel", "constant") %in% names(cv)))


#context("tets getLandings: one covariate")
covariateMaps[["Metier5"]] <- RstoxFDA:::getCovariateMap("Metier5", fishdata, landings)
land <- RstoxFDA:::getLandings(landings, c("Metier5"), covariateMaps, month=landings$Month)
expect_equal(nrow(land$AgeLengthCov), length(land$LiveWeightKG))
expect_equal(nrow(land$WeightLengthCov), length(land$LiveWeightKG))
expect_equal(length(unique(land$AgeLengthCov$Metier5)), length(unique(landings$Metier5)))
expect_equal(max(land$AgeLengthCov$Metier5), length(unique(landings$Metier5)))
expect_true(all(c("Metier5", "midseason") %in% names(land$AgeLengthCov)))
expect_true(all(c("Metier5", "midseason") %in% names(land$WeightLengthCov)))

expect_error(RstoxFDA:::getLandings(landings, c("Metier5"), covariateMaps), "date, month, and quarter can not all be NULL")
expect_error(RstoxFDA:::getLandings(landings, c("Metier5"), covariateMaps, month=landings$Month, quarter=landings$Quarter), "Several arguments for temporal resolution is provided. Provide either: date, month or quarter.")

#context("test getNeighbours: simple run")
covMap <- list()
covMap[[1]] <- "a"
covMap[[2]] <- "b"
covMap[[3]] <- "c"
neighbours <- list()
neighbours[["a"]] <- c("b","c")
neighbours[["b"]] <- c("a")
neighbours[["c"]] <- c("a")
neighboursECA <- RstoxFDA:::getNeighbours(neighbours, covMap)
expect_equal(neighboursECA$numNeighbours, c(2,1,1))
expect_equal(neighboursECA$idNeighbours, c(2,3,1,1))

expect_error(RstoxFDA:::getNeighbours(neighbours, NULL), "covariateMap is not provided")
expect_error(RstoxFDA:::getNeighbours(neighbours, covMap[1:2]), "length of neighbours does not match length of covariateMap")

#context("test prepRECA: CAR effect simple run")
carefftest <- fishdata[1:1000,]
carefftest$some <- NA
carefftestland <- landings
dummycareff <- data.table::as.data.table(unique(carefftest[,c("catchId", "some"), with=F]))
expect_true(data.table::is.data.table(dummycareff))
dummycareff$dummyArea <- c(rep(c("a", "b", "c"), as.integer(round(nrow(dummycareff)/3))), "a")
carefftest <- merge(carefftest, dummycareff, by="catchId")
carefftestland$dummyArea <- c(rep(c("a", "b", "c"), nrow(carefftestland)/3), "a", "a")
stopifnot("Age" %in% names(carefftest))
RECAobj <- RstoxFDA:::prepRECA(carefftest, carefftestland, NULL, c("Metier5", "vessel"), "dummyArea", neighbours = neighbours, month=landings$Month)
expect_equal(RECAobj$AgeLength$CARNeighbours$numNeighbours, c(2,1,1))
expect_equal(RECAobj$AgeLength$CARNeighbours$idNeighbours, c(2,3,1,1))
expect_true(all(RECAobj$AgeLength$CovariateMatrix$dummyArea %in% c(1,2,3)))

ll <- carefftestland
ll$LiveWeightKG <- NULL
expect_error(RstoxFDA:::prepRECA(carefftest, ll, NULL, c("Metier5", "vessel"), "dummyArea", neighbours = neighbours, month=landings$Month), "Column LiveWeightKG is mandatory in landings")

cf <- carefftest
cf$catchId <- NULL
expect_error(RstoxFDA:::prepRECA(cf, carefftestland, NULL, c("Metier5", "vessel"), "dummyArea", neighbours = neighbours, month=landings$Month), "Columns, catchId, sampleId, date, Age, Length, and Weight are mandatory in samples. Missing:  catchId")

cf <- carefftest
cf$vessel[1] <- NA
expect_error(RstoxFDA:::prepRECA(cf, carefftestland, NULL, c("Metier5", "vessel"), "dummyArea", neighbours = neighbours, month=landings$Month), "NAs are only allowed for weight and age in samples, not for covariates, date or length. Found NA for: vessel")

ll <- carefftestland
ll$LiveWeightKG[1] <- NA
expect_error(RstoxFDA:::prepRECA(carefftest, ll, NULL, c("Metier5", "vessel"), "dummyArea", neighbours = neighbours, month=landings$Month), "NAs in landings")


#context("test prepRECA: CAR effect errors")
expect_error(RstoxFDA:::prepRECA(carefftest, landings, NULL, c("Metier5", "vessel"), "dummyArea", neighbours = neighbours, month=landings$Month)) #CAR not in landings
expect_error(RstoxFDA:::prepRECA(carefftest, carefftestland, NULL, c("Metier5", "vessel"), NULL, neighbours = neighbours, month=landings$Month)) #neighbours with no CAR
expect_error(RstoxFDA:::prepRECA(carefftest, carefftestland, NULL, c("Metier5", "vessel"), "dummyArea", month=landings$Month)) #CAR with no neighbours

carefftestland$dummyArea[1] <- "dd"
expect_error(RstoxFDA:::prepRECA(carefftest, carefftestland, NULL, c("Metier5", "vessel"), "dummyArea", neighbours = neighbours, month=landings$Month), "Not all combinations of fixed effects are sampled together with CAR effect or neighbours for Age") #CAR not sampled


#context("test prepRECA: agebounds errors")
expect_error(RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2,3),], landings, NULL, c("Metier5", "vessel"), NULL, month=landings$Month, minAge=1, maxAge = 2), "Samples contains ages")
expect_error(RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2,3),], landings, NULL, c("Metier5", "vessel"), NULL, month=landings$Month, minAge=0, maxAge = 2), "Samples contains ages")
expect_error(RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2),], landings, NULL, c("Metier5", "vessel"), NULL, month=landings$Month, minAge=1, maxAge = 2, maxLength = 1), "Samples contains lengths longer than maxLength")


#context("test prepRECA: age error simple run")
ageErr <- matrix(c(.8,.2,.2,.8), nrow=2, dimnames=list(c(1,2), c(1,2)))
RECAobj <- RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2),], landings, NULL, c("Metier5", "vessel"), NULL, month=landings$Month, ageError = ageErr, minAge=1, maxAge = 2)
expect_equal(nrow(RECAobj$AgeLength$AgeErrorMatrix),2)

#context("test prepRECA: age error with matrix errors")
expect_error(RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2),], landings, NULL, NULL, NULL, month=landings$Month, ageError = list()), "ageError must be a matrix")
expect_error(RstoxFDA:::prepRECA(fishdata, landings, c("Metier5"), c("vessel"), NULL, month=landings$Month, ageError = ageErr)) #outside age range
ageErr <- matrix(c(.8,.2,.1,.8), nrow=2, dimnames=list(c(1,2), c(1,2)))
expect_error( RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2),], landings, c("Metier5"), c("vessel"), NULL, month=landings$Month, ageError = ageErr, minAge=1, maxAge = 2)) # ageError matrix does not sum to 1
ageErr <- matrix(c(.8,.2,.1,.7), nrow=2, dimnames=list(c(1,2), c(1,2)))
expect_error( RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2),], landings, NULL, NULL, NULL, month=landings$Month, ageError = ageErr, minAge=1, maxAge = 2), "Age error columns does not sum to 1.") # ageError matrix does not sum to 1
expect_error( RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2),], landings, NULL, NULL, NULL, month=landings$Month, ageError = ageErr, minAge=1, maxAge = 3), "Age error matrix must have entries for the entire age range estimated.") # ageError matrix does not sum to 1
ageErr <- matrix(c(.8,.2,.1,.8), nrow=2)
expect_error( RstoxFDA:::prepRECA(fishdata[is.na(fishdata$Age) | fishdata$Age %in% c(1,2),], landings, NULL, NULL, NULL, month=landings$Month, ageError = ageErr, minAge=1, maxAge = 2), "rownames and colnames must be set for ageError matrix")


#context("test prepRECA: no custom covariates")
recaObj <- RstoxFDA:::prepRECA(fishdata, landings, fixedEffects = NULL, randomEffects = NULL, NULL, minAge = 1, maxAge = 20, lengthResolution = 1, quarter = landings$Quarter, nFish = nFish)
expect_true(!is.null(recaObj$AgeLength$CovariateMatrix$constant))
expect_true(!is.null(recaObj$WeightLength$CovariateMatrix$constant))
expect_true("constant" %in% rownames(recaObj$AgeLength$info))
expect_true("constant" %in% rownames(recaObj$WeightLength$info))

}
