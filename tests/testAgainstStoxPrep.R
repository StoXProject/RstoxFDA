library(RstoxFDA)
library(data.table)

#
# Prepare data based on StoXexport in "old" prepECA and compare results
#

stoxRobj <- readRDS(system.file(package = "RstoxFDA", "testresources", "oldstoxprepreca"))

samples <- data.table(catchId = as.character(stoxRobj$StoxExport$biotic$serialnumber),
                      sampleId = paste(stoxRobj$StoxExport$biotic$serialnumber, stoxRobj$StoxExport$biotic$catchpartnumber, sep="/"),
                      date = as.POSIXct(stoxRobj$StoxExport$biotic$stationstartdate),
                      Age = stoxRobj$StoxExport$biotic$age,
                      Length = stoxRobj$StoxExport$biotic$lengthcentimeter,
                      Weight = (stoxRobj$StoxExport$biotic$individualweightgram / 1000),
                      temporal = stoxRobj$StoxExport$biotic$temporal,
                      platformfactor = stoxRobj$StoxExport$biotic$platformfactor)

landings <- stoxRobj$StoxExport$landing
landings$LiveWeightKG <- landings$rundvekt
landings$Month <- substr(landings$sistefangstdato, 6,7)
landings$quarter <- NA
landings[landings$Month %in% c("01", "02", "03"), "quarter"] <- 1
landings[landings$Month %in% c("04", "05", "06"), "quarter"] <- 2
landings[landings$Month %in% c("07", "08", "09"), "quarter"] <- 3
landings[landings$Month %in% c("10", "11", "12"), "quarter"] <- 4

prepRecaRobj <- RstoxFDA::prepRECA(samples, landings, c("temporal"), c("platformfactor"), minAge = 1, maxAge = 20, quarter = landings$quarter)
prepRecaResults <- RstoxFDA::runRECA(prepRecaRobj, 100, 100, seed=42, thin=1)

stoxRobj$GlobalParameters$age.error = F
stoxRobj$GlobalParameters$CC = F
stoxRobj$GlobalParameters$CCerror = F
stoxRecaResults <- RstoxFDA::runRECA(stoxRobj, 100, 100, seed=42, thin=1)

tabPrep <- RstoxFDA::makeResultTableRECA(prepRecaResults$prediction)
tabStox <- RstoxFDA::makeResultTableRECA(stoxRecaResults$prediction)

#RstoxFDA::plotCatchAtAge(prepRecaResults$prediction, title="RecaPrep results")
#RstoxFDA::plotCatchAtAge(stoxRecaResults$prediction, title="StoxPrep results")
