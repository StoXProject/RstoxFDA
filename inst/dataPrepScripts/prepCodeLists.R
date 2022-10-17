#
# Prepare UN locodes
#

locodes <- data.table::fread("~/codelists/UN/LOCODES/data/code-list_csv.csv")
portcodes <- locodes[grepl("1", locodes$Function),]
portcodes$Name <- NULL
portcodes$Change <- NULL
portcodes$Date <- NULL

latWhole <- as.numeric(substr(portcodes$Coordinates,1,2))
latFrac <- as.numeric(substr(portcodes$Coordinates,3,4))/60
hist(latFrac)
latFrac[latFrac>1] <- NA
latDir <- rep(NA, length(latFrac))
latDir[substr(portcodes$Coordinates,5,5)=="N"] <- 1
latDir[substr(portcodes$Coordinates,5,5)=="S"] <- -1

lonWhole <- as.numeric(substr(portcodes$Coordinates,7,9))
lonFrac <- as.numeric(substr(portcodes$Coordinates,10,11))/60
hist(lonFrac)
lonFrac[lonFrac>1] <- NA
lonDir <- rep(NA, length(lonFrac))
lonDir[substr(portcodes$Coordinates,12,12)=="E"] <- 1
lonDir[substr(portcodes$Coordinates,12,12)=="W"] <- -1

portcodes$latitude <- (latWhole + latFrac) * latDir
portcodes$longitude <- (lonWhole + lonFrac) * lonDir

# some visual checks
nocodes <- portcodes[portcodes$Country=="NO" & !is.na(portcodes$latitude),]
RstoxFDA::plotArea(nocodes, "latitude", "longitude", areaDef=RstoxFDA::mainareaFdir2017)

sacodes <- portcodes[portcodes$Country=="SA" & !is.na(portcodes$latitude),]
RstoxFDA::plotArea(sacodes, "latitude", "longitude", areaDef=RstoxFDA::mainareaFdir2017)

rucodes <- portcodes[portcodes$Country=="RU" & !is.na(portcodes$longitude) & !is.na(portcodes$latitude),]
RstoxFDA::plotArea(rucodes, "latitude", "longitude", areaDef=RstoxFDA::NAFOareas)

portcodes2020 <- portcodes
portcodes2020$NameWoDiacritics[portcodes2020$Country=="CZ" & portcodes$Location=="KAD"] <- "Kadan"
portcodes2020$NameWoDiacritics[portcodes2020$Country=="HR" & portcodes$Location=="MET"] <- "Metkovic"
portcodes2020$NameWoDiacritics[portcodes2020$Country=="TR" & portcodes$Location=="SRS"] <- "Sar?seki"
portcodes2020$NameWoDiacritics[portcodes2020$Country=="TR" & portcodes$Location=="IZM"] <- "Izmir"
portcodes2020$NameWoDiacritics[portcodes2020$Country=="VN" & portcodes$Location=="VAG"] <- "Vung Ang"
portcodes2020$Remarks[portcodes2020$Country=="ES" & portcodes$Location=="SOL"] <- "Puerto de Soller"
usethis::use_data(portcodes2020, overwrite = T)