library(RstoxFDA)
library(data.table)
#
# prepares example activity census for metier annotation based on logbooks
#
prepMetier4 <- function(){
  metier4table <- data.table(metier=as.character( c("MIS","SX","PS","SB","PS","SB", "GN","GNS","GND", "LX","LLD","LL","LHM","LTL","LLS", "FIX","FYK","FPO","FYK","FPO","MIS", "TX","OTB","PTB","OTM","PTM","TBS","TBB","TBN","OTT","TX", "SSC", "HAR","HAR","HAR", "MIS","DRB","MIS","MIS", "MIS","MIS")),
                            gearcode=as.character(c(NA,   "10","11","12","14","15", "20","22", "21",  "30", "31","32", "33", "34","35",  "40", "41","42",  "43", "44", "45",  "50", "51", "52", "53","54", "55", "56", "57", "58", "59", "61",  "70", "71", "72",  "80","81","82","83",     "90", "99")),
                            target=as.character(NA),
                            meshedGear=as.logical(NA),
                            lowerMeshSize=as.numeric(NA),
                            upperMeshSize=as.numeric(NA),
                            selectivityDevice=as.character(NA),
                            meshedSelectivityDevice=as.logical(NA),
                            selDevLowerMeshSize=as.numeric(NA),
                            selDevUpperMeshSize=as.numeric(NA))
  RstoxFDA:::is.MetierTable(metier4table)
  RstoxFDA:::checkMetierTable(metier4table)
  usethis::use_data(metier4table, overwrite=T)
}
prepMetier4()

prepMetier5 <- function(){
  metier5table <- data.table(metier=as.character( c("MIS_MIS","LA_DEF","PS_SPF","GND_DEF","GNS_DEF","LLD_DEF","LLS_DEF","LHM_DEF","LTL_DEF","MIS_MIS","MIS_MIS","OTB_DEF","PTB_DEF","OTM_SPF","PTM_SPF","OTB_CRU","OTB_CRU","OTT_DEF","SSC_DEF","MIS_MIS","MIS_MIS")),
                             gearcode=as.character(c(NA,      "10",    "11",    "20",     "22",     "31",      "32",     "33",    "34",     "42",      "50",     "51",     "52",     "53",    "54",     "55",      "57",    "58",      "61",    "70",    "80")),
                             target=as.character(NA),
                             meshedGear=as.logical(NA),
                             lowerMeshSize=as.numeric(NA),
                             upperMeshSize=as.numeric(NA),
                             selectivityDevice=as.character(NA),
                             meshedSelectivityDevice=as.logical(NA),
                             selDevLowerMeshSize=as.numeric(NA),
                             selDevUpperMeshSize=as.numeric(NA))
  RstoxFDA:::is.MetierTable(metier5table)
  RstoxFDA:::checkMetierTable(metier5table)
  usethis::use_data(metier5table, overwrite=T)
}
prepMetier5()

prepMetier6 <- function(){
  metier6table <- RstoxFDA::readMetierTable(system.file("configFileExamples", "metiertable.txt", package="RstoxFDA"))
  RstoxFDA:::is.MetierTable(metier6table)
  RstoxFDA:::checkMetierTable(metier6table, meshSize = T)
  usethis::use_data(metier6table, overwrite=T)
}
prepMetier6()

prepGearGroupFdir <- function(){
  GearGroupFdirTable <- data.table(metier=as.character( c("MIS", rep("SEINE",6), rep("GILLNET",3), rep("HOOKS",6), rep("TRAPS",6), rep("TRAWL",10), "D.SEINE", rep("OTHER", 20), "AQUACULTURE", "MIS")),
                             gearcode=as.character(c(NA,         10:15,       20:22,         30:35,          40:45,        50:59,               61,        70:89,              90,       99)),
                             target=as.character(NA),
                             meshedGear=as.logical(NA),
                             lowerMeshSize=as.numeric(NA),
                             upperMeshSize=as.numeric(NA),
                             selectivityDevice=as.character(NA),
                             meshedSelectivityDevice=as.logical(NA),
                             selDevLowerMeshSize=as.numeric(NA),
                             selDevUpperMeshSize=as.numeric(NA))
  RstoxFDA:::is.MetierTable(GearGroupFdirTable)
  RstoxFDA:::checkMetierTable(GearGroupFdirTable)
  usethis::use_data(GearGroupFdirTable, overwrite=T)
}
prepGearGroupFdir()
