#context("metierannotation parse table")
regularfile <- system.file("testresources","metiermapping_example.txt", package="RstoxFDA")
metiertable <- RstoxFDA::readMetierTable(regularfile)
expect_true(!any(is.na(metiertable$metier)))
expect_equal(sum(is.na(metiertable$gearcode)), 1)
expect_equal(sum(is.na(metiertable[1,])), 3)
expect_equal(sum(is.na(metiertable[6,])), 6)
expect_equal(metiertable[7,"gearcode"][[1]], "01")

simplefile <- system.file("testresources","metiermapping_example_simple.txt", package="RstoxFDA")
metiertable <- RstoxFDA::readMetierTable(simplefile)
expect_true(!any(is.na(metiertable$metier)))
expect_true(!any(is.na(metiertable$gearcode)))
expect_equal(sum(is.na(metiertable[1,])), 8)

commafile <- system.file("testresources","metiermapping_example_commanot.txt", package="RstoxFDA")
metiertable <- RstoxFDA::readMetierTable(commafile)
expect_equal(nrow(metiertable), 2)

commafile <- system.file("testresources","metiermapping_example_commanot_target.txt", package="RstoxFDA")
metiertable <- RstoxFDA::readMetierTable(commafile)
expect_equal(nrow(metiertable), 4)


errorfile <- system.file("testresources","metiermapping_example_error.txt", package="RstoxFDA")
expect_error(RstoxFDA::readMetierTable(errorfile), "Some column names are not recognized: area")


#context("Test variant used in intercatch export")
icfile <- system.file("testresources","metiertable_unmeshed.txt", package="RstoxFDA")
expect_error(RstoxFDA::readMetierTable(icfile))


#context("Test annotation")

testdata <- data.table::data.table(id=seq(1,6), gear=as.character(c(51,51,22,22,35,35)),
                                   meshSize=c(90,90,90,120,NA,NA),
                                   selDev=c("G","G",NA,NA,NA,NA),
                                   selDevMS=c(110,90,NA,NA,NA,NA),
                                   area=as.character(c("08","08","09","09","09","10")))

regularfile <- system.file("testresources","metiermapping_example.txt", package="RstoxFDA")
metiertable <- RstoxFDA::readMetierTable(regularfile)

annotated <- RstoxFDA::appendMetier(testdata, metiertable, "gear", meshSizeColumn = "meshSize", selectivityDeviceColumn = "selDev", selectivityDeviceMeshSizeColumn = "selDevMS")
expect_equal(ncol(annotated), ncol(testdata) + 1)
expect_equal(nrow(annotated), nrow(testdata))
expect_true("metier" %in% names(annotated))
expect_equal(annotated$metier[1], "OTB_DEF_0_120_1_101-120")
expect_equal(annotated$metier[2], "OTB_DEF_0_120_1_80-100")

#context("metier Annotation with filename definitions provided as filename")
annotated <- RstoxFDA::appendMetier(testdata, regularfile, "gear", meshSizeColumn = "meshSize", selectivityDeviceColumn = "selDev", selectivityDeviceMeshSizeColumn = "selDevMS")
expect_equal(annotated$metier[1], "OTB_DEF_0_120_1_101-120")


#context("metier Annotation with NA in data")
testdata <- data.table::data.table(id=seq(1,6), gear=as.character(c(NA,51,22,22,35,35)),
                                   meshSize=c(90,90,90,120,NA,NA),
                                   selDev=c(NA,"G",NA,NA,NA,NA),
                                   selDevMS=c(110,90,NA,NA,NA,NA),
                                   area=as.character(c("08","08","09","09","09","10")))
regularfile <- system.file("testresources","metiermapping_example.txt", package="RstoxFDA")
metiertable <- RstoxFDA::readMetierTable(regularfile)

annotated <- RstoxFDA::appendMetier(testdata, metiertable, "gear", meshSizeColumn = "meshSize", selectivityDeviceColumn = "selDev", selectivityDeviceMeshSizeColumn = "selDevMS")
expect_equal(annotated$metier[1], "MIS_MIS")



#context("Annotation error checks")
expect_error(RstoxFDA::appendMetier(testdata, metiertable, "gear"))

metiertableMissingConflict <- metiertable
metiertableMissingConflict[1,] <-metiertableMissingConflict[2,]
metiertableMissingConflict$metier[1] <- "MESS"
expect_error(RstoxFDA:::checkMetierTable(metiertableMissingConflict, F,T,T,T), "The provided metier table cannot be used with this selection of data columns. Some metiers have conflicting definitions: 51:TRUE:0:120:G:TRUE:80:100")

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedGear[2] <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'meshedGear' has a value for some gears, but not all")
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
metiertableMissingMS$meshedGear <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T))
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
metiertableMissingMS$lowerMeshSize <- NA
metiertableMissingMS$upperMeshSize <- NA
expect_true(RstoxFDA:::is.MetierTable(metiertableMissingMS))

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedGear[2] <- F
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T)) #mesh size provided for unmeshed gears

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedSelectivityDevice[2] <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'meshedSelectivityDevice' has a value for some gears, but not all")
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS, F))
metiertableMissingMS$meshedSelectivityDevice <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T))
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS, F))
metiertableMissingMS$selDevLowerMeshSize <- NA
metiertableMissingMS$selDevUpperMeshSize <- NA
expect_true(RstoxFDA:::is.MetierTable(metiertableMissingMS))

metiertableMissingMS <- metiertable
metiertableMissingMS$lowerMeshSize <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T))
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS, F))
metiertableMissingMS <- metiertable
metiertableMissingMS$upperMeshSize <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T))
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS, F))
metiertableMissingMS <- metiertable
metiertableMissingMS$selDevLowerMeshSize <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T))
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))

metiertableMissingMS <- metiertable
metiertableMissingMS$selectivityDevice[2] <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "Mesh sizes provided for selectivity devices where 'selectivityDevice' is not given")

metiertableMissingMS <- metiertable
metiertableMissingMS$gearcode[2] <- NA
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "Mesh sizes provided for gear where 'gear' is not given")


metiertableMissingMS <- metiertable
metiertableMissingMS$selDevUpperMeshSize[2] <- 101
expect_error(RstoxFDA:::checkMetierTable(metiertableMissingMS, meshSize = T, selDevMeshSize = T), "Mesh sizes have overlapping ranges for gear 51/G")

metiertableMissingMS <- metiertable
metiertableMissingMS$upperMeshSize[4] <- 101
expect_error(RstoxFDA:::checkMetierTable(metiertableMissingMS, meshSize = T, selDevMeshSize = T), "Mesh sizes have overlapping ranges for gear 22")

metiertableMissingMS <- metiertable
metiertableMissingMS$gearcode <- NULL
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "Metiertable does not have the required columns")

metiertableMissingMS <- metiertable
metiertableMissingMS <- as.data.frame(metiertableMissingMS)
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "A metiertable must be a data.table")


metiertableMissingMS <- metiertable
metiertableMissingMS$metier <- 1
metiertableMissingMS$metier <- as.integer(metiertableMissingMS$metier)
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'metier' must be a character")

metiertableMissingMS <- metiertable
metiertableMissingMS$gearcode <- 1
metiertableMissingMS$gearcode <- as.integer(metiertableMissingMS$gearcode)
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'gearcode' must be a character")

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedGear <- NULL
metiertableMissingMS$meshedGear <- as.integer(metiertableMissingMS$gearcode)
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'meshedGear' must be a logical")

metiertableMissingMS <- metiertable
metiertableMissingMS$lowerMeshSize <- NULL
metiertableMissingMS$lowerMeshSize <- "ss"
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'lowerMeshSize' must be an integer")

metiertableMissingMS <- metiertable
metiertableMissingMS$upperMeshSize <- NULL
metiertableMissingMS$upperMeshSize <- "ss"
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'upperMeshSize' must be an integer")

metiertableMissingMS <- metiertable
metiertableMissingMS$selectivityDevice <- 1
metiertableMissingMS$selectivityDevice <- as.integer(metiertableMissingMS$gearcode)
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'selectivityDevice' must be a character")

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedSelectivityDevice <- NULL
metiertableMissingMS$meshedSelectivityDevice <- as.integer(metiertableMissingMS$gearcode)
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'meshedSelectivityDevice' must be a logical")

metiertableMissingMS <- metiertable
metiertableMissingMS$selDevLowerMeshSize <- NULL
metiertableMissingMS$selDevLowerMeshSize <- "ss"
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'selDevLowerMeshSize' must be an integer")

metiertableMissingMS <- metiertable
metiertableMissingMS$selDevUpperMeshSize <- NULL
metiertableMissingMS$selDevUpperMeshSize <- "ss"
expect_false(RstoxFDA:::is.MetierTable(metiertableMissingMS))
expect_error(RstoxFDA:::is.MetierTable(metiertableMissingMS, T), "The column 'selDevUpperMeshSize' must be an integer")


