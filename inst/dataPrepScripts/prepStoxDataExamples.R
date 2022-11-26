#
# Prepares example data for a StoX-Reca workflow.
#

project <- "~/workspace/stox3examples/saithe2021/"
bl <- RstoxFramework::runModel(project, "baseline")
al <- RstoxFramework::runModel(project, "analysis")

StoxBioticDataExample <- bl$samples_FilterPBsamplingFrame
StoxLandingDataExample <- bl$landings_FilterPBsamplingFrame
RecaCatchAtAgeExample <- al$RunRecaEstimates

usethis::use_data(StoxBioticDataExample, overwrite = T)
usethis::use_data(StoxLandingDataExample, overwrite = T)
usethis::use_data(RecaCatchAtAgeExample, overwrite = T)
