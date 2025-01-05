# 1.5.0-9003
* Added functions for sampling frame expansion (ExtendAnalyticalSamplingFrameCoverage), and domain interpolation (InterpolateAnalyticalDomainEstimates) (#154)
* Changed how Stratification variables and Domain variables are matched to landings with AnalyticalRatioEstimate (# 125)
* Replaced DefinePSUSamplingParameters (processdata) with ComputePSUSamplingParameters (no processdata) and ReadPSUSamplingParameters (no processdata). This change breaks some pre-release projects (v. v1.3-9006). (#127)
* Replaced DefineIndividualSamplingParameters (processdata) with ComputeIndividualSamplingParameters (no processdata). This change breaks some pre-release projects (v. v1.3-9006).
* Added documentation for ECA convergence analsysis (#153)
* Fixed issue with reporting IndividualAge as SamplingVariable in ReportFdaSampling (#146)
* Fixed issue with running RunRecaModels with temporal resolution 'month' (#144)
* Fixed issue with plotting with other column variables than 'Area' in PlotFisheriesOverviewSpatial (#147)
* Fixed naming of age groups in reports from analytical estimates (#145)
* Fixed documentation for handling NA age groups (#151)
* Fixed issue with using integer columns in landings as stratification column in ratio estimates (#133)
* Fixed error message when analytical catch at age reports are attempted without 'IndividualAge' in the domain definition (#126).

# 1.5.0-9002
* Fixed ordering of age groups in ReportAnalyticalCatchAtAge (#134)
* Improved Error messages in AnalyticalRatioEstimate (#131)
* Incorporated some feedback to the vignette Stox-Analytical (#128)

# RstoxFDA v1.4
* Converted area code resources from sp SpatialPolygons to sf data.frames, reflecting changes in RstoxBase v 2. Breaks compatibility with RstoxBase < 2.
* Changed ReadLandingFDA to use ForceUnique as implemented in RstoxData v 2. Breaks compatibility with RstoxData < 2
* Made definition of coordinates used for appendAreaCodes and DefineAreaPosition stricter (now always centroids)
* Made appendAreaCodes and DefineAreaPosition look for intersects in mercator projection
* Changed the signature of mergePolygons to work with sf data.frames rather than sp SpatialPolygons.
* Changed the definition neighbours in DefineCarNeighbours from polygons touching to polygons within 1 metre distance in mercator projection.
* Changed areaCodeConversionTable to work with provided area definitions in mercator projection
* Fixed issue with PlotMeanWeightAtAge and PlotMeanLengthAtAge that would not plot mean values for configurations with no grouping variables
* Fixed issue where the DefineCarNeighbours would format column headers differently depending on the chosen DefinitionMethods. The column header 'CarValue' is now consistently written in the singular form, which means that output from the DefinitionMethod 'StratumPolygon' has changed, and the json schema for CarNeighbours has changed.
* Fixed the behavior of AddStratumStoxLanding so that it assigns area NA, rather than halting with error, when positions cannot be assigned to an area.
* Fixed the behavior of ReportFdaSampling to count catches as unique hauls (column Haul on StoxBiotic$Haul), rather than stations. Improved documentation for the same function.
* Added checking on the validity of IntervalWidths in report functions that report Reca-credible intervals
* Clarified various warnings and error messages.
* Made provision of area definition optional in PlotArea, so that the function can be used to examine the spatial distribution of data without providing a spatial coding system, or to just make maps without providing either data or area definitions.
* Refactored to get rid of usages of ggplot::aes_string which is soft deprecated by ggplot.
* Added guidance to vignettes about how to deal with several hauls assigned the same station when the field 'station' is not correctly filled in NMDbiotic.
* Added workaround so that the non-linear option for lgamodel can be used with with all version of Reca.

# RstoxFDA v1.3-9006
* Changed analytical variance estimator of means to a less conservative one.

# RstoxFDA v1.3-9005
* Added functions for analytical catch at age estimation (DefinePSUSamplingParameters, ComputeIndividualSamplingParameters, AssignPSUSamplingParameters, AnalyticalPSUEstimate, AnalyticalPopulationEstimate, AnalyticalRatioEstimate, ReportAnalyticalCatchAtAge)
* Converted area code resources from sp SpatialPolygons to sf data.frames, reflecting changes in RstoxBase v 2. Breaks compatibility with RstoxBase < 2.
* Changed ReadLandingFDA to use ForceUnique as implemented in RstoxData v 2. Breaks compatibility with RstoxData < 2
* Made definition of coordinates used for appendAreaCodes and DefineAreaPosition stricter (now always centroids)
* Made appendAreaCodes and DefineAreaPosition look for intersects in mercator projection
* Changed the signature of mergePolygons to work with sf data.frames rather than sp SpatialPolygons.
* Changed the definition neighbours in DefineCarNeighbours from polygons touching to polygons within 1 metre distance in mercator projection.
* Changed areaCodeConversionTable to work with provided area definitions in mercator projection

# RstoxFDA v1.3-9002
* Fixed issue with PlotMeanWeightAtAge and PlotMeanLengthAtAge that would not plot mean values for configurations with no grouping variables

# RstoxFDA v1.2.1-9001
* Fixed issue where the DefineCarNeighbours would format column headers differently depending on the chosen DefinitionMethods. The column header 'CarValue' is now consistently written in the singular form, which means that output from the DefinitionMethod 'StratumPolygon' has changed, and the json schema for CarNeighbours has changed.
* Fixed the behavior of AddStratumStoxLanding so that it assigns area NA, rather than halting with error, when positions cannot be assigned to an area.
* Fixed the behaviour of ReportFdaSampling to count catches as unique hauls (column Haul on StoxBiotic$Haul), rather than stations. Improved documentation for the same function.
* Added checking on the validity of IntervalWidths in report functions that report Reca-credible intervals
* Clarified various warnings and error messages.
* Made provision of area definition optional in PlotArea, so that the function can be used to examine the spatial distribution of data without providing a spatial coding system, or to just make maps without providing either data or area definitions.
* Refactored to get rid of usages of ggplot::aes_string which is soft deprecated by ggplot.
* Added guidance to vignettes about how to deal with several hauls assigned the same station when the field 'station' is not correctly filled in NMDbiotic.
* Added workaround so that the non-linear option for lgamodel can be used with with all version of Reca.

# RstoxFDA v1.2.0  (2023-01-24)
* Bumped version number in order to get latest release in line with semantic versioning

# RstoxFDA v1.1.0-9001  (2022-12-16)
* Fixed bug with secondary axis on Sampling plot (#77)
* Changed secondary axis and its plot elements to have different color from main axis in PlotSamplingCoverage and PlotSamplingVariable
* Clarified some error messages for ConvertWeightBiotic (#79)
* Improved handling of ResultDirectory for ParameterizeRecaModels (#78)
* Fixed json-schema for AgeErrorMatrix (#75)

# RstoxFDA v1.1.0 (2022-11-26)
* contrary to what version numbers indicate, this was released before v1.1.0-9001.
