# RstoxFDA v1.3-9003
* Added functions for analytical catch at age estimation (DefinePSUSamplingParameters, DefineIndividualSamplingParameters, AssignPSUSamplingParameters, AnalyticalPSUEstimate)

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