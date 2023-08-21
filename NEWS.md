# RstoxFDA v1.2.1-9001
* Fixed issue where the DefineCarNeighbours would format column headers differently depending on the chosen DefinitionMethods. The column header 'CarValue' is now consistently written in the singular form, which means that output from the DefinitionMethod 'StratumPolygon' has changed, and the json schema for CarNeighbours has changed.

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