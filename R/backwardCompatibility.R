backwardCompatibility <- list(
  renameParameter = list(
    list(
      changeVersion = "1.0.2", 
      functionName = "RunRecaModels", 
      modelName = "analysis", 
      parameterName = "AggregationVariables",
      newParameterName = "GroupingVariables"
    ),
    list(
      changeVersion = "1.0.2", 
      functionName = "ReportFdaSampling", 
      modelName = "report", 
      parameterName = "AggregationVariables",
      newParameterName = "GroupingVariables"
    ),
    list(
      changeVersion = "1.0.3", 
      functionName = "ReportRecaCatchStatistics", 
      modelName = "report", 
      parameterName = "DecimalOptions",
      newParameterName = "UseDefualtDecimalOptions"
    ),
    list(
      changeVersion = "1.0.3", 
      functionName = "ReportRecaCatchStatistics", 
      modelName = "report", 
      parameterName = "UnitOptions",
      newParameterName = "UseDefualtUnitOptions"
    )
  ) 
)