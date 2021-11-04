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
    )
  ) 
)