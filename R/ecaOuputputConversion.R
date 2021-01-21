#' @noRd
addIntercepts <- function(output, recaResult){
  
  for (covariate in names(recaResult$fit$ProportionAtAge$Intercept$cov)){
    
    for (a in recaResult$input$GlobalParameters$minage:recaResult$input$GlobalParameters$maxage){
      entry <- list()
      
      tab <- data.table::data.table(t(recaResult$fit$ProportionAtAge$Intercept$cov[[covariate]][a-recaResult$input$GlobalParameters$minage+1,,]))
      if (recaResult$input$AgeLength$info[covariate, "in.landings"] & covariate == "constant"){
        names(tab) <- unlist(recaResult$covariateMaps$inLandings[[covariate]])
      }
      else if (!recaResult$input$AgeLength$info[covariate, "in.landings"]){
        names(tab) <- unlist(recaResult$covariateMaps$randomEffects[[covariate]])
      }
      entry$age <- a
      entry$table <- tab
      entry$model <- "ProportionAtAge"
      entry$parameter <- "Intercept"
      entry$covariate <- covariate
      
      output[[paste("ProportionAtAge_Intercept_age", a, covariate, sep="_")]] <- entry
    }
  }
  return(output)
}

#' @noRd
recaResult2Stox <- function(recaResult){
  
  output <- list()
  output <- addIntercepts(output, recaResult)
  return(output)
}