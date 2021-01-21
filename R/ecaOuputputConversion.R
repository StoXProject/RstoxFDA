#' @noRd
convertModelFit2Stox <- function(paramfit, paramtype, covariate, covariateMaps){
  
  paramfit$cov[[covariate]]
  paramtauname <- paste("tau", paramtype, sep="_")
  paramcarname <- paste("car", paramtype, sep="_")
  
  fits <- data.table::as.data.table(paramfit$cov[[covariate]])
  names(fits) <- c("AgeIndex", "LevelIndex", "Iteration", paramtype)
  
  if (!is.null(paramfit$tau[[covariate]])){
    fits[[paramtauname]] <- paramfit$tau[[covariate]][fits$Iteration]
  }
  else{
    fits[[paramtauname]] <- c(NA)[fits$Iteration]
  }
  
  if (!is.null(paramfit$CAR[[covariate]])){
    fits[[paramcarname]] <- paramfit$CAR[fits$Iteration]
  }
  else{
    fits[[paramcarname]] <- c(NA)[fits$Iteration]
  }
  
  return(fits)
}

#' @noRd
convertModelFit <- function(modelfit, covariateMaps){
  output <- list()
  
  output$LogLikelihood <- as.data.table(modelfit$LogLikelihood)
  names(output$LogLikelihood) <- "LogLikelihood"
  output$LogLikelihood$Iteration <- 1:nrow(output$LogLikelihood)
  
  covariates <- names(modelfit$Intercept$cov)
  for (co in covariates){
    
    fit <- convertModelFit2Stox(modelfit$Intercept, "Intercept", co, covariateMaps)
        
    if (("Slope") %in% names(modelfit)){
      if (co %in% names(modelfit$Slope$cov)){
        slopes <- convertModelFit2Stox(modelfit$Slope, "Slope", co, covariateMaps)
        fit <- merge(slopes, fit, by=names(fit)[names(fit) %in% names(slopes)])     
      }
    }
    
    #set covariate levels
    fit$Age <- covariateMaps$AgeCategories[fit$AgeIndex]
    if (co %in% names(covariateMaps$inLandings)){
      fit$Level <- covariateMaps$inLandings[[co]][fit$LevelIndex]
    }
    else if (co %in% names(covariateMaps$randomEffects)){
      fit$Level <- covariateMaps$randomEffects[[co]][fit$LevelIndex]
    }
    else{
      fit$Level <- c(NA)[fit$LevelIndex]
    }
    
    if (co == "contsant"){
      fit$Level <- fit$LevelIndex
    }
    
    first <- c("Age", "Level", "Iteration")
    order <- c(first, names(fit)[!(names(fit) %in% first)])
    
    output[[co]] <- fit[, order, with=F]
    
  }
  
  # For PorportionAtAge, there may be only the tau-parameter for covariate catchSample.
  if ("catchSample" %in% names(modelfit$Intercept$tau) & is.null(output[["catchSample"]])){
    output[["catchSample"]] <- data.table(Iteration=1:length(modelfit$Intercept$tau$catchSample), tau_Intercept=modelfit$Intercept$tau$catchSample)
  }
  
  # For LengthGivenAge and WeightGivenLength there may be the tau-parameter for fish
  if ("fish" %in% names(modelfit$Intercept$tau) & is.null(output[["fish"]])){
    output[["fish"]] <- data.table(Iteration=1:length(modelfit$Intercept$tau$fish), tau_Intercept=modelfit$Intercept$tau$fish)
  }
  
  return(output)
}

#' @noRd
convertModelFit2eca <- function(stoxfit, propatage=F){
  output <- list()
  intercepts <- list()
  tau_intercepts <- list()
  car_intercepts <- list()
  slopes <- list()
  tau_slopes <- list()
  car_slopes <- list()
  
  if (propatage){
    exclude <- c("LogLikelihood", "catchSample")
  }
  else{
    exclude <- c("LogLikelihood", "fish")
  }
  for (cov in names(stoxfit)[!(names(stoxfit) %in% exclude)]){
    
    # add intercept
    intercept <- stoxfit[[cov]][,c("AgeIndex", "LevelIndex", "Iteration", "Intercept")]
    intercept <- intercept[order(intercept$Iteration, intercept$LevelIndex, intercept$AgeIndex),]
    intercepts[[cov]] <- array(intercept$Intercept, dim = c(length(unique(intercept$AgeIndex)), length(unique(intercept$LevelIndex)), length(unique(intercept$Iteration))))
    
    # add slope
    if ("Slope" %in% names(stoxfit[[cov]])){
      slope <- stoxfit[[cov]][,c("AgeIndex", "LevelIndex", "Iteration", "Slope")]
      slope <- slope[order(slope$Iteration, slope$LevelIndex, slope$AgeIndex),]
      slopes[[cov]] <- array(slope$Slope, dim = c(length(unique(slope$AgeIndex)), length(unique(slope$LevelIndex)), length(unique(slope$Iteration))))
      
      # add tau slope
      if (any(!is.na(stoxfit[[cov]]$tau_Slope))){
        tau_int <- stoxfit[[cov]][,c("Iteration", "tau_Slope")]
        tau_int <- tau_int[!duplicated(tau_int$Iteration)]
        tau_int <- tau_int[order(tau_int$Iteration),]
        tau_slopes[[cov]] <- tau_int$tau_Slope
      }
      
      # add car slope
      if (any(!is.na(stoxfit[[cov]]$car_Slope))){
        car_int <- stoxfit[[cov]][,c("Iteration", "car_Slope")]
        car_int <- car_int[!duplicated(car_int$Iteration)]
        car_int <- car_int[order(car_int$Iteration),]
        car_slopes[[cov]] <- car_int$car_Slope
      }
    }
    
    # add tau intercept
    if (any(!is.na(stoxfit[[cov]]$tau_Intercept))){
      tau_int <- stoxfit[[cov]][,c("Iteration", "tau_Intercept")]
      tau_int <- tau_int[!duplicated(tau_int$Iteration)]
      tau_int <- tau_int[order(tau_int$Iteration),]
      tau_intercepts[[cov]] <- tau_int$tau_Intercept
    }
    
    # add car intercept
    if (any(!is.na(stoxfit[[cov]]$car_Intercept))){
      car_int <- stoxfit[[cov]][,c("Iteration", "car_Intercept")]
      car_int <- car_int[!duplicated(car_int$Iteration)]
      car_int <- car_int[order(car_int$Iteration),]
      car_intercepts[[cov]] <- car_int$car_Intercept
    }
    
  }
  
  if (propatage){
    if ("tau_Intercept" %in% names(stoxfit$catchSample)){
      if (is.null(stoxfit$Intercept$tau)){
        stoxfit$Intercept$tau <- list()
      }
      taus <- stoxfit$catchSample
      taus <- taus[order(taus$Iteration)]
      tau_intercepts$catchSample <- stoxfit$catchSample$tau_Intercept
    }
  }
  if ("fish" %in% names(stoxfit)){
    if ("tau_Intercept" %in% names(stoxfit$fish)){
      if (is.null(stoxfit$Intercept$tau)){
        stoxfit$Intercept$tau <- list()
      }
      taus <- stoxfit$fish
      taus <- taus[order(taus$Iteration)]
      tau_intercepts$fish <- stoxfit$fish$tau_Intercept
    }
  }
  
  # force listing of elements even if they are set to NULL
  if (length(car_intercepts) == 0){
    car_intercepts <- NULL
  }
  if (length(tau_intercepts) == 0){
    tau_intercepts <- NULL
  }
  output$Intercept <- list(cov = intercepts, tau = tau_intercepts, CAR = car_intercepts)
  
  if (length(car_slopes) == 0){
    car_slopes <- NULL
  }
  if (length(tau_slopes) == 0){
    tau_slopes <- NULL
  }
  output$Slope <- list(cov = slopes, tau = tau_slopes, CAR = car_slopes)
  
  
  return(output)
 
}

#' Converts output from Reca to format Stox-output format for parameterisation
#' @noRd
recaFit2Stox <- function(fit, covariateMaps){
  
  output <- list()
  output$FitProportionAtAge <- convertModelFit(fit$ProportionAtAge, covariateMaps)
  output$FitLengthGivenAge <- convertModelFit(fit$LengthGivenAge, covariateMaps)
  output$FitWeightGivenLength <- convertModelFit(fit$WeightGivenLength, covariateMaps)
  return(output)
}

#' Converts Stox-output format for Reca parameterisation to Reca to format 
#' @noRd
stox2recaFit <- function(stoxFit){
  
  fits <- list()
  
  fits$ProportionAtAge <- convertModelFit2eca(stoxFit$FitProportionAtAge, propatage=T)
  fits$ProportionAtAge$LogLikelihood <- stoxFit$FitProportionAtAge$LogLikelihood$LogLikelihood
  
  fits$LengthGivenAge <- convertModelFit2eca(stoxFit$FitLengthGivenAge)
  fits$LengthGivenAge$LogLikelihood <- stoxFit$FitLengthGivenAge$LogLikelihood$LogLikelihood
  
  fits$WeightGivenLength <- convertModelFit2eca(stoxFit$FitWeightGivenLength)
  fits$WeightGivenLength$LogLikelihood <- stoxFit$FitWeightGivenLength$LogLikelihood$LogLikelihood
  
  # remove slope from proportionatage
  fits$ProportionAtAge$Slope <- NULL
  
  return(fits)
  
}

#' Converst ecaResults to stox-formatted output
#' @noRd
ecaResult2Stox <- function(ecaPrediction){
  
  CatchAtAge <- data.table::as.data.table(ecaPrediction$TotalCount)
  names(CatchAtAge) <- c("Length", "Age", "Iteration","CatchAtAge")
  
  out <- list()
  
  len <- round(exp(ecaPrediction$LengthIntervalsLog), digits=2)
  CatchAtAge$Length <- len[CatchAtAge$Length]
  CatchAtAge$Age <- ecaPrediction$AgeCategories[CatchAtAge$Age]
  out$CatchAtAge <- CatchAtAge
  
  MeanLength <- data.table::as.data.table(t(ecaPrediction$MeanLength))
  names(MeanLength) <- as.character(ecaPrediction$AgeCategories)
  MeanLength$Iteration <- 1:nrow(MeanLength)
  MeanLength <- data.table::melt(MeanLength, measure.vars=c(as.character(ecaPrediction$AgeCategories)), variable.name="Age", variable.factor=F)
  MeanLength$Age <- as.integer(MeanLength$Age)
  out$MeanLength <- MeanLength
  
  MeanWeight <- data.table::as.data.table(t(ecaPrediction$MeanWeight))
  names(MeanWeight) <- as.character(ecaPrediction$AgeCategories)
  MeanWeight$Iteration <- 1:nrow(MeanWeight)
  MeanWeight <- data.table::melt(MeanWeight, measure.vars=c(as.character(ecaPrediction$AgeCategories)), variable.name="Age", variable.factor=F)
  MeanWeight$Age <- as.integer(MeanWeight$Age)
  out$MeanWeight <- MeanWeight  
  
  return(out)
}

