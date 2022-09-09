#' @noRd
place <- function(item, nested, levels){
  if (length(levels) == 1){
    nested[[levels]] <- item
    return(nested)
  }
  
  if (!(levels[[1]] %in% names(nested))){
    nested[[levels[[1]]]] <- list()
  }
  nested[[levels[[1]]]] <- place(item, nested[[levels[[1]]]], levels[2:length(levels)])

  return(nested)
}

#' @noRd
unpackCMlist <- function(flatlist){
  nested <- list()
  for (name in names(flatlist)){
    levels <- strsplit(name, "_", T)[[1]]
    levels <- levels[2:length(levels)]
    nested <- place(flatlist[[name]], nested, levels)
  }
  return(nested)
}

#' Make covariate compatible with prepRECA
#' @noRd
convertCovariateMap2PrepReca <- function(stoxObj){
  
  CovariateMap <- list()
  for (n in names(stoxObj$CovariateMaps)){
    if (startsWith(n, "CovariateMaps")){
      if (!("names" %in% names(stoxObj[[n]]))){
        CovariateMap[[gsub("CovariateMaps", "", n)]] <- stoxObj$CovariateMaps[[n]]$values  
        stoxObj[[n]] <- NULL
      }
      else if("integer" %in% class(stoxObj[[n]]$names) ){
        l <- as.list(stoxObj$CovariateMaps[[n]]$values)
        CovariateMap[[gsub("CovariateMaps", "", n)]] <- l
        stoxObj[[n]] <- NULL
      }
      else{
        l <- as.list(stoxObj$CovariateMaps[[n]]$values)
        names(l) <- stoxObj$CovariateMaps[[n]]$names
        CovariateMap[[gsub("CovariateMaps", "", n)]] <- l
        stoxObj[[n]] <- NULL
      }
      
    }
  }
  
  stoxObj$CovariateMaps <- unpackCMlist(CovariateMap)
  

  return(stoxObj)
}

#' @noRd
packCMlist <- function(member, name){
  if (class(member) != "list" | is.null(names(member))){
    res <- list()
    res[[name]] <- member
    return(res)
  }

  flat <- list()
  for (m in names(member)){
    subFlat <- packCMlist(member[[m]], paste(name, m, sep="_"))
    flat <- append(flat, subFlat)
  }

  return(flat)  
}

#' Make covariate map conform to RstoxFramework restrictions
#' @noRd
convertCovariateMap2Stox <- function(prepObj){

  newMap <- list()
  CovariateMap <- packCMlist(prepObj$CovariateMaps, "CovariateMaps")
  for (n in names(CovariateMap)){
    if (class(CovariateMap[[n]]) != "list"){
      newMap[[n]] <- data.table::data.table(names=1:length(CovariateMap[[n]]), values=CovariateMap[[n]])
    }
    else{
      newMap[[n]] <- data.table::data.table(names=1:length(CovariateMap[[n]]), values=unlist(CovariateMap[[n]]))  
    }
    
  }
  
  prepObj$CovariateMaps <- newMap
  
  return(prepObj)
}

#' @noRd
convertModelFit2Stox <- function(paramfit, paramtype, covariate, covariateMaps){
  
  paramfit$cov[[covariate]]
  paramtauname <- paste("tau", paramtype, sep="_")
  paramcarname <- paste("ar", paramtype, sep="_")
  
  fits <- data.table::as.data.table(paramfit$cov[[covariate]])
  names(fits) <- c("AgeIndex", "LevelIndex", "Iteration", paramtype)
  
  if (!is.null(paramfit$tau[[covariate]])){
    fits[[paramtauname]] <- paramfit$tau[[covariate]][fits$Iteration]
  }
  else{
    fits[[paramtauname]] <- c(NA)[fits$Iteration]
  }
  
  if (!is.null(covariateMaps$carEffect)){
    if (covariateMaps$carEffect==covariate){
      fits[[paramcarname]] <- paramfit$CAR$spatial[fits$Iteration]  
    }
  }
  else{
    fits[[paramcarname]] <- c(NA)[fits$Iteration]
  }
  
  return(fits)
}

#' @noRd
convertModelFit <- function(modelfit, covariateMaps, model){
  output <- list()
  output$LogLikelihood <- data.table::as.data.table(modelfit$LogLikelihood)
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
    if (length(covariateMaps$StockSplitting$StockNameCC)>0){
      fit$Age <- c(paste(covariateMaps$StockSplitting$StockNameCC,covariateMaps$AgeCategories), paste(covariateMaps$StockSplitting$StockNameCC,covariateMaps$AgeCategories))[fit$AgeIndex]
    }
    else{
      fit$Age <- covariateMaps$AgeCategories[fit$AgeIndex]      
    }
    if (co %in% names(covariateMaps$inLandings)){
      fit$Level <- covariateMaps$inLandings[[co]][fit$LevelIndex]
    }
    else if (co %in% names(covariateMaps$randomEffects$AgeLength) & model == "LengthGivenAge"){
      fit$Level <- unlist(covariateMaps$randomEffects$AgeLength[[co]])[fit$LevelIndex]
    }
    else if (co %in% names(covariateMaps$randomEffects$WeightLength) & model == "WeightGivenLength"){
      fit$Level <- unlist(covariateMaps$randomEffects$WeightLength[[co]])[fit$LevelIndex]
    }
    else{
      fit$Level <- paste("LevelIndex",fit$LevelIndex,sep="_")
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
    output[["catchSample"]] <- data.table::data.table(Iteration=1:length(modelfit$Intercept$tau$catchSample), tau_Intercept=modelfit$Intercept$tau$catchSample)
  }
  
  # For LengthGivenAge and WeightGivenLength there may be the tau-parameter for fish
  if ("fish" %in% names(modelfit$Intercept$tau) & is.null(output[["fish"]])){
    output[["fish"]] <- data.table::data.table(Iteration=1:length(modelfit$Intercept$tau$fish), tau_Intercept=modelfit$Intercept$tau$fish)
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
      if (any(!is.na(stoxfit[[cov]]$ar_Slope))){
        car_int <- stoxfit[[cov]][,c("Iteration", "ar_Slope")]
        car_int <- car_int[!duplicated(car_int$Iteration)]
        car_int <- car_int[order(car_int$Iteration),]
        car_slopes[[cov]] <- car_int$ar_Slope
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
    if (any(!is.na(stoxfit[[cov]]$ar_Intercept))){
      car_int <- stoxfit[[cov]][,c("Iteration", "ar_Intercept")]
      car_int <- car_int[!duplicated(car_int$Iteration)]
      car_int <- car_int[order(car_int$Iteration),]
      car_intercepts[[cov]] <- car_int$ar_Intercept
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
  output$FitProportionAtAge <- convertModelFit(fit$ProportionAtAge, covariateMaps, "ProportionAtAge")
  output$FitLengthGivenAge <- convertModelFit(fit$LengthGivenAge, covariateMaps, "LengthGivenAge")
  output$FitWeightGivenLength <- convertModelFit(fit$WeightGivenLength, covariateMaps, "WeightGivenLength")
  
  if ("LengthGivenAgeCC" %in% names(fit)){
    output$FitLengthGivenAgeCC <- convertModelFit(fit$LengthGivenAgeCC, covariateMaps, "LengthGivenAge")
  }
  if ("WeightGivenLengthCC" %in% names(fit)){
    output$FitWeightGivenLengthCC <- convertModelFit(fit$WeightGivenLengthCC, covariateMaps, "WeightGivenLength")
  }
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
  
  if ("FitLengthGivenAgeCC" %in% names(stoxFit)){
    fits$LengthGivenAgeCC <- convertModelFit2eca(stoxFit$FitLengthGivenAgeCC)
    fits$LengthGivenAgeCC$LogLikelihood <- stoxFit$FitLengthGivenAgeCC$LogLikelihood$LogLikelihood
  }
  
  if ("FitWeightGivenLengthCC" %in% names(stoxFit)){
    fits$WeightGivenLengthCC <- convertModelFit2eca(stoxFit$FitWeightGivenLengthCC)
    fits$WeightGivenLengthCC$LogLikelihood <- stoxFit$FitWeightGivenLengthCC$LogLikelihood$LogLikelihood  
  }
  
  # remove slope from proportionatage
  fits$ProportionAtAge$Slope <- NULL
  
  return(fits)
  
}

#' Converst ecaResults to stox-formatted output
#' @param stocks the member StocSplitting of covariateMaps, iff stock splitting was used.
#' @noRd
ecaResult2Stox <- function(ecaPrediction, stocks=NULL){
  
  CatchAtAge <- data.table::as.data.table(ecaPrediction$TotalCount)
  names(CatchAtAge) <- c("Length", "Age", "Iteration","CatchAtAge")
  
  #handle stock splitting
  if (!is.null(stocks)){
    stopifnot(all(c("StockNameCC","StockNameS") %in% names(stocks)))
    stockNames <- c(rep(stocks$StockNameS, length(ecaPrediction$AgeCategories)/2),
                    rep(stocks$StockNameCC, length(ecaPrediction$AgeCategories)/2))
    CatchAtAge$Stock <- stockNames[CatchAtAge$Age]
  }
  
  out <- list()
  
  #fix varaible levels
  len <- round(exp(ecaPrediction$LengthIntervalsLog), digits=2)
  CatchAtAge$Length <- len[CatchAtAge$Length]
  CatchAtAge$Age <- ecaPrediction$AgeCategories[CatchAtAge$Age]
  out$CatchAtAge <- CatchAtAge
    
  
  # extract mean length
  MeanLength <- data.table::as.data.table(ecaPrediction$MeanLength)
  names(MeanLength) <- as.character(1:ncol(MeanLength))
  MeanLength$Age <- as.character(ecaPrediction$AgeCategories)
  
  if (!is.null(stocks)){
    stockNames <- c(rep(stocks$StockNameS, length(ecaPrediction$AgeCategories)/2),
                    rep(stocks$StockNameCC, length(ecaPrediction$AgeCategories)/2))
    MeanLength$Stock <- stockNames
    MeanLength <- data.table::melt(MeanLength, id.vars=c("Age", "Stock"), variable.name="Iteration", value.name="MeanIndividualLength", variable.factor=F)
  }
  else{
    MeanLength <- data.table::melt(MeanLength, id.vars=c("Age"), variable.name="Iteration", value.name="MeanIndividualLength", variable.factor=F)
  }
  
  MeanLength$Age <- as.integer(MeanLength$Age)
  MeanLength$Iteration <- as.integer(MeanLength$Iteration)
  out$MeanLength <- MeanLength
  
  
  # extract mean weight
  
  MeanWeight <- data.table::as.data.table(ecaPrediction$MeanWeight)
  names(MeanWeight) <- as.character(1:ncol(MeanWeight))
  MeanWeight$Age <- as.character(ecaPrediction$AgeCategories)
  
  if (!is.null(stocks)){
    stockNames <- c(rep(stocks$StockNameS, length(ecaPrediction$AgeCategories)/2),
                    rep(stocks$StockNameCC, length(ecaPrediction$AgeCategories)/2))
    MeanWeight$Stock <- stockNames
    MeanWeight <- data.table::melt(MeanWeight, id.vars=c("Age", "Stock"), variable.name="Iteration", value.name="MeanIndividualWeight", variable.factor=F)
  }
  else{
    MeanWeight <- data.table::melt(MeanWeight, id.vars=c("Age"), variable.name="Iteration", value.name="MeanIndividualWeight", variable.factor=F)
  }
  
  MeanWeight$Age <- as.integer(MeanWeight$Age)
  MeanWeight$Iteration <- as.integer(MeanWeight$Iteration)
  out$MeanWeight <- MeanWeight
  
  return(out)
}

#' reformats CarNeigbours, must handle covariate names
#' @noRd
convertCarNeighbours2stox <- function(ecaCar, covariateMaps){
  if (is.null(ecaCar)){
    return(NULL)
  }
  CarValue <- unlist(covariateMaps$inLandings[[covariateMaps$carEffect]])
  Neighbours <- rep("", length(CarValue))
  numNindex <- 1
  for (i in 1:length(Neighbours)){
    Neighbours[i] <- paste(covariateMaps$inLandings[[covariateMaps$carEffect]][ecaCar$idNeighbours[numNindex:(numNindex+ecaCar$numNeighbours[i]-1)]], collapse=",")
    numNindex <- numNindex + ecaCar$numNeighbours[i]
  }
  return(data.table::data.table(CarValue=CarValue, Neighbours=Neighbours))
}

#' reformats CarNeigbours to format accepted by wrapReca
#' @noRd
convertCarNeighbours2recaWrap <- function(stoxCar, carLandingsValues){
  if (is.null(stoxCar)){
    return(NULL)
  }
    
    stoxCar <- stoxCar[stoxCar$CarValue %in% carLandingsValues,]    
    neighbourList <- lapply(stoxCar$Neighbours, FUN=function(x){
      nb <-strsplit(x, ",")[[1]];
      return(nb[nb %in% carLandingsValues]);})
    names(neighbourList) <- stoxCar$CarValue
    
    
    #neighbourList <- lapply(stoxCar$Neighbours, FUN=function(x){
    #  nb <-strsplit(x, ",")[[1]];
    #  return(nb);})
    #names(neighbourList) <- stoxCar$CarValue

  return(neighbourList)
}

#' reformats CarNeigbours to format accepted by Reca
#' @noRd
convertCarNeighbours2reca <- function(stoxCar, covariateMap){
  if (is.null(stoxCar)){
    return(NULL)
  }
  neighbourList <- lapply(stoxCar$Neighbours, FUN=function(x){
    nb <-strsplit(x, ",")[[1]];
    return(nb);})
  names(neighbourList) <- stoxCar$CarValue
  
  return(getNeighbours(neighbourList, covariateMap$inLandings[[covariateMap$carEffect]]))
}


#' reformats CarNeigbours, does not translate covariate names
#' @noRd
convertPrepReca2stox <- function(prepRecaOutput){
  prepRecaOutput$AgeLength$DataMatrix <- data.table::as.data.table(prepRecaOutput$AgeLength$DataMatrix)
  prepRecaOutput$AgeLength$CovariateMatrix <- data.table::as.data.table(prepRecaOutput$AgeLength$CovariateMatrix)
  
  effectnames <- rownames(prepRecaOutput$AgeLength$info)
  prepRecaOutput$AgeLength$info <- cbind(data.table::data.table(covariate=effectnames),data.table::as.data.table(prepRecaOutput$AgeLength$info))
  
  prepRecaOutput$WeightLength$DataMatrix <- data.table::as.data.table(prepRecaOutput$WeightLength$DataMatrix)
  prepRecaOutput$WeightLength$CovariateMatrix <- data.table::as.data.table(prepRecaOutput$WeightLength$CovariateMatrix)
  
  effectnames <- rownames(prepRecaOutput$WeightLength$info)
  prepRecaOutput$WeightLength$info <- cbind(data.table::data.table(covariate=effectnames),data.table::as.data.table(prepRecaOutput$WeightLength$info))
  
  prepRecaOutput$Landings$AgeLengthCov <- data.table::as.data.table(prepRecaOutput$Landings$AgeLengthCov)
  prepRecaOutput$Landings$WeightLengthCov <- data.table::as.data.table(prepRecaOutput$Landings$WeightLengthCov)
  
  prepRecaOutput$Landings$LiveWeightKG <- data.table::data.table(LiveWeightKG=prepRecaOutput$Landings$LiveWeightKG)
  
  if (!is.null(prepRecaOutput$AgeLength$CARNeighbours)){
    prepRecaOutput$AgeLength$CARNeighbours <- convertCarNeighbours2stox(prepRecaOutput$AgeLength$CARNeighbours, prepRecaOutput$CovariateMaps)    
  }
  if (!is.null(prepRecaOutput$WeightLength$CARNeighbours)){
    prepRecaOutput$WeightLength$CARNeighbours <- convertCarNeighbours2stox(prepRecaOutput$WeightLength$CARNeighbours, prepRecaOutput$CovariateMaps)    
  }
  if (!is.null(prepRecaOutput$AgeLength$AgeErrorMatrix)){
    prepRecaOutput$AgeLength$AgeErrorMatrix <- convertAgeErrorMatrixStox(prepRecaOutput$AgeLength$AgeErrorMatrix, prepRecaOutput$GlobalParameters$minage, prepRecaOutput$GlobalParameters$maxage)
  }
  if (!is.null(prepRecaOutput$WeightLength$AgeErrorMatrix)){
    prepRecaOutput$WeightLength$AgeErrorMatrix <- convertAgeErrorMatrixStox(prepRecaOutput$WeightLength$AgeErrorMatrix, prepRecaOutput$GlobalParameters$minage, prepRecaOutput$GlobalParameters$maxage)
  }
  
  if (!is.null(prepRecaOutput$AgeLength$CCerrorList)){
    prepRecaOutput$AgeLength$StockSplittingParameters <- convertStockSplittingParameters2stox(prepRecaOutput$AgeLength$CCerrorList, prepRecaOutput$CovariateMaps)
    prepRecaOutput$AgeLength$CCerrorList <- NULL
  }
  
  GlobalParameters <- data.table::data.table(lengthresCM=prepRecaOutput$GlobalParameters$lengthresCM,
                                                            maxlength=prepRecaOutput$GlobalParameters$maxlength,
                                                            minage=prepRecaOutput$GlobalParameters$minage,
                                                            maxage=prepRecaOutput$GlobalParameters$maxage,
                                                            age.error=prepRecaOutput$GlobalParameters$age.error,
                                                            CC=prepRecaOutput$GlobalParameters$CC,
                                                            CCerror=prepRecaOutput$GlobalParameters$CCerror,
                                             
                                                            nSamples=prepRecaOutput$GlobalParameters$nSamples,
                                                            burnin=prepRecaOutput$GlobalParameters$burnin,
                                                            lgamodel=prepRecaOutput$GlobalParameters$lgamodel,
                                                            resultdir=prepRecaOutput$GlobalParameters$resultdir,
                                                            fitfile=prepRecaOutput$GlobalParameters$fitfile,
                                                            predictfile=prepRecaOutput$GlobalParameters$predictfile,
                                                            thin=prepRecaOutput$GlobalParameters$thin,
                                                            delta.age=prepRecaOutput$GlobalParameters$delta.age,
                                                            seed=prepRecaOutput$GlobalParameters$seed
                                                            )
  prepRecaOutput$GlobalParameters <- list()
  prepRecaOutput$GlobalParameters$GlobalParameters <-  GlobalParameters
  prepRecaOutput <- convertCovariateMap2Stox(prepRecaOutput)
  
  return(prepRecaOutput)
}

#' reformats data.tables
#' @noRd
convertStox2PrepReca <- function(stoxPrep){
  stoxPrep <- convertCovariateMap2PrepReca(stoxPrep)
    
  stoxPrep$AgeLength$DataMatrix <- as.data.frame(stoxPrep$AgeLength$DataMatrix)
  stoxPrep$AgeLength$CovariateMatrix <- as.data.frame(stoxPrep$AgeLength$CovariateMatrix)
  
  rn <- stoxPrep$AgeLength$info$covariate
  stoxPrep$AgeLength$info <- as.matrix(stoxPrep$AgeLength$info[,names(stoxPrep$AgeLength$info) != "covariate", with=F])
  rownames(stoxPrep$AgeLength$info) <- rn
  
  stoxPrep$WeightLength$DataMatrix <- as.data.frame(stoxPrep$WeightLength$DataMatrix)
  stoxPrep$WeightLength$CovariateMatrix <- as.data.frame(stoxPrep$WeightLength$CovariateMatrix)
  
  rn <- stoxPrep$WeightLength$info$covariate
  stoxPrep$WeightLength$info <- as.matrix(stoxPrep$WeightLength$info[,names(stoxPrep$WeightLength$info) != "covariate", with=F])
  rownames(stoxPrep$WeightLength$info) <- rn
  
  stoxPrep$Landings$AgeLengthCov <- as.data.frame(stoxPrep$Landings$AgeLengthCov)
  stoxPrep$Landings$WeightLengthCov <- as.data.frame(stoxPrep$Landings$WeightLengthCov)
  
  stoxPrep$Landings$LiveWeightKG <- stoxPrep$Landings$LiveWeightKG$LiveWeightKG
  
  if (!is.null(stoxPrep$AgeLength$CARNeighbours)){
    stoxPrep$AgeLength$CARNeighbours <- convertCarNeighbours2reca(stoxPrep$AgeLength$CARNeighbours, stoxPrep$CovariateMaps)    
  }
  if (!is.null(stoxPrep$WeightLength$CARNeighbours)){
    stoxPrep$WeightLength$CARNeighbours <- convertCarNeighbours2reca(stoxPrep$WeightLength$CARNeighbours, stoxPrep$CovariateMaps)    
  }
  if (!is.null(stoxPrep$AgeLength$AgeErrorMatrix)){
    stoxPrep$AgeLength$AgeErrorMatrix <- convertAgeErrorMatrixReca(stoxPrep$AgeLength$AgeErrorMatrix)
  }
  if (!is.null(stoxPrep$WeightLength$AgeErrorMatrix)){
    stoxPrep$WeightLength$AgeErrorMatrix <- convertAgeErrorMatrixReca(stoxPrep$WeightLength$AgeErrorMatrix)
  }
  
  if (!is.null(stoxPrep$AgeLength$StockSplittingParameters)){
    stoxPrep$AgeLength$CCerrorList <- convertStockSplittingParameters2reca(stoxPrep$AgeLength$StockSplittingParameters)
    stoxPrep$AgeLength$StockSplittingParameters <- NULL
  }
  
  gb <- stoxPrep$GlobalParameters
  GlobalParameters <- list()
  GlobalParameters$lengthresCM <- gb$GlobalParameters$lengthresCM
  GlobalParameters$maxlength <- gb$GlobalParameters$maxlength
  GlobalParameters$minage <- gb$GlobalParameters$minage
  GlobalParameters$maxage <- gb$GlobalParameters$maxage
  GlobalParameters$age.error <- gb$GlobalParameters$age.error
  GlobalParameters$CC <- gb$GlobalParameters$CC
  GlobalParameters$CCerror <- gb$GlobalParameters$CCerror
  
  GlobalParameters$nSamples <- gb$GlobalParameters$nSamples
  GlobalParameters$burnin < gb$GlobalParameters$burnin
  GlobalParameters$lgamodel <- gb$GlobalParameters$lgamodel
  GlobalParameters$resultdir <- gb$GlobalParameters$resultdir
  GlobalParameters$fitfile <- gb$GlobalParameters$fitfile
  GlobalParameters$predictfile <- gb$GlobalParameters$predictfile
  GlobalParameters$thin <- gb$GlobalParameters$thin
  GlobalParameters$delta.age <- gb$GlobalParameters$delta.age
  GlobalParameters$seed <- gb$GlobalParameters$seed

  stoxPrep$GlobalParameters <- GlobalParameters
  
  return(stoxPrep)
}

#' Converts Age Error matrix to Reca format, also accepted by recaWrap
#' @noRd
convertAgeErrorMatrixReca <- function(AgeErrorMatrix){
  AgeErrorMatrix <- as.matrix(AgeErrorMatrix[,!(names(AgeErrorMatrix) %in% "ReadAge"), with=F])
  rownames(AgeErrorMatrix) <- colnames(AgeErrorMatrix)
  return(AgeErrorMatrix)
}

#' Converts Age Error matrix from Reca format, also accepted by recaWrap
#' @noRd
convertAgeErrorMatrixStox <- function(AgeErrorMatrix, minAge, maxAge){
  AgeErrorMatrix <- data.table::data.table(AgeErrorMatrix)
  AgeErrorMatrix$ReadAge <- minAge:maxAge
  return(AgeErrorMatrix)
}


#' Converts Classification error to Reca format (Stock splitting not supported by recaWrap)
#' @noRd
convertStockSplittingParameters2reca <- function(stocksplit){
  CCerrorList <- list()
  CCerrorList$ptype1.CC <- stocksplit$ProbabilityType1As1
  CCerrorList$ptype1.S <- stocksplit$ProbabilityType5As1
  CCerrorList$ptype2.CC <- stocksplit$ProbabilityType2As2
  CCerrorList$ptype2.S <- stocksplit$ProbabilityType4As2
  CCerrorList$ptype4.CC <- stocksplit$ProbabilityType2As4
  CCerrorList$ptype4.S <- stocksplit$ProbabilityType4As4
  CCerrorList$ptype5.CC <- stocksplit$ProbabilityType1As5
  CCerrorList$ptype5.S <- stocksplit$ProbabilityType5As5
  
  return(CCerrorList)
  
}

#' Converts Classification error to Stox format
#' @noRd
convertStockSplittingParameters2stox <- function(CCerrorList, covariateMaps){
  
  stopifnot(!is.null(covariateMaps$StockSplitting))
  
  tab <- data.table::data.table(StockNameCC=covariateMaps$StockSplitting$StockNameCC,
                    StockNameS=covariateMaps$StockSplitting$StockNameS,
                    ProbabilityType1As1 = CCerrorList$ptype1.CC,
                    ProbabilityType5As1 = CCerrorList$ptype1.S,
                    ProbabilityType2As2 = CCerrorList$ptype2.CC,
                    ProbabilityType4As2 = CCerrorList$ptype2.S,
                    ProbabilityType2As4 = CCerrorList$ptype4.CC,
                    ProbabilityType4As4 = CCerrorList$ptype4.S,
                    ProbabilityType1As5 = CCerrorList$ptype5.CC,
                    ProbabilityType5As5 = CCerrorList$ptype5.S)
  stopifnot(is.StockSplittingParameters(tab))
  return(tab)
}

#' Convert RecaData
#' @description 
#'  Converts \code{\link[RstoxFDA]{RecaData}} prepared for StoX-Reca to the input format accepted by \code{\link[Reca]{eca.estimate}}.
#'  This facilitates preparing input data with a StoX-project for more cusomized programs directly interfacing Reca.
#' @details 
#'  The StoX-function interfacing Reca-parameterisation (\code{\link[RstoxFDA]{RunRecaModels}}) accepts a data format
#'  \code{\link[RstoxFDA]{RecaData}} that is not exactly compatible with the format accepted by \code{\link[Reca]{eca.estimate}}.
#'  This inconvenience is introduced to make sure that output from \code{\link[RstoxFDA]{PrepareRecaEstimate}} adheres to
#'  restrictions in the StoX-user interface, so that the results are inspectable there.
#'  
#'  Reca-input include model parameters and options, but \code{\link[RstoxFDA]{PrepareRecaEstimate}} only sets parameters that have
#'  implications for data formatting, other parameters are subsequently set in for instance \code{\link[RstoxFDA]{RunRecaModels}}. 
#'  The optional arguments to this function can be used to set those parameters that is handled by \code{\link[RstoxFDA]{RunRecaModels}} in the StoX-workflow.
#'  
#'  Reca requires categorical variables to be encoded as consecutive integers, starting with 1. In order to keep track of how this correspond to other representations, 
#'  this function provides the mapping in the list 'CovariateMaps'. CovariateMaps is not an input to Reca, but only provided to assist book-keeping.
#' @param RecaData \code{\link[RstoxFDA]{RecaData}}, as prepared by \code{\link[RstoxFDA]{PrepareRecaEstimate}}.
#' @param nSamples nSamples as specified in \code{\link[Reca]{eca.estimate}}
#' @param burnin burnin as specified in \code{\link[Reca]{eca.estimate}}
#' @param thin thin as specified in \code{\link[Reca]{eca.estimate}}
#' @param resultdir resultdir as specified in \code{\link[Reca]{eca.estimate}}
#' @param lgamodel lgamodel as specified in \code{\link[Reca]{eca.estimate}}
#' @param delta.age delta.age as specified in \code{\link[Reca]{eca.estimate}}
#' @param seed seed as specified in \code{\link[Reca]{eca.estimate}}
#' @param fitfile fitfile as specified in \code{\link[Reca]{eca.estimate}}
#' @param predictfile predictfile as specified in \code{\link[Reca]{eca.estimate}}
#' @return list with 5 members:
#'  \describe{
#'   \item{AgeLength}{Data prepared for the argument AgeLength to \code{\link[Reca]{eca.estimate}}}
#'   \item{WeightLength}{Data prepared for the argument WeightLength to \code{\link[Reca]{eca.estimate}}}
#'   \item{Landings}{Data prepared for the argument Landings to \code{\link[Reca]{eca.estimate}}}
#'   \item{GlobalParameters}{Data prepared for the argument GlobalParameters to \code{\link[Reca]{eca.estimate}}, Completeness of this data structure depend on whether optional parameters where provided.}
#'   \item{CovariateMaps}{Not input to Reca. Nested list, providing mapping between integer encoding and character encoding of the levels of categorical variables.}
#'  }
#' @family Reca functions
#' @export
convertRecaData <- function(RecaData, nSamples=as.integer(NA), 
                            burnin=as.integer(NA), 
                            thin=as.integer(NA), 
                            resultdir=as.character(NA), 
                            lgamodel=as.character(NA), 
                            delta.age=as.numeric(NA),
                            seed=as.numeric(NA), 
                            fitfile=as.character(NA), 
                            predictfile=as.character(NA)){
  RecaData <- convertStox2PrepReca(RecaData)
  RecaData$GlobalParameters$nSamples <- nSamples
  RecaData$GlobalParameters$burnin <- burnin
  RecaData$GlobalParameters$lgamodel <- lgamodel
  RecaData$GlobalParameters$resultdir <- resultdir
  RecaData$GlobalParameters$fitfile <- fitfile
  RecaData$GlobalParameters$predictfile <- predictfile
  RecaData$GlobalParameters$thin <- thin
  RecaData$GlobalParameters$delta.age <- delta.age
  RecaData$GlobalParameters$seed <- seed
  
  return(RecaData)
}
