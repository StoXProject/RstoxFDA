
#' checks that column names are present on datamatrix
#' @noRd
check_columns_present <- function(datamatrix, columns){
  errors <- ""
  for (col in columns){
    if (!(col %in% attributes(datamatrix)$names)){
      errors <- paste(errors, "column", col, "missing.\n")
    }
  }
  if (errors != ""){
    stop(errors)
  }
}
#' columns does not have missing values
#' @noRd
check_none_missing <- function(datamatrix, columns){
  errors <- ""
  for (col in columns){
    if (any(is.na(datamatrix[,col]))){
      errors <- paste(errors, "column", col, "has missing value.\n")
    }
  }
  if (errors != ""){
    stop(errors)
  }
}
#' @noRd
check_cov_vs_info <- function(modelobj){
  if (!("constant" %in% names(modelobj$CovariateMatrix)) | !("constant" %in% rownames(modelobj$info))){
    stop("No constant column provided in covariate matrix or info matrix")
  }
  if (modelobj$info["constant","in.landings"]!=1 | modelobj$info["constant","in.slopeModel"]!=1 | modelobj$info["constant","random"]!=0 | modelobj$info["constant","CAR"]!=0 | modelobj$info["constant","continuous"]!=0){
    stop("Constant covariate is not configured correctly")
  }
  for (co in names(modelobj$CovariateMatrix)){
    if (!co %in% rownames(modelobj$info)){
      stop(paste("Covariate", co, "not in info matrix"))
    }
    if (any(is.na(modelobj$CovariateMatrix[,co]))){
      stop(paste("NAs for covariate", co))
    }
  
    ma <- max(modelobj$CovariateMatrix[,co])
    mi <- min(modelobj$CovariateMatrix[,co])
    num_unique <- length(unique(modelobj$CovariateMatrix[,co]))

    if (modelobj$info[co,"continuous"]==0 & co!="constant" & modelobj$info[co,"nlev"]==1){
      stop(paste("Only one level for categorical covariate", co))
    }
    if (modelobj$info[co,"continuous"]==0 & ma > modelobj$info[co,"nlev"]){
      stop(paste("Max value higher than nlev for covariate", co))
    }
    if (modelobj$info[co,"continuous"]==0 & mi < 1){
      stop(paste("Min value lower than 1 for covariate", co))
    }
    if (modelobj$info[co,"CAR"]==1 & modelobj$info[co,"random"]!=1){
      stop("CAR variable not designated as random effect.")
    }
    if (modelobj$info[co,"continuous"]==1 & modelobj$info[co,"nlev"]!=1){
      stop(paste("nlev wrongly configured for continuous variable", co))
    }
    if (modelobj$info[co,"interaction"]==1 & modelobj$info[co,"in.landings"]!=1){
      stop(paste("Interaction specified for covariate that are not in landings", co))
    }
    if (modelobj$info[co,"random"]==0 & modelobj$info[co,"continuous"]==0 & num_unique!=modelobj$info[co,"nlev"]){
      stop(paste("Not all values present for fixed covariate", co, "(samples)"))
    }
    if (modelobj$info[co,"random"]==1 & modelobj$info[co,"in.landings"]==0 & modelobj$info[co,"continuous"]==0 & num_unique!=modelobj$info[co,"nlev"]){
      stop(paste("Nlev is incorrectly set for random effect not in landings: ", co, "(unique samples:", num_unique, "nlev:", modelobj$info[co,"nlev"], ")"))
    }
    if (modelobj$info[co,"CAR"]==1 & is.null(modelobj$CARNeighbours)){
      stop(paste("CAR variable specified as", co, "but CARneighbours not specified"))
    }
    if (modelobj$info[co,"CAR"]==1 & !is.null(modelobj$CARNeighbours)){
      if (modelobj$info[co,"nlev"]>1){
        if (max(modelobj$CARNeighbours$idNeighbours)>modelobj$info[co,"nlev"] | max(modelobj$CARNeighbours$idNeighbours)<1){
          stop(paste("Neigbour matrix not consistent with nlev for CAR variable", co))
        }
        #
        # Checked with Hanne that this condition can be relaxed. ECA will tolarate areas with no neighbours, if
        # all fixed effects has been sampled for that area.
        #
        if (modelobj$info[co,"CAR"]==1 & (any(modelobj$CARNeighbours$numNeighbours<1) | length(modelobj$CARNeighbours$numNeighbours) < modelobj$info[co,"nlev"])){
          stop(paste("CAR variable specified as", co, "but some areas are missing neighbours in the data."))
        }
        if (modelobj$info[co,"CAR"]==1 & sum(modelobj$CARNeighbours$numNeighbours) != length(modelobj$CARNeighbours$idNeighbours)){
          stop(paste("CAR variable specified as", co, "numNeigbours is not consistent with idNeigbours"))
        }
        if (modelobj$info[co,"CAR"]==1){
          asymmetric_pairs <- ""
          for (i in 1:modelobj$info[co,"nlev"]){
            last_i <- sum(modelobj$CARNeighbours$numNeighbours[1:i])
            num_i <- modelobj$CARNeighbours$numNeighbours[i]
            neighbours_i <- modelobj$CARNeighbours$idNeighbours[(last_i-num_i+1):last_i]
            for (j in 1:modelobj$info[co,"nlev"]){
              last_j <- sum(modelobj$CARNeighbours$numNeighbours[1:j])
              num_j <- modelobj$CARNeighbours$numNeighbours[j]
              neighbours_j <- modelobj$CARNeighbours$idNeighbours[(last_j-num_j+1):last_j]

              ineighbourofj <- i %in% neighbours_j
              jneighbourofi <- j %in% neighbours_i

              if (ineighbourofj!=jneighbourofi){
                asymmetric_pairs <- paste(asymmetric_pairs, " (",i,",", j, ") ", sep="")
              }
            }
          }
          if (nchar(asymmetric_pairs)>0){
            stop(paste("CAR variable specified as", co, "but neighbour matrix is not symmetric (i is neighbour of j, but not j of i, or vice versa). Asymmetric pairs: ", asymmetric_pairs))
          }
        }
      }
    }
  }
}

#' @noRd
check_data_matrix <- function(modelobj){
  lastsample <- max(modelobj$DataMatrix$samplingID)
  if (!lastsample==nrow(modelobj$CovariateMatrix)){
    stop("sampling ids does not equal the number of rows in covariate matrix")
  }

}
#' checks that specification of covariates are OK
#' @noRd
check_covariates <- function(modelobject){
  check_cov_vs_info(modelobject)
}

#' checks that agelenght is configured correctly
#' @noRd
checkAgeLength<-function(agelength, num_tolerance = 1e-10){
  check_columns_present(agelength$DataMatrix, c("age", "part.year", "lengthCM", "samplingID", "partnumber", "partcount"))
  check_none_missing(agelength$DataMatrix, c("lengthCM", "samplingID", "partnumber")) #Seems to need at least some partnumbers, clear up doc for Reca::eca.estimate

  samplesPrCatch <- stats::aggregate(list(partCount=agelength$DataMatrix$partnumber), by=list(samplingID=agelength$DataMatrix$samplingID), FUN=function(x){length(unique(x))})
  samplesPrCatchLT1 <- samplesPrCatch[samplesPrCatch$partCount > 1,]
  if (nrow(samplesPrCatchLT1) > 0){
    needpartnumber <- agelength$DataMatrix[agelength$DataMatrix$samplingID %in% samplesPrCatchLT1[, "samplingID"],]
    if (any(is.na(needpartnumber$partcount))){
      stop("Missing values for partcount where several samples are taken from a catch")
    }
  }
  check_data_matrix(agelength)
  check_covariates(agelength)
  if (any(!is.na(agelength$DataMatrix$part.year) & agelength$DataMatrix$part.year<=0)){
    stop("part.year must be in <0,1]")
  }
  if (any(!is.na(agelength$DataMatrix$part.year) & agelength$DataMatrix$part.year>1)){
    stop("part.year must be in <0,1]")
  }
  if (!is.null(agelength$AgeErrorMatrix) & (any(is.na(agelength$AgeErrorMatrix)) || any(agelength$AgeErrorMatrix>1) || any(agelength$AgeErrorMatrix<0))){
    stop("Invalid values in age error matrix")
  }
  if (!is.null(agelength$AgeErrorMatrix)){
    if (any(abs(colSums(agelength$AgeErrorMatrix)-1)>num_tolerance)){
      stop("Columns of age error matrix does not sum to 1")
    }
  }
  
}
#' checks that weightlenght is configured correctly
#' @noRd
checkWeightLength<-function(weightlength, landings){
  check_columns_present(weightlength$DataMatrix, c("weightKG", "lengthCM", "samplingID", "partnumber", "partcount"))
  check_none_missing(weightlength$DataMatrix, c("lengthCM", "samplingID", "partnumber", "weightKG"))

  samplesPrCatch <- stats::aggregate(list(partCount=weightlength$DataMatrix$partnumber), by=list(samplingID=weightlength$DataMatrix$samplingID), FUN=function(x){length(unique(x))})
  samplesPrCatchLT1 <- samplesPrCatch[samplesPrCatch$partCount > 1,]
  if (nrow(samplesPrCatchLT1) > 0){
    needpartnumber <- weightlength$DataMatrix[weightlength$DataMatrix$samplingID %in% samplesPrCatchLT1["samplingID"],]
    if (any(is.na(needpartnumber$partcount))){
      stop("Missing values for partcount where several samples are taken from a catch")
    }
  }

  check_data_matrix(weightlength)
  check_covariates(weightlength)
}

#' checks that covariates are compatible between model and landings
#' @noRd
checkCovariateConsistency <- function(modelobj, landingscov){
  inlandings <- rownames(modelobj$info[modelobj$info[,"in.landings"]==1,])
  if (any(!(inlandings %in% names(landingscov)))){
    browser()
    stop("some covariates labeled as in.landings are not found in corresponding covariate matrix in landings")
  }

  landingscoovariates <- names(landingscov)[names(landingscov) %in% inlandings]
  if (!all(inlandings==landingscoovariates)){
    stop("Covariates are not ordered consistently in info matrix and landings")
  }
  modelcoovariates <- names(modelobj$CovariateMatrix)[names(modelobj$CovariateMatrix) %in% inlandings]
  if (!all(modelcoovariates==landingscoovariates)){
    stop("Covariates are not ordered consistently in model and landings")
  }

  # check that all cells exists in landings
  # constant is in inlandings, so case of length(inlandings) == 1, need not be handled.
  if (length(inlandings)>1){
    cellsSamples <- apply( modelobj$CovariateMatrix[,inlandings] , 1 , paste , collapse = "/" )
    cellsLandings <- apply( landingscov[,inlandings] , 1 , paste , collapse = "/" )

    if (!all(unique(cellsSamples) %in% unique(cellsLandings))){
      missing <- unique(cellsSamples)[!(unique(cellsSamples) %in% unique(cellsLandings))]
      stop(paste("Not all sampled cells exist in landings (", length(missing), " missing)", sep=""))
    }
  }

  #check that all level are present for all fixed effects
  nonconfixedeffects <- rownames(modelobj$info[modelobj$info[,"random"]==0 & modelobj$info[,"continuous"]==0,])

  #if (modelobj$info[co,"random"]==0 & modelobj$info[co,"continuous"]==0 & num_unique!=modelobj$info[co,"nlev"]){
  #  stop(paste("Not all values present for fixed covariate", co, "(samples)"))
  #}
  for (co in nonconfixedeffects){
    num_unique <- length(unique(landingscov[,co]))
    if (num_unique!=modelobj$info[co,"nlev"]){
      stop(paste("Fixed effect", co, "does not have values for all corresponding landings"))
    }
  }

  #check that the number of combinations of fixed effects in samples equal those in landing
  samp <- unique(modelobj$CovariateMatrix[,nonconfixedeffects])
  land <- unique(landingscov[,nonconfixedeffects])

  sample_combos <- nrow(samp)
  land_combos <- nrow(land)
  if (is.null(sample_combos) & is.null(land_combos) & length(sample_combos)!=length(land_combos)){
    stop("Not all combinations of fixed effects are sampled")
  }
  else if (sample_combos != land_combos){
    stop("Not all combinations of fixed effects are sampled")
  }

}

#' checks formatting on landing cov-matrices
#' @noRd
check_landings_cov <- function(cov){
  if (!all(cov$midseason>0 & cov$midseason<=1)){
    stop("midseason must be in <0,1]")
  }
  naerrors <- c()

  for (i in 1:ncol(cov)){
    if (any(is.na(cov[,i]))){
      naerrors <- c(naerrors, names(cov)[i])
    }
    if (length(naerrors)>0){
      stop(paste("NAs in landings: ", paste(naerrors, collapse=",")))
    }
  }
}

#' checks that landings are specified correctly
#' @noRd
checkLandings <- function(landings){
  if (nrow(landings$AgeLengthCov) != nrow(landings$WeightLengthCov)){
    stop("number of rows in landings covariate matrices does not match")
  }
  if (nrow(landings$AgeLengthCov) != length(landings$LiveWeightKG)){
    stop("length of weight vector does not match number of rows in covariate matrices in landings.")
  }
  check_landings_cov(landings$AgeLengthCov)
  check_landings_cov(landings$WeightLengthCov)
}

#' @param stage: specify the stage of sanitation for, only parameters that have implications for data-formatting ("datapred"), before Reca::eca.estimate ("parameterize") or before Reca::eca.predict ("predict")
#' @noRd
checkGlobalParameters <- function(globalparameters, agelength, weightlength, stage=c("dataprep", "parameterize", "predict")){

  stage <- match.arg(stage, stage)
  
  if (stage == "parameterize"){
    required <- c("nSamples", "thin", "burnin", "lengthresCM", "maxlength", "minage", "maxage", "resultdir", "fitfile", "delta.age", "age.error", "lgamodel", "CC", "CCerror")
    missingnames <- required[!(required %in% names(globalparameters))]
    if (length(missingnames) > 0){
      stop(paste("Some required global parameters are missing:", paste(missingnames, collapse=",")))
    }
    nanames <- names(globalparameters)[(names(globalparameters) %in% required) & is.na(globalparameters)]
    if (length(nanames) > 0){
      stop(paste("Some required global parameters are NA:", paste(nanames, collapse=",")))
    }
  }
  
  if (stage == "predict"){
    required <- c("nSamples", "thin", "burnin", "lengthresCM", "maxlength", "minage", "maxage", "resultdir", "fitfile", "predictfile", "delta.age", "age.error", "lgamodel", "CC", "CCerror", "caa.burnin")
    missingnames <- required[!(required %in% names(globalparameters))]
    if (length(missingnames) > 0){
      stop(paste("Some required global parameters are missing:", paste(missingnames, collapse=",")))
    }
    nanames <- names(globalparameters)[(names(globalparameters) %in% required) & is.na(globalparameters)]
    if (length(nanames) > 0){
      stop(paste("Some required global parameters are NA:", paste(nanames, collapse=",")))
    }
  }
  
  if (stage == "dataprep"){
    required <- c("lengthresCM", "maxlength", "minage", "maxage", "age.error", "CC", "CCerror")
    missingnames <- required[!(required %in% names(globalparameters))]
    if (length(missingnames) > 0){
      stop(paste("Some required global parameters are missing:", paste(missingnames, collapse=",")))
    }
    nanames <- names(globalparameters)[(names(globalparameters) %in% required) & is.na(globalparameters)]
    if (length(nanames) > 0){
      stop(paste("Some required global parameters are NA:", paste(nanames, collapse=",")))
    }
  }
  
  if (length(globalparameters$lengthresCM)==0 || is.na(globalparameters$lengthresCM)){
    stop("Length resolution not set (lengthresCM)")
  }
  if (max(agelength$DataMatrix$age, na.rm=T)>globalparameters$maxage){ #ages is checked for nas elsewere
    stop(paste("Parameter maxage", globalparameters$maxage, "is smaller than maximal age in samples (", max(agelength$DataMatrix$age, na.rm=T), ")"))
  }
  if (min(agelength$DataMatrix$age, na.rm=T)<globalparameters$minage){ #ages is checked for nas elsewere
    stop(paste("Parameter minage", globalparameters$minage, "is larger than minimal age in samples (", min(agelength$DataMatrix$age, na.rm=T), ")"))
  }
  if (max(weightlength$DataMatrix$lengthCM, na.rm=T)>globalparameters$maxlength){ #lengths are checked for nas elsewere
    stop(paste("Parameter maxlength (", globalparameters$maxlength, ") is smaller than maximal length in samples (", max(weightlength$DataMatrix$lengthCM, na.rm=T), ")"))
  }
  if (!globalparameters$age.error & !is.null(agelength$AgeErrorMatrix)){
    warning("Age error matrix set, but age.error parameter set to FALSE.")
  }
  if (globalparameters$age.error & is.null(agelength$AgeErrorMatrix)){
    stop("Age error matrix not set, but age.error parameter set to TRUE.")
  }
  if (globalparameters$age.error){
    if (nrow(agelength$AgeErrorMatrix) != (globalparameters$maxage-globalparameters$minage+1)){
      stop(paste0("Rows of age matrix does not match minage maxage parameters (", nrow(agelength$AgeErrorMatrix), " vs ", globalparameters$minage, ":", globalparameters$maxage, ")"))
    }
    if (as.numeric(row.names(agelength$AgeErrorMatrix))[1] != globalparameters$minage){
      stop("First age of age error matrix does not correspond to minage")
    }
    if (as.numeric(row.names(agelength$AgeErrorMatrix))[length(row.names(agelength$AgeErrorMatrix))] != globalparameters$maxage){
      stop("Last age of age error matrix does not correspond to maxage")
    }
    if (ncol(agelength$AgeErrorMatrix) != (globalparameters$maxage-globalparameters$minage+1)){
      stop(paste0("Columns of age matrix does not match minage maxage parameters (", ncol(agelength$AgeErrorMatrix), " vs ", globalparameters$maxage, ":", globalparameters$minage, ")"))
    }
  }

}

#' Input sanitation for Reca
#' @description 
#'  Performs input sanitation for Reca
#' @details 
#'  The main interfaces to the Reca-package is \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}.
#'  This have strict formatting requirements, well documented in the help pages for these functions,
#'  but they do not perform much input sanitation. 
#'  This function checks that data is formatted in accordance with the requirements. 
#'  
#'  The input formats are shared between the \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#'  and the formatting requirements are largely overlapping. Sanitation has therefore been conseptually divided into three stages,
#'  and which checks are to be performed is controlled by the argument 'stage'. The three stages are:
#'  \describe{
#'   \item{dataprep}{check that data is correctly and consistently formatted, and check that all global parameters that have implications for data formatting are provided.}
#'   \item{parameterize}{check that arguments are valid input to \code{\link[Reca]{eca.estimate}}}
#'   \item{predict}{check that arguments are valid input to \code{\link[Reca]{eca.predict}}}
#'  }
#'  
#'  Errors or warnings are raised on any issue, and this function does not have a meaningful return value.
#'  
#'  All or some inputs to the Reca-functions may be provided for sanitation, and available checks will be run accordingly.
#'  As Reca impose some restriction on consistency in formatting between the different arguments,
#'  some checks require certain combination of arguments to be provided. For instance, checks on
#'  consistent encoding of variables in 'AgeLength' and 'Landings' are only performed if both 'AgeLength' and 'Landings' are provided.
#'  
#' @param AgeLength list that is to be provided as the AgeLength argument to \code{\link[Reca]{eca.estimate}} or \code{\link[Reca]{eca.predict}}
#' @param WeightLength list that is to be provided as the AgeLength argument to \code{\link[Reca]{eca.estimate}} or \code{\link[Reca]{eca.predict}}
#' @param Landings list that is to be provided as the AgeLength argument to \code{\link[Reca]{eca.estimate}} or \code{\link[Reca]{eca.predict}}
#' @param GlobalParameters list that is to be provided as the AgeLength argument to \code{\link[Reca]{eca.estimate}} or \code{\link[Reca]{eca.predict}}
#' @param stage character specifying which stage the sanitation should be performed for. See details.
#' @return NULL
#' @concept Reca functions
#' @export  
sanitizeRecaInput <- function(AgeLength=NULL, WeightLength=NULL, Landings=NULL, GlobalParameters=NULL, stage=c("dataprep", "parameterize", "predict")){
  
  stage <- match.arg(stage, stage)
  
  if (!is.null(WeightLength) & !is.null(Landings)){
    checkWeightLength(WeightLength, Landings)    
  }
  if (!is.null(AgeLength)){
    checkAgeLength(AgeLength)    
  }
  if (!is.null(AgeLength) & !is.null(Landings)){
    checkCovariateConsistency(AgeLength, Landings$AgeLengthCov)    
  }
  if (!is.null(WeightLength) & !is.null(Landings)){
    checkCovariateConsistency(WeightLength, Landings$WeightLengthCov)    
  }
  if (!is.null(Landings)){
    checkLandings(Landings)    
  }
  
  if (!is.null(WeightLength) & !is.null(WeightLength) & !is.null(GlobalParameters)){
    checkGlobalParameters(GlobalParameters, AgeLength, WeightLength, stage=stage)    
  }
  
  return(NULL)
}
