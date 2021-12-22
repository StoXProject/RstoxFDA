#' @noRd
filterVonBsigmaMask <- function(individuals, 
                                Linf,
                                K,
                                sigma,
                                kAl,
                                kAu=kAl,
                                ageCol="IndividualAge", 
                                lengthCol="IndividualTotalLength"){
  
  if (!(ageCol %in% names(individuals))){
    stop(paste("Colummn", ageCol, "not found in 'individuals'."))
  }
  if (!(lengthCol %in% names(individuals))){
    stop(paste("Colummn", lengthCol, "not found in 'individuals'."))
  }
  
  # It is important that this function reproduce ECA functionality
  # from ECA 3.x / 4 documentation:
  # L=Linf*(1-exp(-K*(AGE)))*exp(+-k*sigmaL)
  # The formulation above is not identical to the manual,
  # but seems equivalent and is lifted from Rd-documentation files in
  # the source code of one ECA version.
  
  upper <- Linf*(1-exp(-K*(individuals[[ageCol]])))*exp(kAu*sigma)
  lower <- Linf*(1-exp(-K*(individuals[[ageCol]])))*exp(-kAl*sigma)
  
  mask <- is.na(upper) | 
             is.na(lower) |
             is.na(individuals[[lengthCol]]) |
             is.na(individuals[[ageCol]]) |
             (individuals[[lengthCol]] < upper &
                individuals[[lengthCol]] > lower)

  return(mask)
}

#' Filter length-age outliers
#' @description 
#'  Removes fish records that fall outside an acceptable age-length region
#'  defined by a von-Bertalanffy growth curve:
#'  
#'  length=Linf(1-exp(-K\*age))\*exp(epsilon); epsilon~N(0,sigma^2)
#'  
#'  with parameters corresponding to arguments to this function.
#'  
#' @details 
#'  This function is intended to provide the same filtering that is offered in ECA 3.x and ECA 4.x
#'  for removing outliers based on von-Bertalanffy growth relationships, and
#'  function arguments are named to correspond to the naming convention used in ECA.
#'  
#'  Fish with lengths that fall outside the range from:
#'  Linf\*(1-exp(-K\*(AGE)))\*exp(kAu\*sigma)
#'  to
#'  Linf\*(1-exp(-K\*(AGE)))\*exp(-kAl\*sigma)
#'  are removed
#' 
#'  any records with missing length or age is not removed.
#'  
#'  The filtering may be sensitive to the resolution of age for small fish.
#'  When age is counting completed winter-ring growth, 
#'  an acceptable resolution may be achieved by adding M/12 to the age of each fish,
#'  where M is the month-number for the date of catch.
#'  
#'  Note that kAl and kAu are given on a log scale, so that the acceptable region
#'  is not symmetric around the growth curve when kAl=kAu.
#'  
#' @param individuals \code{\link[data.table]{data.table}} of fish records
#' @param Linf Asymptotic length for the von-Bertalanffy growth curve
#' @param K The growth coefficient for the von-Bertalanffy growth curve
#' @param sigma The standard deviation of length for the von-Bertalanffy growth curve
#' @param kAl Number of standard deviations (on a log scale) that defines the lower limit of the acceptable region
#' @param kAu Number of standard deviations (on a log scale) that defines the upper limit of the acceptable region
#' @param ageCol name of column in 'individuals' that contain fish age in decimal years. Default correspond to the Individual level of \code{\link[RstoxData]{StoxBioticData}}, but see details.
#' @param lengthCol name of column in 'individuals' that contain fish length in the same unit as Linf and sigma. Default correspond to the Individual level of \code{\link[RstoxData]{StoxBioticData}}
#' @return \code{\link[data.table]{data.table}}, like individuals, but with some records removed.
#' @export
filterVonBsigma <- function(individuals, 
                      Linf,
                      K,
                      sigma,
                      kAl,
                      kAu=kAl,
                      ageCol="IndividualAge", 
                      lengthCol="IndividualTotalLength"){
  

  
  return(individuals[filterVonBsigmaMask(individuals, 
                                         Linf,
                                         K,
                                         sigma,
                                         kAl,
                                         kAu,
                                         ageCol,
                                         lengthCol),])
}

#' @noRd
filterLogLinearMask <- function(individuals, 
                            logalfa,
                            beta,
                            sigma,
                            kAl,
                            kAu=kAl,
                            weightCol="IndividualRoundWeight", 
                            lengthCol="IndividualTotalLength"){
  
  if (!(weightCol %in% names(individuals))){
    stop(paste("Colummn", weightCol, "not found in 'individuals'."))
  }
  if (!(lengthCol %in% names(individuals))){
    stop(paste("Colummn", lengthCol, "not found in 'individuals'."))
  }
  
  # It is important that this function reproduce ECA functionality
  # from ECA 3.x / 4 documentation:
  # log(W)=alfa+beta*log(L),
  # in the parameter file alfa is referred to as logalfa, so for clarity:
  # log(W)=logalfa+beta*log(L).
  #
  # Equivalently from Rd-documentation files in
  # the source code of one ECA version:
  # W=alfa*L^beta*exp(+-k*sigmaW)
  # which makes clear how the sigma is incorporated
  # in that expression alfa is not on log scale
  
  upper <- exp(logalfa)*(individuals[[lengthCol]]**beta)*exp(kAu*sigma)
  lower <- exp(logalfa)*(individuals[[lengthCol]]**beta)*exp(-kAl*sigma)
  
  mask <- is.na(upper) | 
    is.na(lower) |
    is.na(individuals[[weightCol]]) |
    is.na(individuals[[lengthCol]]) |
    (individuals[[weightCol]] < upper &
       individuals[[weightCol]] > lower)
  
  return(mask)
  
}

#' Filter length-weight outliers
#' @description 
#'  Removes fish records that fall outside an acceptable weight-length region
#'  defined by the log-linear model
#'  
#'  weight=alfa\*length^beta\*exp(epsilon); epsilon~N(0,sigma^2)
#'  
#'  with alfa=exp(logalfa), logalfa and other parameters corresponding to arguments to this function.
#'  
#' @details 
#'  This function is intended to provide the same filtering that is offered in ECA 3.x and ECA 4.x
#'  for removing outliers based on a log-linear weight-length model, and
#'  function arguments are named to correspond to the naming convention used in ECA.
#'  
#'  Records are removed if their weights that outside the range from:
#'  alfa\*L^beta\*exp(kAu\*sigma)
#'  to
#'  alfa\*L^beta\*exp(-kAl\*sigma)
#' 
#'  any records with missing length or weight is not removed.
#'  
#'  Note that kAl and kAu are given on a log scale, so that the acceptable region
#'  is not symmetric around the growth curve when kAl=kAu.
#'  
#' @param individuals \code{\link[data.table]{data.table}} of fish records
#' @param logalfa The alfa parameter (on a log scale) for the log-linear model
#' @param beta The beta parameter for the log-linear model
#' @param sigma The standard deviation of weight for the log-linear model
#' @param kAl Number of standard deviations (on a log scale) that defines the lower limit of the acceptable region
#' @param kAu Number of standard deviations (on a log scale) that defines the upper limit of the acceptable region
#' @param weightCol name of column in 'individuals' that contain fish weight (in a unit corresponding to alfa and beta). Default correspond to the Individual level of \code{\link[RstoxData]{StoxBioticData}}, but see details.
#' @param lengthCol name of column in 'individuals' that contain fish length (in a unit corresponding to alfa and beta). Default correspond to the Individual level of \code{\link[RstoxData]{StoxBioticData}}
#' @return \code{\link[data.table]{data.table}}, like individuals, but with some records removed.
#' @export
filterLogLinear <- function(individuals, 
                            logalfa,
                            beta,
                            sigma,
                            kAl,
                            kAu=kAl,
                            weightCol="IndividualRoundWeight",
                            lengthCol="IndividualTotalLength"){
  
  return(individuals[filterLogLinearMask(individuals, 
                                         logalfa,
                                         beta,
                                         sigma,
                                         kAl,
                                         kAu,
                                         weightCol,
                                         lengthCol),])
}


