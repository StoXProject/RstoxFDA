#
# Internal functions. Shorthand for common tasks, and standardizing of messages
#

#' Issues warning prefixed with StoX:, necessary to be picked up by GUI
#' @noRd
stoxWarning <- function(msg){
  warning(paste("StoX:", msg))
}

#' Check if parameter is given
#' @noRd
isGiven <- function(value=NULL){
  if (length(value) > 1){
    return(TRUE)
  }
  if (length(value) == 1){
    if (is.character(value)){
      if (value == ""){
        return(FALSE)
      }
    }
    return(TRUE)
  }
  if (length(value) == 0){
    return(FALSE)
  }

}

#' For pattern where a usedefault flag overrides other options
#' warning is not issued if the argument is a vector to accomodate combinatin of default an option notations in default arguments
#' @noRd
getDefault <- function(argument, argName, useDefault, default){
  if (!isGiven(argument) | useDefault){
    if (isGiven(argument) & length(argument)==1){
      warning(paste("Argument '", argName, "' is ignored because default settings are chosen.", sep=""))
    }
    return(default)
  }
  return(argument)
}

#' For giving error if a mandatory argument is not provided
#' @noRd
checkMandatory <- function(argument, argName){
  if (!isGiven(argument)){
    stop(paste("Argument '", argName, "' must be provided.", sep=""))
  }
}

#' For checking if a chosen option is among valid options.
#' argument is considered mandatory. Gives an error if not provided.
#' should always be run after getDefault, if defaults are set.
#' if a vector is provided. The first option is chosen via match.arg (suitable for functions with regular R defaults set to a vector)
#' @noRd
checkOptions <- function(argument, argName, validOptions){
  if (isGiven(argument)){
    if (length(argument)>1){
      argument <- match.arg(argument, argument)      
    }
    if (!(argument %in% validOptions)){
      stop(paste("Does not recognize option, '", argument, "' for ", argName, ". Valid options are: ", paste(validOptions, collapse=","), sep=""))
    }
    return(argument)
  }
  else{
    checkMandatory(argument, argName)
  }
}

#' Rounds the specified number of decimals
#' @noRd
desimals <- function(x, Decimals=integer()){
  
  if (isGiven(Decimals)){
    return(round(x, digits = Decimals))
  }
  
  return(x)
  
}

#' @noRd
setDecimals <- function(table, columns, decimals){
  for (co in columns){
    table[[co]] <- desimals(table[[co]], decimals)
  }
  return(table)
}

#' modifies unit by reference (note: no return value)
#' @noRd
setUnits <- function(table, columns, unit, quantity){
  for (co in columns){
    table[[co]] <- RstoxData::setUnit(table[[co]], RstoxData::findUnit(quantity, unit))
  }
  return(table)
}

#' Make tempdir for Reca results
#' returns path
#' @noRd
makeTempDirReca <- function(dirname="Recadir"){
  fpath <- file.path(tempdir(), dirname)
  if (dir.exists(fpath)){
    unlink(fpath, recursive = T)
  }
  dir.create(fpath)
  return(fpath)
}

#' Remove tempdir for Reca results
#' @noRd
removeTempDirReca <- function(fpath){
  unlink(fpath, recursive = T)
  write("Removing tempdir:", stderr())
  write(fpath, stderr())
  if (dir.exists(fpath)){
    warning(paste("Could not remove tempdir: ", fpath))
  }
}

#' warns user about deprection
#' @noRd
deprecationWarning <- function(functionName, deprecationTime, message=NULL){
  warningstring <- paste("The function", functionName, "has been marked for deprecation since", deprecationTime, "and may be removed in future releases.")
  warningstring <- paste(warningstring, " See functon help page (?", functionName, ") for suggestions for replacement function.")
  if (!is.null(message)){
    warningstring <- paste(warningstring, message)
  }
  stoxWarning(warningstring)
}

#' Construct truncated vector of strings with aal elements more than maxsize replaced by a single character  "..."
#' @noRd
truncateStringVector <- function(missing, maxsize=5){
  
  if (length(missing)>maxsize){
    missing <- c(missing[1:maxsize], "...")
  }
  return(missing)
}