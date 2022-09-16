#' replacement for sp::transform, not using rgdal
#' 
#' @noRd
transformSpatialPolygons <- function(x, CRSobj){
  obj <- sf::st_as_sf(x)
  trans <- sf::st_transform(obj, CRSobj)
  return(sf::as_Spatial(trans))
}

#' read tab separated file
#' @noRd
readTabSepFile <- function(filepath, encoding="UTF-8", col_classes = NULL, col_names = NULL, trim_ws=T){
  
  
  #check headers
  header <- utils::read.table(filepath, sep="\t", fileEncoding = encoding, comment.char = "#", strip.white = trim_ws, header=T, na.strings = c("", " "))
  
  if (length(col_names)>0){
    missing <- col_names[!(col_names %in% names(header))]
    if (length(missing)>0){
      stop(paste("Resource file does not have required columns:", paste(missing, collapse=", ")))
    }
  }
  
    
  tab <- utils::read.table(filepath, sep="\t", fileEncoding = encoding, colClasses = col_classes, comment.char = "#", strip.white = trim_ws, header=T, na.strings = c("", " "))
  tab <- data.table::as.data.table(tab)

  for (n in names(tab)){
    if (length(date) == 1 & ("Date" %in% class(date))){
      stop("Date is not supported, use POSIXct")
    }
  }


  return(tab)
}

#' Get temporal categories
#' @description define a categorical variable based for a vector of dates
#' @param date POSIXct() vector of dates
#' @param temporalType character() specify the kind of temporal category to define: "quarter", "week" or "custom"
#' @param seasonal logical() specify whether the temporal category should be seasonal (e.g. week 1 in year x is the same category as week 1 in year y)
#' @param FUN function(day, month) mapping a day (1-based integer()) and month (1-based integer()) to a value (character()) for the categorical variable to be defined.
#' @examples
#'  #get current quarter
#'  quarter <- categoriseDate(Sys.time())
#'
#'  #get custom non-seasonal category
#'  dates <- as.POSIXct(c(
#'     "2018-01-17 21:37:29 CET",
#'     "2019-02-28 21:37:29 CET",
#'     "2019-12-28 21:37:29 CET"))
#'  inDecember <- function(day, month){if(month==12){return("Yes")};return("No")}
#'  categoriseDate(dates, temporalType = "custom", FUN=inDecember, seasonal = FALSE)
#'
#' @return character() a vector of values for the categorical variable, corresponding to the dates in 'date'
#' @family temporal coding functions
#' @export
categoriseDate <- function(date, temporalType="quarter", seasonal=T, FUN=NULL){

  if (any(is.na(date))){
    stop("NAs in date.")
  }

  if (length(date) == 0){
    return(character())
  }


  if (!is.null(FUN) & temporalType != "custom"){
    stop("Parameter 'FUN' may only be used with temporalType custom")
  }

  output <- NULL

  if (temporalType == "week"){
    output <- paste("W", strftime(date, format="%V"), sep="")
  }
  else if (temporalType == "month"){
    output <- strftime(date, format="%B")
  }
  else if (temporalType == "quarter"){
    output <- paste("Q", (as.integer(strftime(date, format="%m"))-1)%/%3+1L, sep="")
  }
  else if (temporalType == "custom"){
    vf <- Vectorize(FUN, SIMPLIFY=T)
    output <- vf(as.integer(strftime(date, format="%d")), as.integer(strftime(date, format="%m")))
  }
  else{
    stop(paste("Temporal type", temporalType, "not recognized."))
  }

  if (!seasonal){
    year <- strftime(date, format="%Y")
    output <- paste(output, year, sep="-")
  }

  return(output)

}

#' Convert codes.
#' @description
#'  Apply conversion table, perform approriate checks and return result.
#' @details
#'  Will stop with error if any codes can not be converted, or if any entries are NA.
#'  Require all codes (original and converted) to be character().
#' @param code character() with original codes
#' @param conversionTable list() mapping code to converted code.
#' @return character() with converted codes
#' @examples
#'  gearConversion <- list()
#'  gearConversion["TBS"] <- "OTB"
#'  gearConversion["TBN"] <- "OTB"
#'  gearConversion["OTB"] <- "OTB"
#'  convertCodes(c("TBS", "TBN", "OTB"), gearConversion)
#' @family parameter conversion functions
#' @export
convertCodes <- function(code, conversionTable){

  if (length(code) == 0){
    return(character())
  }

  if (!is.character(code)){
    stop("Codes must be character()")
  }

  if (!is.character(unlist(conversionTable))){
    warning("Coercing converted codes to character")
  }

  if (is.null(names(conversionTable))){
    stop("Conversion table must be indexed by character(). names(conversionTable) is NULL.")
  }

  if (any(is.na(code))){
    stop("NAs in codes")
  }

  if (!all(code %in% names(conversionTable))){
    missing <- unique(code[!(code %in% names(conversionTable))])
    stop(paste("Conversion not defined for all codes. Missing for:", paste(missing, collapse=", ")))
  }

  return(as.character(conversionTable[code]))
}

#' Append area code
#' @details
#'  Appends a column with an area code to a table, based on positions.
#'  
#'  If polygons are overlapping, so that one point may be in several polygons, 
#'  an arbitrary choice is made and a warning is issued.
#' @param table data.table to be annotated.
#' @param areaPolygons \code{\link[sp]{SpatialPolygonsDataFrame}}
#' @param latName name of WGS84 lat column in 'table'
#' @param lonName name of WGS84 lon column in 'table
#' @param colName name of column to be appended to 'table'
#' @param StratumName name of column in 'areaPolygons' that identify the area name
#' @return 'table' with the area appended in the column 'colName'
#' @family spatial coding functions
#' @export
appendAreaCode <- function(table, areaPolygons, latName, lonName, colName, StratumName="StratumName"){
  if (!data.table::is.data.table(table)){
    stop("Parameter 'table' must be a data.table")
  }
  if (colName %in% names(table)){
    stop(paste("Column name", colName, "already exists."))
  }
  if (!(latName %in% names(table)) | !is.numeric(table[[latName]])){
    stop(paste(latName, "(parameter 'latName') must be provided as a numeric column."))
  }
  if (!(lonName %in% names(table)) | !is.numeric(table[[lonName]])){
    stop(paste(lonName, "(parameter 'lonName') must be provided as a numeric column."))
  }
  if (any(is.na(table[[latName]]))){
    stop("Missing values in column: ", latName)
  }
  if (any(is.na(table[[lonName]]))){
    stop("Missing values in column: ", lonName)
  }
  
  pos <- sf::st_as_sf(table, coords=c(lonName, latName), crs = sp::CRS("EPSG:4326"))
  poly <- sf::st_transform(sf::st_as_sf(areaPolygons), crs = sp::CRS("EPSG:4326"))
  
  intersects <- sf::st_intersects(pos, poly)
  
  #
  # Polygon files are often prepared for visualization purposes,
  # small overlaps are common.
  # There we issue a warning, rather than an error.
  #
  if (any(sapply(intersects, length) > 1)){
    stoxWarning("Overlapping polygons: Some points are in several polygons, area code is arbitrarily chosen.")
  }
  
  indecies <- sapply(intersects, head, n=1)
  table[[colName]] <- areaPolygons[[StratumName]][indecies]

  return(table)
}

#' Append positions
#' @description
#'  Appends columns with positions to a data table, based on an area code.
#'
#'  Coordinates are retrieved from a \code{\link[sp]{SpatialPolygonsDataFrame}} and not calculated
#'  the exact defintion of the coordinates depend on how the polygons were constructed.
#'
#'  Datum and projection is not enforced, but a warning is issued if 'areaPolygons' does not pass some
#'  checks to verify that it is not a planar projection.
#'
#' @param table data.table to be annotated.
#' @param areaPolygons \code{\link[sp]{SpatialPolygonsDataFrame}}
#' @param areaName name of column that identifies the area in 'table'
#' @param latColName name of the latitdue column to be appended to 'table'
#' @param lonColName name of the longitude column to be appended to 'table'
#' @param StratumName name of the column in 'areaPolygons' that identifies the area.
#' @return 'table' with the positions appended in the columns 'latColName' and 'lonColName'.
#' @family spatial coding functions
#' @export
appendPosition <- function(table, areaPolygons, areaName, latColName, lonColName, StratumName="StratumName"){
  if (latColName %in% names(table)){
    stop(paste("Column name", latColName, "already exists."))
  }
  if (lonColName %in% names(table)){
    stop(paste("Column name", lonColName, "already exists."))
  }
  if (!(areaName %in% names(table))){
    stop(paste("Column name", areaName, "not found in 'table'."))
  }

  if (!startsWith(sf::st_crs(sf::st_as_sf(areaPolygons))$wkt, "GEOGCRS")){
    warning("could not verify projection of 'areaPolygons'")
  }
  
  mapping <- cbind(data.table::as.data.table(sp::coordinates(areaPolygons)), areaPolygons[[StratumName]])
  names(mapping) <- c(lonColName, latColName, areaName)

  newTab <- merge(table, mapping, by=areaName, all.x=T)

  return(newTab)
}
