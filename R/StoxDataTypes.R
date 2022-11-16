#' @noRd
is.POSIXct <- function(date){
  if ("POSIXct" %in% class(date)){
    return(TRUE)
  }

  return(FALSE)
}

#' @noRd
is.Date <- function(date){
  if ("Date" %in% class(date)){
    return(TRUE)
  }

  return(FALSE)
}


#' Trip Partition
#'
#' Partitioning of catch from a trip.
#'
#' @details
#' list with two members 'fractions' and 'groupDefinition'
#'
#' 'fractions' is a \code{\link[data.table]{data.table}} with columns:
#' \describe{
#'  \item{tripid}{trip identifier}
#'  \item{species}{species identifier}
#'  \item{groupid}{identifies the group the fraction is provided for. Groups are further specified in 'groupDefinition'}
#'  \item{fraction}{fraction of the total catch of species in the given group}
#' }
#'
#' 'groupDefinition' is a \code{\link[data.table]{data.table}} with columns:
#' \describe{
#'  \item{groupid}{identifies the group the defintion is provided for.}
#'  \item{...}{one or more custom columns definint the group}
#' }
#'
#' @name TripPartition
#' @concept Data types
#'
NULL

#' Metier table
#'
#' Table (\code{\link[data.table]{data.table}}) defining metiers.
#'
#' Fishing activity metiers are approximate decompositions of fleets commonly used in Europe.
#' This table defines an approximate definition of metiers by a custom gear code.
#'
#' Metiers are used in EU-regulations for EU-member states, and are therefore often required by ICES data formats
#' Metiers are formal strings encoding decomposition an idealized fleet or set of trips, where target species and gear are clearly and unambigiously identified.
#' The metier system recognize that these parameters are not always clearly and unamibigiously identified to the desired resolution, and allows for grouping and omission of some parameters, and for coding that information is missing.
#' This makes the metier system very flexible, but also provides poor standardization and ICES databases and data-calls may provide code-lists of allowed metiers, sometimes with different metiers being requested for different areas.
#' In effect metier annotations cannot be completely standardized, but must to be configurable through conversion tables like this. Often the annotation has to be approximate.
#'
#' The metier system is spesified by the EU - Data Collection Framework: \url{https://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier},
#' briefly a metier is fully specified by a string: <gear>_<target species>_<gear mesh size>_<selectivity device>_<selectivity device mesh size>_<vessel length class>,
#' with coding systems and grouping conventions defined for each of the variables.
#' Common trunctations of these strings are also used. E.g.:
#' Metier level 6 (no truncation): <gear>_<target species>_<gear mesh size>_<selectivity device>_<selectivity device mesh size>
#' Metier level 5 : <gear>_<target species>
#' Metier level 4 : <gear>
#' The term "metier" is also used for some derived systems, for instance some data-calls requests that a code for intended usage of catch (industrial vs human consumption) be appended to the metiers where gear and target species is missing.
#'
#' For example, in intercatch, the code OTB_DEF_>=120_0_0_all identifies bottom trawl with otter boards (OTB) targeting demershal fish (DEF), with mesh size larger than or equal to 120 mm, no selectivity device (0_0) and all vessel size (all)
#'
#' @details
#'  \describe{
#'   \item{metier}{character() metier-string, e.g.: OTB_DEF_>=120_0_0_all}
#'   \item{gearcode}{character() encoding gear}
#'   \item{target}{character(), optional, target species}
#'   \item{meshedGear}{logical(), optional, whether the gear is a meshed gear. Should be provided for all or none gears.}
#'   \item{lowerMeshSize}{integer(), optional, the lower mesh size to include in this metier. Should be provided for all rows where meshedGear is True, and not for other rows.}
#'   \item{upperMeshSize}{integer(), optional, the upper mesh size to include in this metier. Should be provided for all rows where meshedGear is True, and not for other rows.}
#'   \item{selectivityDevice}{character(), optional, encoding selectivity device.}
#'   \item{meshedSelectivityDevice}{logical(), optional, encoding selectivity device. Should be provided for all or none selectivity devices.}
#'   \item{selDevLowerMeshSize}{integer(), optional, the lower mesh size of selectivity device to include in this metier. Should be provided for all rows where meshedSelectivityDevice is True, and not for other rows.}
#'   \item{selDevUpperMeshSize}{integer(), optional, the upper mesh size of selectivity device to include in this metier. Should be provided for all rows where meshedSelectivityDevice is True, and not for other rows.}
#'  }
#'
#'  The metier-defining parameters are written in camelCase, parameters that may be used to distinguish applicability of different metierdefinitions are writter in UPPER case.
#'
#' @name MetierTable
#' @concept Data types
#'
NULL

#' Check if table is correctly formatted metier table
#' @param table \code{\link[RstoxFDA]{MetierTable}}
#' @param throwError if set errors are raised, if not, validity will be returned as T/F
#' @return validity
#' @concept Data types
#' @noRd
is.MetierTable <- function(table, throwError=F){
  
  if (!data.table::is.data.table(table)){
    if (throwError){
      stop("A metiertable must be a data.table")
    }
    return(FALSE)
  }
  if (!all(c("metier", "gearcode", "target", "meshedGear", "lowerMeshSize", "upperMeshSize", "selectivityDevice", "meshedSelectivityDevice", "selDevLowerMeshSize", "selDevUpperMeshSize") %in% names(table))){
    if (throwError){
      stop("Metiertable does not have the required columns")
    }
    return(FALSE)
  }
  if (!is.character(table$metier)){
    if (throwError){
      stop("The column 'metier' must be a character")
    }
    return(FALSE)
  }
  if (!is.character(table$gearcode)){
    if (throwError){
      stop("The column 'gearcode' must be a character")
    }
    return(FALSE)
  }
  if (!is.logical(table$meshedGear)){
    if (throwError){
      stop("The column 'meshedGear' must be a logical")
    }
    return(FALSE)
  }
  if (!is.numeric(table$lowerMeshSize)){
    if (throwError){
      stop("The column 'lowerMeshSize' must be an integer")
    }
  }
  if (!is.numeric(table$upperMeshSize)){
    if (throwError){
      stop("The column 'upperMeshSize' must be an integer")
    }
    return(FALSE)
  }
  if (!is.character(table$selectivityDevice)){
    if (throwError){
      stop("The column 'selectivityDevice' must be a character")
    }
    return(FALSE)
  }
  if (!is.logical(table$meshedSelectivityDevice)){
    if (throwError){
      stop("The column 'meshedSelectivityDevice' must be a logical")
    }
    return(FALSE)
  }
  if (!is.numeric(table$selDevLowerMeshSize)){
    if (throwError){
      stop("The column 'selDevLowerMeshSize' must be an integer")
    }
  }
  if (!is.numeric(table$selDevUpperMeshSize)){
    if (throwError){
      stop("The column 'selDevUpperMeshSize' must be an integer")
    }
    return(FALSE)
  }
  
  meshed <- table$meshedGear[!is.na(table$gearcode)]
  if (any(is.na(meshed)) & !all(is.na(meshed))){
    if (throwError){
      stop("The column 'meshedGear' has a value for some gears, but not all")
    }
    return(FALSE)
  }
  
  upperMesh <- table$upperMeshSize[!is.na(table$meshedGear) & table$meshedGear]
  if (any(is.na(upperMesh))){
    if (throwError){
      stop("The column 'upperMeshSize' is not provided for all meshed gears (where meshedGear is True)")
    }
    return(FALSE)
  }
  lowerMesh <- table$lowerMeshSize[!is.na(table$meshedGear) & table$meshedGear]
  if (any(is.na(lowerMesh))){
    if (throwError){
      stop("The column 'lowerMeshSize' is not provided for all meshed gears (where meshedGear is True)")
    }
    return(FALSE)
  }
  
  if (any((!is.na(table$lowerMeshSize) | !is.na(table$upperMeshSize)) & (is.na(table$meshedGear) | !table$meshedGear))){
    if (throwError){
      stop("Mesh sizes provided for gears that are not meshed (where meshedGear is missing or False)")
    }
    return(FALSE)
  }
  
  meshedSel <- table$meshedSelectivityDevice[!is.na(table$gearcode) & !is.na(table$selectivityDevice)]
  if (any(is.na(meshedSel)) & !all(is.na(meshedSel))){
    if (throwError){
      stop("The column 'meshedSelectivityDevice' has a value for some gears, but not all")
    }
    return(FALSE)
  }
  
  if (any((!is.na(table$lowerMeshSize) | !is.na(table$upperMeshSize)) & is.na(table$gear))){
    if (throwError){
      stop("Mesh sizes provided for gear where 'gear' is not given")
    }
    return(FALSE)
  }
  
  upperMeshSD <- table$selDevUpperMeshSize[!is.na(table$meshedSelectivityDevice) & table$meshedSelectivityDevice]
  if (any(is.na(upperMeshSD))){
    if (throwError){
      stop("The column 'selDevUpperMeshSize' is not provided for all meshed selectivty devices gears (where meshedSelectivityDevice is True)")
    }
    return(FALSE)
  }
  lowerMeshSD <- table$selDevLowerMeshSize[!is.na(table$meshedSelectivityDevice) & table$meshedSelectivityDevice]
  if (any(is.na(lowerMeshSD))){
    if (throwError){
      stop("The column 'selDevUpperMeshSize' is not provided for all meshed selectivty devices gears (where meshedSelectivityDevice is True)")
    }
    return(FALSE)
  }
  
  if (any((!is.na(table$selDevLowerMeshSize) | !is.na(table$selDevUpperMeshSize)) & (is.na(table$meshedSelectivityDevice) | !table$meshedSelectivityDevice))){
    if (throwError){
      stop("Mesh sizes provided for selectivity devices that are not meshed (where meshedSelectivityDevice is missing or False)")
    }
    return(FALSE)
  }
  
  if (any((!is.na(table$selDevLowerMeshSize) | !is.na(table$selDevUpperMeshSize)) & is.na(table$selectivityDevice))){
    if (throwError){
      stop("Mesh sizes provided for selectivity devices where 'selectivityDevice' is not given")
    }
    return(FALSE)
  }
  
  return(TRUE)
}

#' Check if input conforms to StoxBioticData.
#' Should perhaps be moved to RstoxData together with is.StoxLandingData
#' @noRd
is.StoxBioticData <- function(StoxBioticData, raiseErrors=F){
  if (!is.list(StoxBioticData)){
    return(FALSE)
  }
  if (!all(sapply(StoxBioticData, FUN=data.table::is.data.table))){
    return(FALSE)
  }
  if (!all(c("Cruise", "Station", "Haul", "SpeciesCategory", "Sample", "Individual") %in% names(StoxBioticData))){
    return(FALSE)
  }
  if (any(duplicated(StoxBioticData$Cruise$Cruise))){
    if (raiseErrors){
      stop("Duplicate Cruise keys")
    }
    return(FALSE)
  }
  if (any(duplicated(StoxBioticData$Station$Station))){
    if (raiseErrors){
      stop("Duplicate Station keys")
    }
    return(FALSE)
  }
  if (any(duplicated(StoxBioticData$Haul$Haul))){
    if (raiseErrors){
      stop("Duplicate Haul keys")
    }
    return(FALSE)
  }
  # species category only have component key
  if (any(duplicated(paste(StoxBioticData$SpeciesCategory$CruiseKey, StoxBioticData$SpeciesCategory$StationKey, StoxBioticData$SpeciesCategory$HaulKey, StoxBioticData$SpeciesCategory$SpeciesCategory)))){
    if (raiseErrors){
      stop("Duplicate SpeciesCategory keys")
    }
    return(FALSE)
  }
  if (any(duplicated(StoxBioticData$Sample$Sample))){
    if (raiseErrors){
      stop("Duplicate Sample keys")
    }
    return(FALSE)
  }
  if (any(duplicated(StoxBioticData$Individual$Individual))){
    if (raiseErrors){
      stop("Duplicate Individual keys")
    }
    return(FALSE)
  }
  return(TRUE)
}

#' LOCODE table (LocodeTable)
#' 
#' @description 
#'  Table of United Nations Codes for Trade and Transport Locations  (UN/LOCODE)
#'  
#'  \describe{
#'   \item{Country}{ISO 3166 alpha-2 Country Code for the country in which the location is placed.}
#'   \item{Location}{3-character code identifying the location within country}
#'   \item{NameWoDiacritics}{Name of location without diacritic signs.}
#'   \item{Subdivision}{ISO 1-3 character alphabetic and/or numeric code for the administrative division of the country concerned (the latter part of the complete ISO 3166-2 code element (after the hyphen).)}
#'   \item{Status}{code for status of the code. E.g. Codes starting with A are various official approvals.}
#'   \item{Function}{string with codes classifying the function of the location. If this contains 1, the location has a seaport.}
#'   \item{Coordinates}{latitude and longitude in degrees and minutes.}
#'   \item{Remarks}{general remark about location or change in coding}
#'   \item{latitude}{latitude in decimal degrees}
#'   \item{longitude}{longitude in decimal degrees}
#'  }
#' 
#' 
#' @name LocodeTable
#' @concept Data types
#' 
NULL

#' Kommune polygons
#' 
#' @description 
#'  \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'StratumName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'  
#'  Polygons are defined in WGS84 coordinates (unprojected).
#' 
#'  The data contains the following columns with rows for each area:
#'   \describe{
#'    \item{id}{Offical code for the muncipality (kommunenummer)}
#'    \item{name}{Official name for the muncipality}
#'    \item{StratumName}{Equal to id}
#'   }
#'  
#' @name KommunePolygons
#' @concept Data types
#' 
NULL

#' Landings archive (LandingsArchiveData)
#' 
#' @description 
#'  Landings (aggregated sales notes). Format used for landings archive delivered to IMR by FDIR.
#'  Additional documentation (in Norwegian) can be found in the files:
#'  docs/documentation_landingsdata_archive_norwegian.csv
#'  docs/selected_code_lists_landingsdata_archive_norwegian.xlsx
#'  docs/supplementary_documentation_landingsdata_archive_norwegian.csv
#'  
#'  Note that 'AAR' and 'AAR2' denotes the year of catch, which may differ from the year of landing. The convention has been adopted to use 13 for 'LEVMND' (month of landing), when catch has been landed in January the year after catch.
#'  \describe{
#'   \item{AAR}{integer. Two last digits of year of catch}
#'   \item{AAR2}{integer. Year of catch (4 digits)}
#'   \item{FARTLAND}{character. Code for vessel flag (NOR for Norwegian, UTL for non-Norwegian)}
#'   \item{LEVAAR}{integer. Two last digits of year of landing)}
#'   \item{LEVMND}{integer. Month of landing (1=January, 12=December, 13=January following year)). Values larger than 13 are likely errors.}
#'   \item{KYST}{integer. Code for whether the catch was caught within the coastal region (12 nautical miles from the coast). Code 0 denotes oceanic catch, code 8 and 9 denotes coastal catch.)}
#'   \item{HOMR}{character. Main area of catch. As identified by the column 'StratumName' in \code{\link[RstoxFDA]{mainareaFdir2017}}), except that leading zeroes are not used for the areas 0-9.}
#'   \item{LOK}{character. Location of catch. As identified by the column 'Lokasjon' in \code{\link[RstoxFDA]{locationsFdir2017}}), together with Main area (the column HAVOMR), except neither use leading zeroes.}
#'   \item{REDS}{character. Gear code as defined by the standard NS9400. See code lists.}
#'   \item{LEVHERRD}{character. Common official code for the muncipality (kommune) the catch was landed. Not using leading zeroes}
#'   \item{LEVHERRD2}{character. Common official code for the muncipality (kommune) the catch was landed. Using leading zeroes}
#'   \item{LEVFYLKE}{character. Common official code for the muncipality (fylke) the catch was landed. Same as the two leading digits in LEVHERRD2. Using leading zeroes}
#'   \item{FISK}{character. Code for the species landed}
#'   \item{FISK_NAVN}{character. Norwegian name of the species landed.}
#'   \item{BIPROD}{character. Code for the product landed (0 codes for main-product, 1-8 codes for bi-products.)}
#'   \item{ANVEND}{character. Code for the usage of the landing (human consumption vs industiral usage). See code lists.)}
#'   \item{UTBET}{numeric. The prize paid to fisher in Norwegian currency at the time of purchase. Consult supplementary documentation for details.}
#'   \item{VEKT}{numeric. Live weight (Round weight) of landed catch in kg. Listed as zero for bi-products.}
#'  }
#' 
#' 
#' @name LandingsArchiveData
#' @concept Data types
#' 
NULL

#' Logbooks (LstLogbookData)
#' 
#' @description 
#'  Logbooks read from the lst format delivered by Directorate of Fisheries (FDIR).
#' 
#'  This format is not matched with WMS records and contains less detail than the format read by
#'  \code{\link[RstoxData]{readErsFile}}
#' 
#'  Each row represent one fishing operation, which is defined in the legislation current at the time of reporting.
#'  
#'  Additional documentation (in Norwegian) can be found in the file:
#'  doc/documentation_logbookdata_lst.xls
#'  \describe{
#'   \item{FAAR}{character. Year of catch (4 digits)}
#'   \item{REGM}{character License number of fishing vessel (registreringsmerke)}
#'   \item{FM}{character. Month of catch. number with leading zeroes. 1=January, 12=December}
#'   \item{FD}{character. Day of catch. number with leading zeroes}
#'   \item{DBNR}{character. Logbook number.}
#'   \item{TUR}{character. Trip number.}
#'   \item{FM}{character. Month of departure (start of trip). number with leading zeroes. 1=January, 12=December}
#'   \item{FD}{character. Day of departure (start of trip). number with leading zeroes.}
#'   \item{AH}{character. Port of departure (start of trip). Code identifying port.}
#'   \item{LM}{character. Month of landing of catch. number with leading zeroes. 1=January, 12=December}
#'   \item{LD}{character. Day of landing of catch. number with leading zeroes.}
#'   \item{LH}{character. Port where catch was landed. Code identifying port.}
#'   \item{RE}{character. Gear. Main gear for fishing operation definitions that allow several.}
#'   \item{MA}{character. Mesh size (mm) for meshed gear.}
#'   \item{HA}{character. Number of hauls/sets for fishing operation definitions that allow several.}
#'   \item{VAR}{numeric. Total fishing time (hours).}
#'   \item{OMRA}{character. International area code (ICES, NAFO, etc.)}
#'   \item{OKSO}{character. Economic zone. Three letter code.}
#'   \item{HO}{character. Main area of catch. As identified by the column 'StratumName' in \code{\link[RstoxFDA]{mainareaFdir2017}} or \code{\link[RstoxFDA]{mainareaFdir2018}})}
#'   \item{LO}{character. Location of catch. As identified by the column 'Lokasjon' in \code{\link[RstoxFDA]{locationsFdir2017}} or \code{\link[RstoxFDA]{locationsFdir2018}}, together with Main area (the column HAVOMR)}
#'   \item{LENG}{numeric. vessel length (m).}
#'   \item{BTON}{character. Gross tonnage of vessel.}
#'   \item{TENH}{character. Tonnage units of vessel.}
#'   \item{HEST}{character. Engine effect of vessel (Hp)}
#'   \item{FISK}{character. Code for the species landed. NS9400.}
#'   \item{VEKT}{numeric Liveweight (Roundweight) in kg.}
#'  }
#' 
#' 
#' @name LstLogbookData
#' @concept Data types
#' 
NULL

#' Age group statistics (ReportFdaByAgeData)
#' 
#' @description 
#'  Results from catch at age estimations. The results may be presented
#'  decomposed on combinations of aggregation variables, such as gear, area, stock etc.
#'  
#'  ReportFdaByAgeData is a \code{\link[data.table]{data.table}} which may have the following columns:
#'  \describe{
#'   \item{AgeGroup}{character. The age group the estimate is reported for. May be age or plus group}
#'   \item{Age}{integer. The lower age the estimate is reported for. May be an age or lower limit of plus group (inclusive)}
#'   \item{LengthGroup}{character. The length group the estimate is reported for.}
#'   \item{Length}{numeric. The upper length of the length group.}
#'   \item{<Statistic>}{A reported statistic}
#'   \item{SD}{Standard deviation for the reported statistic.}
#'   \item{Low}{The lower limit of the estimated interval for the reported statistic.}
#'   \item{High}{The higher limit of the estimated interval for the reported statistic.}
#'   \item{...}{Any aggregation variables. The names of these are listed in 'GroupingVariables'}
#'  }
#' 
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#' 
#' @name ReportFdaByAgeData
#' @concept Data types
#' 
NULL


#' Total catch statistics (ReportFdaSummaryData)
#' 
#' @description 
#'  Results from catch estimations. The results may be presented
#'  decomposed on combinations of aggregation variables, such as gear, area, stock etc.
#'  
#'  list with six members 'MeanAge', 'MeanWeight', 'MeanLength', 'TotalWeight',
#'  'TotalNumber', and 'GroupingVariables'.
#'  'MeanAge', 'MeanWeight', 'MeanLength', 'TotalWeight',
#'  'TotalNumber' are \code{\link[data.table]{data.table}}s with the columns:
#'  \describe{
#'   \item{<Statistic>}{The reported statistic, either 'MeanIndividualAge', 'MeanIndividualWeight', 'MeanIndividualLength', 'TotalWeight', or 'TotalNumber'}
#'   \item{SD}{Standard deviation for the reported statistic.}
#'   \item{Low}{The lower limit of the estimated interval for the reported statistic.}
#'   \item{High}{The higher limit of the estimated interval for the reported statistic.}
#'   \item{...}{Any aggregation variables. The names of these are listed in 'GroupingVariables'}
#'  }
#'  
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#'  
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#' @name ReportFdaSummaryData
#' @concept Data types
NULL

#' Fisheries dependent Catch At Age Report (ReportFdaCatchAtAgeData)
#' 
#' @description 
#'  A list with two members: 'NbyAge' and 'GroupingVariables'.
#'  
#'  \describe{
#'   \item{NbyAge}{A \code{\link[RstoxFDA]{ReportFdaByAgeData}} table with reported <Statistic> being 'CatchAtAge': the total catch at age in numbers.}
#'   \item{GroupingVariables}{Any specified Grouping variables.}
#'  }
#'  
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#'  
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#' @name ReportFdaCatchAtAgeData
#' @concept Data types
#' 
NULL

#' Fisheries dependent Catch At Age Covariance Report (ReportFdaCatchAtAgeCovarianceData)
#' 
#' @description 
#'  Covariances from catch at age estimations. Covariances are presented
#'  for age groups, together with any additional grouping (aggregation variables), 
#'  such as gear, area, stock etc.
#'  
#'  list with two members 'CovarianceNbyAge' and 'Variables'.
#'  'CovarianceNbyAge' is a \code{\link[data.table]{data.table}} which may have the following columns:
#'  \describe{
#'     \item{VariableId1}{Identifies the one of the variables the covariance is calculated for}
#'     \item{VariableId2}{Identifies the other one of the variables the covariance is calculated for}
#'     \item{Covariance}{The covariance of catch at age in numbers between groups identified by 'Variable1' and 'Variable2'.}
#'  }
#'  \describe{
#'   \item{VariableId}{Identifier for variable that covariances are provided for.}
#'   \item{AgeGroup}{character. The age group for the variable. May be age or plus group}
#'   \item{Age}{integer. The lower age group for the variable. May be an age or lower limit of plus group (inclusive)}
#'   \item{...}{Any aggregation variables.}
#'  }
#' 
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#' 
#' 
#' @name ReportFdaCatchAtAgeCovarianceData
#' @concept Data types
#' 
NULL

#' Fisheries dependent Catch At Age Report (ReportFdaCatchAtLengthData)
#' 
#' @description 
#'  A list with two members: 'NbyLength', and 'GroupingVariables'.
#'  
#'  \describe{
#'   \item{NbyLength}{A \code{\link[RstoxFDA]{ReportFdaByAgeData}} table with reported <Statistic> being 'CacthAtLength': the total catch at length in numbers.}
#'   \item{GroupingVariables}{Any specified Grouping variables.}
#'  }
#'  
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#'  
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#' 
#' @name ReportFdaCatchAtLengthData
#' @concept Data types
#' 
NULL

#' Fisheries dependent Catch At Age Report (ReportFdaCatchAtLengthAndAgeData)
#' 
#' @description 
#'  A list with two members: 'NbyLengthAge', and 'GroupingVariables'.
#'  
#'  \describe{
#'   \item{NbyLengthAge}{A \code{\link[RstoxFDA]{ReportFdaByAgeData}} table with reported <Statistic> being 'CatchAtAgeLength': the total catch at length and age in numbers.}
#'   \item{GroupingVariables}{Any specified Grouping variables.}
#'  }
#'  
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#'  
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#' @name ReportFdaCatchAtLengthAndAgeData
#' @concept Data types
#' 
NULL

#' Fisheries dependent Length At Age Report (ReportFdaLengthAtAgeData)
#' 
#' @description 
#'  A list with two members: 'MeanLengthByAge', and 'GroupingVariables'.
#'  
#'  \describe{
#'   \item{MeanLengthByAge}{A \code{\link[RstoxFDA]{ReportFdaByAgeData}} table with reported <Statistic> being 'MeanIndividualLength': the mean length.}
#'   \item{GroupingVariables}{Any specified Grouping variables.}
#'  }
#'  
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#'  
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#'  
#'  Note that the summary statistics are reported for summaries of mean lengths, 
#'  so that e.g. SD report the standard deviation of the means,
#'  and does not characterize the length distribution of fish.
#' 
#' @name ReportFdaLengthAtAgeData
#' @concept Data types
#' 
NULL

#' Reca Weight At Age Report (ReportFdaWeightAtAgeData)
#' 
#' @description 
#'  A list with two members: 'MeanWeightByAge', and 'GroupingVariables'.
#'  
#'  \describe{
#'   \item{MeanWeightByAge}{A \code{\link[RstoxFDA]{ReportFdaByAgeData}} table with reported <Statistic> being 'MeanIndividualWeight': the mean weight}
#'   \item{GroupingVariables}{Any specified Grouping variables.}
#'  }
#'  
#'  Units are configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#'  
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#'  
#'  Note that the summary statistics are reported for summaries
#'  of mean weights, so that e.g. SD report the standard deviation of the means,
#'  and does not characterize the weight distribution of fish.
#' 
#' @name ReportFdaWeightAtAgeData
#' @concept Data types
#' 
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ReportFdaByAgeData}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaByAgeData}}
#' @param ReportFdaByAgeData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaByAgeData}}
#' @concept Data types
#' @export
is.ReportFdaByAgeData <- function(ReportFdaByAgeData){
  if (!is.list(ReportFdaByAgeData)){
    return(FALSE)
  }
  if (!all(c("GroupingVariables") %in% names(ReportFdaByAgeData))){
    return(FALSE)
  }
  if (length(ReportFdaByAgeData)<2){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaByAgeData[[1]])){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaByAgeData$GroupingVariables)){
    return(FALSE)
  }
  if (!all(c("Age", "Low", "High", "SD") %in% names(ReportFdaByAgeData[[1]]))){
    return(FALSE)
  }
  if (!all(c("GroupingVariables") %in% names(ReportFdaByAgeData$GroupingVariables))){
    return(FALSE)
  }
  return(TRUE)
}

#' Sum of Products report (ReportFdaSopData)
#' 
#' @description 
#'  Sum of Products report (SOP-report), comparing the total landed weight of fish
#'  with the product of mean weight at age estimates and total number 
#'  at age estimates 
#'  
#'  list with two members 'SopReport' and 'GroupingVariables'.
#'  'SopReport' is a \code{\link[data.table]{data.table}} with the columns:
#'  \describe{
#'   \item{TotalWeightEstimated}{Total round weight estimated in kg}
#'   \item{LandedWeight}{Landed round weight reported in kg}
#'   \item{Difference}{The difference between estimated and reported landed weight in kg}
#'   \item{RelativeDifference}{The difference between estimated and reported landed weight relative to reported weight}
#'   \item{...}{Any aggregation variables. The names of these are listed in 'GroupingVariables'}
#'  }
#'  'GroupingVariables' is a \code{\link[data.table]{data.table}} with a column containing the names of any aggregation variables.
#' 
#'  The unit for RelativeDifference is configurable, and can be inspected by ~\code{\link[RstoxData]{getUnit}}
#' 
#' @name ReportFdaSopData
#' @concept Data types
#' 
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ReportFdaSOP}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSOP}}
#' @param ReportFdaSOP argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSOP}}
#' @concept Data types
#' @export
is.ReportFdaSOP <- function(ReportFdaSOP){
  
  if (!is.list(ReportFdaSOP)){
    return(FALSE)
  }
  if (!all(c("GroupingVariables", "SopReport") %in% names(ReportFdaSOP))){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaSOP$SopReport)){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaSOP$GroupingVariables)){
    return(FALSE)
  }
  if (!all(c("TotalWeightEstimated", "LandedWeight", "Difference", "RelativeDifference") %in% names(ReportFdaSOP$SopReport))){
    return(FALSE)
  }
  if (!all(c("GroupingVariables") %in% names(ReportFdaSOP$GroupingVariables))){
    return(FALSE)
  }
  return(TRUE)
  
}


#' Summary statistics for simulated parameters (ParameterizationSummaryData)
#' 
#' @description 
#'  Summary statistics for (potentially multi-chained) simulated parameters, 
#'  such as MCMC simulations with Reca.
#'  'chains' in this respect refers to statistically independent simulations.
#'  
#'  list with two members 'ParameterSummary' and 'RunParameters',
#'  which both all \code{\link[data.table]{data.table}}s. 'ParameterSummary'
#'  contains parameter statistics for simulations and have the following columns:
#'  \describe{
#'   \item{Parameter}{Identifies the parameter that is summarized.}
#'   \item{Mean}{Mean of the parameter}
#'   \item{Variance}{Variance of the parameter}
#'   \item{chainId}{Identifies the parameterization chain.}
#'  }
#'  
#'  RunParameters summarizes global control parameters for each chain and has columns:
#'  \describe{
#'   \item{chainId}{Identifies the parameterization chain.}
#'   \item{Iterations}{The number of iterations for parameter simulation}
#'  }
#'  
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @seealso \code{\link[RstoxFDA]{ReportRecaParameterStatistics}} 
#'  for creating ParameterizationSummaryData from Reca-simulations.
#' 
#' @name ParameterizationSummaryData
#' @concept Data types
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ParameterizationSummaryData}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ParameterizationSummaryData}}
#' @param ParameterizationSummaryData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ParameterizationSummaryData}}
#' @concept Data types
#' @export
is.ParameterizationSummaryData <- function(ParameterizationSummaryData){
  
  if (!is.list(ParameterizationSummaryData)){
    return(FALSE)
  }
  
  if (!all(c("ParameterSummary", "RunParameters") %in% names(ParameterizationSummaryData))){
    return(FALSE)
  }
  
  if (!data.table::is.data.table(ParameterizationSummaryData$ParameterSummary)){
    return(FALSE)
  }
  
  if (!data.table::is.data.table(ParameterizationSummaryData$RunParameters)){
    return(FALSE)
  }
  
  if (!all(c("chainId","Iterations") %in% names(ParameterizationSummaryData$RunParameters))){
    return(FALSE)
  }
  
  if (!all(c("Parameter", "Mean", "Variance", "chainId") %in% names(ParameterizationSummaryData$ParameterSummary))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Convergence Report for simulated parameters (ParameterConvergenceData)
#' 
#' @description 
#'  Convergence Report for multi-chained simulated parameters, 
#'  such as MCMC simulations with Reca.
#'  'chains' in this respect refers to statistically independent simulations.
#'  
#'  list with one members 'ConvergenceReport',
#'  which is a \code{\link[data.table]{data.table}} containing the following columns:
#'  \describe{
#'   \item{Parameter}{Identifies the parameter that is summarized.}
#'   \item{InterVariance}{The Mean-Squared-Deviation the means of the parameter in each chain, to the mean across all chains}
#'   \item{IntraVariance}{The mean of the within-chain variances of the parameter}
#'   \item{GelmanRubinR}{Gelman-Rubins R}
#'  }
#'  
#' @details 
#'  Gelman-Rubins R is described by Gelman and Rubin (Statistical Science, 1992):
#'  DOI: https://doi.org/10.1214/ss/1177011136
#'  
#' @seealso \code{\link[RstoxFDA]{RunRecaModels}} for running Reca-analysis
#' @seealso \code{\link[RstoxFDA]{ReportParameterConvergence}} 
#'  for creating ParameterConvergenceData.
#' 
#' @name ParameterConvergenceData
#' @concept Data types
#' 
NULL

#' Checks if argument is \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @param ParameterConvergenceData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ParameterConvergenceData}}
#' @concept Data types
#' @export
is.ParameterConvergenceData <- function(ParameterConvergenceData){
  
  if (!is.list(ParameterConvergenceData)){
    return(FALSE)
  }
  if (!all(c("ConvergenceReport") %in% names(ParameterConvergenceData))){
    return(FALSE)
  }
  cnames <- c("Parameter", "InterVariance", "IntraVariance", "GelmanRubinR")
  if (!all(cnames %in% names(ParameterConvergenceData$ConvergenceReport))){
    return(FALSE)
  }
  if (!data.table::is.data.table(ParameterConvergenceData$ConvergenceReport)){
    return(FALSE)
  }

  return(TRUE)
}

#' Checks if argument is \code{\link[RstoxData]{Translation}}
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxData]{Translation}}
#' @param Translation argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxData]{Translation}}
#' @concept Data types
#' @export
is.Translation <- function(Translation){
  if (!data.table::is.data.table(Translation)){
    return(FALSE)
  }
  if (!all(c("NewValue") %in% names(Translation))){
    return(FALSE)
  }
  if (ncol(Translation)<2){
    return(FALSE)
  }
  return(TRUE)
}

#' Length Conversion Table (LengthConversionTable)
#' 
#' @description
#'  Length conversion parameters realting different length measurements.
#' @details 
#'  Length conversion factors relating different length measurements, such as 'standard length' and 'fork length'
#'  based on a linear regression fit between length measurements:
#'  L1 = alpha + beta \* L2,
#'  where L1 and L2 are different length measurements
#'  and 'alpha' and 'beta' are species-specific coefficients.
#'  
#'  \code{\link[data.table]{data.table}} with columns:
#'  \describe{
#'  \item{'Description'}{character: Free-text description of the product}
#'  \item{'Species'}{character: Identifier for the species that the conversion applies to}
#'  \item{'MeasurmentType'}{character: Identifier for the type of length measurement for the independent variable (L2 above)}
#'  \item{'Alpha'}{numeric: scalar value representing the intercept (in cm) of a linear regression fit between length measurements.}
#'  \item{'Beta'}{numeric: scalar value representing the slope of a linear regression fit between length measurements.}
#'  }
#'  
#' @name LengthConversionTable
#' @concept Data types
#' 
NULL

#' Check if argument is LengthConversionTable
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{LengthConversionTable}}
#' @param LengthConversionTable argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{LengthConversionTable}}
#' @concept Data types
#' @export
is.LengthConversionTable <- function(LengthConversionTable){
  if (!data.table::is.data.table(LengthConversionTable)){
    return(FALSE)
  }
  if (!all(c("Description", "Species", "MeasurementType", "Alpha", "Beta") %in% names(LengthConversionTable))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Weight Conversion Table (WeightConversionTable)
#' 
#' @description 
#'  \code{\link[data.table]{data.table}} with factors for approximating the weight of a 
#'  desired product type (e.g. round fish)
#'  from weights of other fish products. Contains the columns:
#'  \describe{
#'  \item{'Description'}{Free-text description of the product type}
#'  \item{'Species'}{Identifier for the species that the conversion applies to}
#'  \item{'ProductType'}{Identifier for the type of product that the conversion applies to}
#'  \item{'WeightFactor'}{scalar value that weights for the given 'ProductType' can be multiplied with to approximate desired product type (e.g. round fish).}
#'  }
#'  NA is allowed for 'WeightFactor', which will result in NA for weights after conversion
#'  
#' @name WeightConversionTable
#' @concept Data types
#' 
NULL

#' Check if argument is WeightConversionTable
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{WeightConversionTable}}
#' @param WeightConversionTable argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{WeightConversionTable}}
#' @concept Data types
#' @export
is.WeightConversionTable <- function(WeightConversionTable){
  if (!data.table::is.data.table(WeightConversionTable)){
    return(FALSE)
  }
  if (!all(c("Description", "Species", "ProductType", "WeightFactor") %in% names(WeightConversionTable))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Sampling Overview cell-plot data (PlotSamplingOverviewCellData)
#' 
#' @description 
#'  a ggplot object that renders the a colored 'cell plot' overview of samples and landings.
#'  
#' @name PlotSamplingOverviewCellData
#' @concept Data types
#' 
NULL

#' Sampling variables plot data (PlotSamplingVariablesData)
#' 
#' @description 
#'  a ggplot object that renders a stacked barplot of sampling variables for each part of the fishery, with total landings on a secondary axis.
#'  
#' @name PlotSamplingVariablesData
#' @concept Data types
#' 
NULL

#' Sampling Report data (ReportFdaSamplingData)
#' 
#' @description 
#'  list with tow members:
#'  \describe{
#'   \item{GroupingVariables}{a \code{\link[data.table]{data.table}} with the variables used for aggregation in 'FisheriesSampling' stored in the column 'GroupingVariables'}
#'   \item{SamplingVariables}{a \code{\link[data.table]{data.table}} with the variables used for partitioning samples in 'FisheriesSampling' stored in the column 'SamplingVariables'}
#'   \item{FisheriesSampling}{a \code{\link[data.table]{data.table}} described below.}
#'  }
#'  
#'  FisheriesSampling is a report of sampling against total landings for partitions of a fishery.
#'  The report is a \code{\link[data.table]{data.table}} with columns:
#'  \describe{
#'   \item{...}{A column for each of the provided Aggregation variables}
#'   \item{...}{A column for each of the provided Sampling variables}
#'   \item{LandedRoundWeight}{Total landings in kg}
#'   \item{Catches}{Number of catches sampled}
#'   \item{Vessels}{Number of vessels sampled}
#'   \item{WeightMeasurements}{Number of fished measured for weight}
#'   \item{LengthMeasurements}{Number of fished measured for length}
#'   \item{AgeReadings}{Number of fished with age determined}
#'   \item{WeightOfSampledCatches}{Total weight of the sampled catches}
#'  }
#' 
#' @name ReportFdaSamplingData
#' @concept Data types
#' 
NULL

#' Check if argument is ReportFdaSamplingData
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @param ReportFdaSamplingData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{ReportFdaSamplingData}}
#' @concept Data types
#' @export
is.ReportFdaSamplingData <- function(ReportFdaSamplingData){
  if (!is.list(ReportFdaSamplingData)){
    return(FALSE)
  }
  if (!all(c("GroupingVariables", "SamplingVariables", "FisheriesSampling") %in% names(ReportFdaSamplingData))){
    return(FALSE)
  }
  if (!data.table::is.data.table(ReportFdaSamplingData$FisheriesSampling)){
    return(FALSE)
  }
  
  if (!all(c("LandedRoundWeight", "Catches", "Vessels", "WeightMeasurements", "LengthMeasurements", "AgeReadings", "WeightOfSampledCatches") %in% names(ReportFdaSamplingData$FisheriesSampling))){
    return(FALSE)
  }
  
  return(TRUE)
  
}

#' Landings Report data (ReportFdaLandingData)
#' 
#' @description 
#'  list with tow members:
#'  \describe{
#'   \item{GroupingVariables}{a \code{\link[data.table]{data.table}} with the variables used for aggregation in 'FisheriesLandings' stored in the column 'GroupingVariables'}
#'   \item{FisheriesLandings}{a \code{\link[data.table]{data.table}} described below.}
#'  }
#'  
#'  FisheriesLandings is a report of landings for partitions of a fishery.
#'  The report is a \code{\link[data.table]{data.table}} with columns:
#'  \describe{
#'   \item{...}{A column for each of the provided Aggregation variables}
#'   \item{LandedRoundWeight}{Total landings in kg}
#'  }
#' 
#' @name ReportFdaLandingData
#' @concept Data types
#' 
NULL

#' Reca Data (RecaData)
#'
#' Data and some data parameters prepared for running
#' \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' via \code{\link[RstoxFDA]{RunRecaEstimate}}.
#' 
#' A list with five members:
#'
#' @details
#' \describe{
#'  \item{AgeLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. List of \code{\link[data.table]{data.table}}}
#'  \item{WeightLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. List of \code{\link[data.table]{data.table}}}
#'  \item{Landings}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. List of \code{\link[data.table]{data.table}}}
#'  \item{GlobalParameters}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. see details. List of \code{\link[data.table]{data.table}}}
#'  \item{CovariateMaps}{Mapping of values for each covariate in landings and samples (including non-configurable catchId) to integer value used in R-ECA. List of \code{\link[data.table]{data.table}}}
#' }
#'
#' @name RecaData
#' @concept Data types
#'
NULL

#' Check if argument is RecaData
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaData}}
#' @param RecaData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaData}}
#' @concept Data types
#' @export
is.RecaData <- function(RecaData){
  if (!is.list(RecaData)){
    return(FALSE)
  }
  if (!all(c("AgeLength", "WeightLength", "Landings", "GlobalParameters") %in% names(RecaData))){
    return(FALSE)
  }
  if (!is.list(RecaData$AgeLength)){
    return(FALSE)
  }
  if (!is.list(RecaData$WeightLength)){
    return(FALSE)
  }
  if (!is.list(RecaData$Landings)){
    return(FALSE)
  }
  if (!is.list(RecaData$GlobalParameters)){
    return(FALSE)
  }

  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaData$AgeLength))){
    return(FALSE)
  }
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaData$WeightLength))){
    return(FALSE)
  }
  if (!all(c("AgeLengthCov", "WeightLengthCov", "LiveWeightKG") %in% names(RecaData$Landings))){
    return(FALSE)
  }
  
  return(TRUE)
}


#' Reca Parameter Data (RecaParameterData)
#'
#' @description 
#' Data and some data parameters prepared for running
#' various report functions that invoke \code{\link[Reca]{eca.predict}}.
#'
#' @section model fit:
#'  For inspection or analysis of model fit, the lists 'FitProportionAtAge', 
#'  'FitLengthGivenAge' and 'FitWeightGivenLength' is of interest. For stock-splitting analysis,
#'  the lists FitLengthGivenAgeCC and FitWeightGivenLengthCC will be added as well, corresponding to one of the stocks.
#'  These lists correspond to the three Reca-models and contain:
#'  \describe{
#'  \item{LogLikeliehood}{A \code{\link[data.table]{data.table}} 
#'    tabulating the logarithm of the likeliehood of the parameter set for each iteration}
#'  \item{...}{A \code{\link[data.table]{data.table}} for each of the model effects (e.g. covariates).}
#'  }
#'  
#'  In addition to configurable covariates, the models always contain a constant effect (named 'constant'),
#'  a catch or haul effect (named 'catchSample') and effects for fish measurments (named 'fish'). 
#'  Where relevant the following parameters may be tabulated for each effect:
#'  \describe{
#'  \item{Age}{Identifying the age the effect applies to}
#'  \item{Level}{Identifying the value or level of the covariate the effect applies to}
#'  \item{Iteration}{Identifying the iteration the fit is provided for}
#'  \item{AgeIndex}{Age identifier used internally in Reca}
#'  \item{LevelIndex}{Level identifier used internally in Reca}
#'  \item{Slope}{The value of the regression slope}
#'  \item{tau_Slope}{The value of tau parameter for the regression slope}
#'  \item{ar_Slope}{The value of regression slope of a the autoregressive coefficient associated with the effect}
#'  \item{Intercept}{The value of the regression intercept}
#'  \item{tau_Intercept}{The value of tau parameter for the regression intercept}
#'  \item{ar_Intercept}{The value of the regression intercept of a autoregressive coefficient associated with the effect}
#'  }
#'  Consult Hirst et.al. 2005 for description of the parameters
#'  
#'  @section other data:
#'  The lists 'AgeLength', 'WeightLength', 'Landings', 'GlobalParameters' and 'CovariateMaps'
#'  may be passed to \code{\link[Reca]{eca.predict}} in functions consuming output from this function. All in all
#'  the following lists can be accessed on RecaParameterData objects:
#'  \describe{
#'  \item{FitProportionAtAge}{list of data tables with parameters for for the Proportion-at-age model}
#'  \item{FitLengthGivenAge}{list of data tables with parameters for for the Length-given-age model}
#'  \item{FitWeightGivenLength}{list of data tables with parameters for for the Weight-given-length model}
#'  \item{AgeLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{WeightLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{Landings}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{GlobalParameters}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. see details}
#'  \item{CovariateMaps}{Mapping of values for each covariate in landings and samples (including non-configurable catchId) to integer value used in R-ECA.}
#' }
#'
#' @name RecaParameterData
#' @concept Data types
#'
NULL

#' Check if argument is RecaParameterData
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaParameterData}}
#' @param RecaParameterData argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaParameterData}}
#' @concept Data types
#' @export
is.RecaParameterData <- function(RecaParameterData){
  
  if (!is.list(RecaParameterData)){
    return(FALSE)
  }
  if (!all(c("FitProportionAtAge", "FitLengthGivenAge", "FitWeightGivenLength", "AgeLength", "WeightLength", "Landings", "GlobalParameters") %in% names(RecaParameterData))){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$AgeLength)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$WeightLength)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$Landings)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$GlobalParameters)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$FitProportionAtAge)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$FitLengthGivenAge)){
    return(FALSE)
  }
  if (!is.list(RecaParameterData$FitWeightGivenLength)){
    return(FALSE)
  }
  
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaParameterData$AgeLength))){
    return(FALSE)
  }
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaParameterData$WeightLength))){
    return(FALSE)
  }
  if (!all(c("AgeLengthCov", "WeightLengthCov", "LiveWeightKG") %in% names(RecaParameterData$Landings))){
    return(FALSE)
  }
  if (!all(c("LogLikelihood") %in% names(RecaParameterData$FitProportionAtAge))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaParameterData$FitProportionAtAge$LogLikelihood)){
    return(FALSE)
  }
  if (!all(c("LogLikelihood") %in% names(RecaParameterData$FitLengthGivenAge))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaParameterData$FitLengthGivenAge$LogLikelihood)){
    return(FALSE)
  }
  if (!all(c("LogLikelihood") %in% names(RecaParameterData$FitWeightGivenLength))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaParameterData$FitWeightGivenLength$LogLikelihood)){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Reca Results (RecaCatchAtAge)
#' 
#' @description
#'  Posterior distribution of total catch at age and weight and length parameters.
#'
#' @details
#' a list of data tables:
#' \describe{
#'  \item{CatchAtAge}{\code{\link[data.table]{data.table}} tabulating the estimated catch-at-age by length group for each Reca iteration (MCMC sample)}
#'  \item{MeanLength}{\code{\link[data.table]{data.table}} tabulating the mean length in cm by age for each Reca iteration (MCMC sample)}
#'  \item{MeanWeight}{\code{\link[data.table]{data.table}} tabulating the mean weight in g by age for each Reca iteration (MCMC sample)}
#'  \item{GroupingVariables}{\code{\link[data.table]{data.table}} with any variables that catch-at-age estimates are partitioned on 
#'            in the column 'GroupingVariables'. These may correspond to variables in the landings, or maye be the variable 'Stock' if
#'            stock-splitting analysis have been perfomred.}
#' }
#' In addition to columns for the variables in 'GroupingVariables', the data tables 'CatchAtAge', 'MeanLength', and 'MeanWeight' have the following variables:
#' \describe{
#'  \item{Age}{Age in number of years.}
#'  \item{Iteration}{The Reca iteration (MCMC sample) that estimates are calculated for}
#' }
#' 
#' 'CatchAtAge' also have the variables
#' \describe{
#'  \item{Length}{Upper limit of length group in cm}
#'  \item{CatchAtAge}{The total catch at age in numbers}
#' }
#' 
#' 'MeanLength' has the variable:
#' \describe{
#'  \item{MeanIndividualLength}{Mean Length at age in cm}
#' }
#' 
#' 'MeanWeight' has the variable:
#' \describe{
#'  \item{MeanIndividualWeight}{Mean weight at age in g}
#' }
#'
#' @name RecaCatchAtAge
#' @concept Data types
#'
NULL

#' Check if argument is RecaCatchAtAge
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaCatchAtAge}}
#' @param RecaCatchAtAge argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaCatchAtAge}}
#' @concept Data types
#' @export
is.RecaCatchAtAge <- function(RecaCatchAtAge){
  if (!is.list(RecaCatchAtAge)){
    return(FALSE)
  }
  if (!all(c("CatchAtAge", "MeanLength", "MeanWeight") %in% names(RecaCatchAtAge))){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaCatchAtAge$CatchAtAge)){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaCatchAtAge$MeanLength)){
    return(FALSE)
  }
  if (!data.table::is.data.table(RecaCatchAtAge$MeanWeight)){
    return(FALSE)
  }
  if (!all(c("Length", "Age", "Iteration", "CatchAtAge") %in% names(RecaCatchAtAge$CatchAtAge))){
    return(FALSE)
  }
  if (!all(c("MeanIndividualLength", "Age", "Iteration") %in% names(RecaCatchAtAge$MeanLength))){
    return(FALSE)
  }
  if (!all(c("MeanIndividualWeight", "Age", "Iteration") %in% names(RecaCatchAtAge$MeanWeight))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Reca Results (RecaResult)
#'
#' Results from running
#' \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' via \code{\link[RstoxFDA]{RunRecaEstimate}}.
#'
#' @details
#'
#' \describe{
#'  \item{input}{All input data and parameters provided to \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{fit}{as returned by \code{\link[Reca]{eca.estimate}}}
#'  \item{prediction}{as returned by \code{\link[Reca]{eca.predict}}}
#'  \item{covariateMaps}{list() mapping from Reca covariate encoding to values fed to \code{\link[RstoxFDA]{PrepareRecaEstimate}}. As in \code{\link[RstoxFDA]{RecaData}}}
#' }
#'
#' @name RecaResult
#' @concept Data types
#'
NULL

#' @noRd
is.RecaPrediction <- function(prediction){
  if (!is.list(prediction)){
    return(FALSE)
  }
  if (!all(c("TotalCount", "MeanLength", "MeanWeight", "AgeCategories", "LengthIntervalsLog") %in% names(prediction))){
    return(FALSE)
  }
  if (!is.array(prediction$TotalCount)){
    return(FALSE)
  }
  if (!is.array(prediction$MeanLength)){
    return(FALSE)
  }
  if (!is.array(prediction$MeanWeight)){
    return(FALSE)
  }
  return(TRUE)
}

#' Check if argument is RecaResult
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaResult}}
#' @param RecaResult argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{RecaResult}}
#' @concept Data types
#' @export
is.RecaResult <- function(RecaResult){
  if (!is.list(RecaResult)){
    return(FALSE)
  }
  if (!all(c("input", "fit", "prediction", "covariateMaps") %in% names(RecaResult))){
    return(FALSE)
  }
  if (!is.RecaData(RecaResult$input)){
    return(FALSE)
  }
  if (!is.RecaPrediction(RecaResult$prediction)){
    return(FALSE)
  }
  
  return(TRUE)
}


#' Temporal Categories (TemporalDefinition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a categorical variable for grouping data based on date.
#'
#' @details
#'  \describe{
#'   \item{TemporalCategory}{character() Value of the temporal category}
#'   \item{StartDay}{integer() Day of month for first day in the temporal category (1-based)}
#'   \item{StartMonth}{integer() Month for first day in the temporal category (1-based)}
#'   \item{StartYear}{optional integer() Year for which the category is defined, omit this column for seasonal definitions.}
#'  }
#'
#'  Start and end of year is not implied as category delimitations when not included.
#'  If 1st of January is not defined as the start of a category,
#'  it is taken to be included in the last category of the preceding year.
#'
#' @name TemporalDefinition
#' @concept Data types
#'
NULL

#' Check if argument is TemporalDefinition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{TemporalDefinition}}
#' @param TemporalDefinition argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{TemporalDefinition}}
#' @concept Data types
#' @export
is.TemporalDefinition <- function(TemporalDefinition){
  if (!data.table::is.data.table(TemporalDefinition)){
    return(FALSE)
  }
  #StartYear is optional.
  if (!all(c("Period", "StartDay", "StartMonth") %in% names(TemporalDefinition))){
    return(FALSE)
  }
  if (!all(names(TemporalDefinition) %in% c("Period", "StartDay", "StartMonth", "StartYear"))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Area Code Positions (AreaPosition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a position for area codes.
#'
#' @details
#'  \describe{
#'   \item{Area}{Area code. (key)}
#'   \item{Location}{optional subdivision of 'Area'}
#'   \item{Latitude}{WGS84 Latitude, decimal degrees}
#'   \item{Longitude}{WGS84 Longitude, decimal degrees}
#'  }
#'  If location is provided, the case for missing location is also encoded.
#'
#' @name AreaPosition
#' @concept Data types
#'
NULL

#' Check if argument is AreaPosition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{AreaPosition}}
#' @param AreaPosition argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{AreaPosition}}
#' @concept Data types
#' @export
is.AreaPosition <- function(AreaPosition){
  if (!data.table::is.data.table(AreaPosition)){
    return(FALSE)
  }
  if (!all(c("Area", "Location", "Latitude", "Longitude") %in% names(AreaPosition))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Area Neighbour Definition (CarNeighbours)
#'
#' Table (\code{\link[data.table]{data.table}})
#' defining neighbours for a CAR-variable (Conditional autoregressive variable).
#'
#' @details
#'  \describe{
#'   \item{CarValue}{Values for a variable used as CAR-variable}
#'   \item{Neighbours}{Comma-separated list of neighbours}
#'  }
#'
#'  The table is symmetric, so that if b is a neighbour of a. a is also a neighbour of b.
#'
#' @name CarNeighbours
#' @concept Data types
#'
NULL


#' Landing data (LandingData)
#'
#' @description 
#'  See \code{\link[RstoxData]{LandingData}}
#'
#' @name LandingData
#' @concept Data types
#'
NULL

#' Check if argument is CarNeighbours
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{CarNeighbours}}
#' @param CarNeighbours argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{CarNeighbours}}
#' @concept Data types
#' @export
is.CarNeighbours <- function(CarNeighbours){
  if (!data.table::is.data.table(CarNeighbours)){
    return(FALSE)
  }
  if (!all(c("CarValue", "Neighbours") %in% names(CarNeighbours))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Age Error Matrix (AgeErrorMatrix)
#'
#' Table (\code{\link[data.table]{data.table}})
#' defining probabilities of misreading age.
#'
#' @details
#'  \describe{
#'   \item{columns 1..n}{numeric() [0,1]. Probability of reading read age, given that true age is as column name.}
#'   \item{ReadAge}{The read age.}
#'  }
#'
#'  Columns sum to 1.
#'
#' @name AgeErrorMatrix
#' @concept Data types
#'
NULL

#' Check if argument is AgeErrorMatrix
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{AgeErrorMatrix}}
#' @param AgeErrorMatrix argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{AgeErrorMatrix}}
#' @concept Data types
#' @export
is.AgeErrorMatrix <- function(AgeErrorMatrix){
  if (!data.table::is.data.table(AgeErrorMatrix)){
    return(FALSE)
  }
  if (!("ReadAge" %in% names(AgeErrorMatrix))){
    return(FALSE)
  }
  
  return(TRUE)
}

#' Stock splitting parameters (StockSplittingParameters)
#'
#' @description
#'  Table (\code{\link[data.table]{data.table}})
#'  
#'  Defining parameters for the stock-splitting analysis in Reca, including
#'  parameters for the probability of misclassifying when determining stock membership of a specimen.
#'
#'  The stock splitting analysis allows catch at age to be estimated for two domains that partition all individuals,
#'  and that are observed for all age-determined specimens. It was developed for disciminating Coastal Cod and North East Arctic Cod,
#'  based on otholith growth patterns, and naming conventions are derived from that. It could be adapted to
#'  other stocks and in principle to other bipartite domain definitions (such as Sex).
#'  
#'  Two otolith patterns are defined for each of the two stocks 'CC' and 'S'. Otolith type 1 and 2 identifies
#'  that a specimen belongs to the stock 'CC', and are interpreted by otoloith readers as 'certain' and 'uncertain' CC, respectively.
#'  Otolith type 4 and 5 identifies that a specimen belongs to the stock 'S', and are interpreted as 'uncertain' and 'certain' S, respectively.
#'  
#'  \describe{
#'   \item{StockNameCC}{Name of the stock identified as CC}
#'   \item{StockNameS}{Name of the stock identified as S}
#'   \item{ProbabilityType1As1}{Probability of classifying a type 1 specimen as type 1 (certain CC).}
#'   \item{ProbabilityType5As1}{Probability of classifying a type 5 (certain S) specimen as type 1 (certain CC).}
#'   \item{ProbabilityType2As2}{Probability of classifying a type 2 (uncertain CC) specimen as type 2 (uncertain CC).}
#'   \item{ProbabilityType4As2}{Probability of classifying a type 4 (uncertain S) specimen as type 2 (uncertain CC).}
#'   \item{ProbabilityType2As4}{Probability of classifying a type 2 (uncertain CC) specimen as type 4 (uncertain S).}
#'   \item{ProbabilityType4As4}{Probability of classifying a type 4 (uncertain S) specimen as type 4 (uncertain S).}
#'   \item{ProbabilityType1As5}{Probability of classifying a type 1 (certain CC) specimen as type 5 (certain S).}
#'   \item{ProbabilityType5As5}{Probability of classifying a type 5 (certain S) specimen as type 5 (certain S).}
#'  }
#'
#'  The probabilities for different ways to classify a type must sum to 1.
#'  E.g.: ProbabilityType1As1 + ProbabilityType1As5 = 1.
#'
#'  The data table contains only one row.
#'
#' @name StockSplittingParameters
#' @concept Data types
#'
NULL

#' Check if argument is StockSplittingParameters
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @param StockSplittingParameters argument to be checked for data conformity
#' @return logical, TRUE if argument conforms to specification for \code{\link[RstoxFDA]{StockSplittingParameters}}
#' @concept Data types
#' @export
is.StockSplittingParameters <- function(StockSplittingParameters){
  if (!data.table::is.data.table(StockSplittingParameters)){
    return(FALSE)
  }
  if (!all(c("StockNameCC", "StockNameS", "ProbabilityType1As1",
             "ProbabilityType1As5", "ProbabilityType2As2",
             "ProbabilityType2As4",	"ProbabilityType4As2",
             "ProbabilityType4As4",	"ProbabilityType5As1",
             "ProbabilityType5As5") %in% names(StockSplittingParameters))){
    return(FALSE)
  }
  if (nrow(StockSplittingParameters) != 1){
    return(FALSE)
  }
  
  prob <- function(arg){
    if (arg<0 | arg > 1){
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (!prob(StockSplittingParameters$ProbabilityType1As1)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType1As5)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType2As2)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType2As4)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType4As2)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType4As4)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType5As1)){
    return(FALSE)
  }
  if (!prob(StockSplittingParameters$ProbabilityType5As5)){
    return(FALSE)
  }
  
  return(TRUE)
}

plotDefaultsCellPlotColors <- list(
  MinVessels=2,
  MinCatches=2, 
  MinMeasurements=100,
  ColorNoSamples = "#ffffcc", 
  ColorFewCacthes = "#c2e699", 
  ColorFewVessels = "#78c679", 
  ColorGoodSampling = "#238443"
)

#' Function specification for inclusion in StoX UI
#' @export
stoxFunctionAttributes <- list(
  
  ReadLandingFDA = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "LandingData", 
    functionParameterFormat = list(FileNames = "filePaths"), 
    functionArgumentHierarchy = list()
  ), 
  DefineStockSplittingParameters = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StockSplittingParameters",
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      ),
      StockNameCC=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      StockNameS=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType1As1=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType5As1=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType2As2=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType4As2=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType2As4=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType4As4=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType1As5=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      ),
      ProbabilityType5As5=list(
        DefinitionMethod = "FunctionParameters", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineCarNeighbours = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "CarNeighbours",
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      StratumPolygon = list(
        DefinitionMethod = "StratumPolygon", 
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineAgeErrorMatrix = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "AgeErrorMatrix",
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineAreaPosition = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "AreaPosition", 
    functionParameterFormat = list(
      FileName = "filePath"
    ), 
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      StratumPolygon = list(
        DefinitionMethod = "StratumPolygon", 
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      )
    )
  ),
  DefinePeriod = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "TemporalDefinition", 
    functionParameterFormat = list(
      CustomPeriods = "periodvector"
    ), 
    functionArgumentHierarchy = list(
      TemporalCategory = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      CustomPeriods = list(
        TemporalCategory = "Custom", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineWeightConversionFactor = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "WeightConversionTable", 
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      )
    )
  ),
  
  DefineLengthConversionParameters = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "LengthConversionTable", 
    functionParameterFormat = list(
      FileName = "filePath"
    ),
    functionArgumentHierarchy = list(
      DefinitionMethod = list(
        UseProcessData = FALSE
      ), 
      # These two are joined with AND, and must both be fulfilled:
      FileName = list(
        DefinitionMethod = "ResourceFile", 
        UseProcessData = FALSE
      )
    )
  ),
  
  LoadFdaStratumPolygon = list(
    functionType = "processData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StratumPolygon",
    functionArgumentHierarchy = list(
      StrataSystem = list(
        UseProcessData = FALSE
      )
    )
  ),
  
  ConvertWeightBiotic =list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData" 
  ),
  
  ConvertLengthBiotic =list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData" 
  ),
  
  SetTimeBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  SetShortGearBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  SetStartDateBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  
  SetAreaPositionsBiotic =list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  
  AddAreaPositionStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddGearGroupStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddGearGroupStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  
  AddStratumStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddStratumStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  
  AddPeriodStoxLanding = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxLandingData"
  ),
  
  AddPeriodStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  
  ListBioticDifference = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "BioticData"
  ),
  FilterAgeLengthOutliersStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  FilterWeightLengthOutliersStoxBiotic = list(
    functionType = "modelData", 
    functionCategory = "baseline", 
    functionOutputDataType = "StoxBioticData"
  ),
  
  PrepareRecaEstimate = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaData",
    functionParameterFormat = list(
      RandomEffects = "randomcovariates",
      CarEffect = "carcovariate",
      FixedEffects = "fixedcovariates"),
    functionArgumentHierarchy = list(
      AgeErrorMatrix = list(
        UseAgingError = TRUE
      ),
      CarNeighbours = list(
        UseCarEffect = TRUE
      ),
      CarEffect = list(
        UseCarEffect = TRUE
      ),
      StockSplittingParameters = list(
        UseStockSplitting = TRUE
      ),
      UseStockSplittingError = list(
        UseStockSplitting = TRUE
      )
    )
  ),
  ParameterizeRecaModels = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaParameterData"
    #doesnt work for directory ?
    #functionParameterFormat = list(
    #  ResultDirectory = "filePath"
    #)
  ),
  RunRecaModels = list(
    functionType = "modelData",
    functionCategory = "analysis",
    functionOutputDataType = "RecaCatchAtAge",
    functionParameterFormat = list(
      GroupingVariables = "GroupingVariables"
    )
  ),
  ReportFdaSampling = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaSamplingData",
    functionParameterFormat = list(
      GroupingVariables = "samplereportvariables",
      SamplingVariables = "onlysamplereportvariables"
    )
  ),
  ReportFdaLandings = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaLandingsData",
    functionParameterFormat = list(
      GroupingVariables = "landingsreportvariables"
    )
  ),
  ReportRecaCatchAtAge = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaCatchAtAgeData"
  ),
  ReportRecaCatchAtAgeCovariance = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaCatchAtAgeCovarianceData"
  ),
  ReportRecaCatchAtLength = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaCatchAtLengthData"
  ),
  ReportRecaCatchAtLengthAndAge = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaCatchAtLengthAndAgeData"
  ),
  ReportRecaLengthAtAge = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaLengthAtAgeData"
  ),
  ReportRecaWeightAtAge = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaWeightAtAgeData"
  ),
  ReportRecaCatchStatistics = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaCatchSummaryData",
    functionArgumentHierarchy = list(
      DecimalTotalNumber = list(
        UseDefaultDecimalOptions = FALSE
      ),
      DecimalTotalWeight = list(
        UseDefaultDecimalOptions = FALSE
      ),
      DecimalMeanAge = list(
        UseDefaultDecimalOptions = FALSE
      ),
      DecimalMeanWeight = list(
        UseDefaultDecimalOptions = FALSE
      ),
      DecimalMeanLength = list(
        UseDefaultDecimalOptions = FALSE
      ),
      UnitTotalNumber = list(
        UseDefaultUnitOptions = FALSE
      ),
      UnitTotalWeight = list(
        UseDefaultUnitOptions = FALSE
      ),
      UnitMeanWeight = list(
        UseDefaultUnitOptions = FALSE
      ),
      UnitMeanLength = list(
        UseDefaultUnitOptions = FALSE
      )
    )
  ),
  ReportFdaSOP = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ReportFdaSopData",
    functionParameterFormat = list(
      GroupingVariables = "GroupingVariablesSop"
    )
  ),
  ReportRecaParameterStatistics = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ParameterizationSummaryData",
    functionArgumentHierarchy = list(
      ParameterizationSummaryData = list(
        AppendReport = TRUE
      )
    )
  ),
  ReportParameterConvergence = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "ParameterConvergenceData"
  ),
  PlotSamplingOverviewCell = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "PlotSamplingOverviewCellData",
    functionParameterFormat = list(
      ColumnVariable = "columnvariablecellplot"
    ),
    functionArgumentHierarchy = list(
      MinVessels = list(
        UseDefaultColorScheme = FALSE
      ),
      MinCatches = list(
        UseDefaultColorScheme = FALSE
      ),
      MinMeasurements = list(
        UseDefaultColorScheme = FALSE
      ),
      ColorNoSamples = list(
        UseDefaultColorScheme = FALSE
      ),
      ColorFewCacthes = list(
        UseDefaultColorScheme = FALSE
      ),
      ColorFewVessels = list(
        UseDefaultColorScheme = FALSE
      ),
      ColorGoodSampling = list(
        UseDefaultColorScheme = FALSE
      )
    ),
    functionParameterDefaults = c(
      plotDefaultsCellPlotColors
    )
  ),
  PlotSamplingVariables = list(
    functionType = "modelData",
    functionCategory = "report",
    functionOutputDataType = "PlotSamplingVariablesData"
  )
)

#' Define the process property formats for inclusion in stox UI
#' 
#' @export
#' 
processPropertyFormats <- list(
  filePath = list(
    class = "single", 
    title = "The path to a single file"
  ),
  periodvector = list(
    class = "vector", 
    title = "Period defintinions. Start date on the form \"DD-MM\" or \"DD-MM-YYYY\"", 
    variableTypes = "character"
  ),
  randomcovariates = list(
    class = "vector", 
    title = "One or more variables to use as covariates in Reca", 
    possibleValues = function(StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[!(possibleValues %in% c("CruiseKey", "StationKey", "HaulKey", "SpeciesCategoryKey", "SampleKey"))]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  GroupingVariablesSop = list(
    class = "vector", 
    title = "One or more variables to use as aggregation variables.", 
    possibleValues = function(ReportFdaCatchAtAgeData, ReportFdaWeightAtAgeData, StoxLandingData) {
      possibleValues <- names(StoxLandingData$Landing)[(names(StoxLandingData$Landing) %in% ReportFdaCatchAtAgeData$GroupingVariables$GroupingVariables) &
                                                         (names(StoxLandingData$Landing) %in% ReportFdaWeightAtAgeData$GroupingVariables$GroupingVariables)]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  GroupingVariables = list(
    class = "vector", 
    title = "One or more variables to use as aggregation variables.", 
    possibleValues = function(StoxLandingData) {
      possibleValues <- names(StoxLandingData$Landing)[!(names(StoxLandingData$Landing) %in% c("RoundWeight"))]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  samplereportvariables = list(
    class = "vector", 
    title = "One or more variables to use as aggregation variables.", 
    possibleValues = function(StoxLandingData, StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[possibleValues %in% names(StoxLandingData$Landing)]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  onlysamplereportvariables = list(
    class = "vector", 
    title = "One or more variables to use as sampling variables.", 
    possibleValues = function(StoxBioticData) {
      possibleValues <- c()
      for (n in names(StoxBioticData)){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  landingsreportvariables = list(
    class = "vector", 
    title = "One or more variables to use as aggregation variables.", 
    possibleValues = function(StoxLandingData) {
      possibleValues <- names(StoxLandingData$Landing)[names(StoxLandingData$Landing) != "Rundvekt"]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  fixedcovariates = list(
    class = "vector", 
    title = "One or more variables to use as covariates in Reca", 
    possibleValues = function(StoxLandingData, StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[possibleValues %in% names(StoxLandingData$Landing)]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  carcovariate = list(
    class = "vector", #convert to class single, if that becomes available.
    title = "A variable (choose only one) to use for the CAR variable in Reca.",
    possibleValues = function(StoxBioticData) {
      possibleValues <- c()
      for (n in c("Station", "Haul", "SpeciesCategory", "Sample")){
        for (nn in names(StoxBioticData[[n]])){
          if (is.character(StoxBioticData[[n]][[nn]]) | is.factor(StoxBioticData[[n]][[nn]]) | is.integer(StoxBioticData[[n]][[nn]])){
            possibleValues <- c(possibleValues, nn)
          }
        }
      }
      possibleValues <- unique(possibleValues)
      possibleValues <- possibleValues[!(possibleValues %in% c("CruiseKey", "StationKey", "HaulKey", "SpeciesCategoryKey", "SampleKey"))]
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  ),
  columnvariablecellplot = list(
    class = "vector", #convert to class single, if that becomes available.
    title = "A variable (choose only one) to use for columns in cell plot.", 
    possibleValues = function(ReportFdaSamplingData) {
      possibleValues <- ReportFdaSamplingData$GroupingVariables$GroupingVariables
      return(sort(possibleValues))
    }, 
    variableTypes = "character"
  )
)

