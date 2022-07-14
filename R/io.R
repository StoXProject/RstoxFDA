
#' @noRd
getLssSpec <- function(){
  spec_land <- list(
    Dokumentnummer = "character",
    `Dokumenttype (kode)` = "character",
    Dokumenttype = "character",
    `Dokument versjonsnummer` = "character",
    `Dokument salgsdato` = "character",
    `Dokument versjonstidspunkt` = "character",
    `Salgslag ID` = "character",
    `Salgslag (kode)` = "character",
    Salgslag = "character",
    `Mottakernasjonalitet (kode)` = "character",
    Mottakernasjonalitet = "character",
    Mottaksstasjon = "character",
    `Landingskommune (kode)` = "character",
    Landingskommune = "character",
    `Landingsfylke (kode)` = "character",
    Landingsfylke = "character",
    `Landingsnasjon (kode)` = "character",
    Landingsnasjon = "character",
    Produksjonsanlegg = "character",
    `Produksjonskommune (kode)` = "character",
    Produksjonskommune = "character",
    `Fiskerkommune (kode)` = "character",
    Fiskerkommune = "character",
    `Fiskernasjonalitet (kode)` = "character",
    Fiskernasjonalitet = "character",
    Fartoynavn = "character",
    `Fartoy ID` = "character",
    `Registreringsmerke (seddel)` = "character",
    `Radiokallesignal (seddel)` = "character",
    `Storste lengde` = "numeric",
    `Lengdegruppe (kode)` = "character",
    Lengdegruppe = "character",
    `Bruttotonnasje 1969` = "numeric",
    `Bruttotonnasje annen` = "numeric",
    Byggear = "integer",
    Ombyggingsar = "integer",
    Motorkraft = "numeric",
    Motorbyggear = "integer",
    `Fartoy gjelder fra dato` = "character",
    `Fartoy gjelder til dato` = "character",
    `Fartoytype (kode)` = "character",
    Fartoytype = "character",
    `Kvotefartoy reg.merke` = "character",
    `Fartoykommune (kode)` = "character",
    Fartoykommune = "character",
    `Fartoyfylke (kode)` = "character",
    Fartoyfylke = "character",
    `Fartoynasjonalitet (kode)` = "character",
    Fartoynasjonalitet = "character",
    `Mottakende fartoy reg.merke` = "character",
    `Mottakende fartoy rkal` = "character",
    `Mottakende fartoytype (kode)` = "character",
    `Mottakende fart.type` = "character",
    `Mottakende fartoynasj. (kode)` = "character",
    `Mottakende fart.nasj` = "character",
    Fangstar = "integer",
    `Siste fangstdato` = "character",
    `Kvotetype (kode)` = "character",
    Kvotetype = "character",
    `Redskap (kode)` = "character",
    Redskap = "character",
    `Redskap - hovedgruppe (kode)` = "character",
    `Redskap - hovedgruppe` = "character",
    `Fangstfelt (kode)` = "character",
    `Kyst/hav (kode)` = "character",
    `Hovedomrade (kode)` = "character",
    Hovedomrade = "character",
    `Lokasjon (kode)` = "character",
    `Sone (kode)` = "character",
    Sone = "character",
    Omradegruppering = "character",
    `Hovedomrade FAO (kode)` = "character",
    `Hovedomrade FAO` = "character",
    `Nord/sor for 62 grader nord` = "character",
    `Fangstdagbok (nummer)` = "character",
    `Fangstdagbok (turnummer)` = "character",
    Landingsdato = "character",
    Landingsklokkeslett = "character",
    `Dellanding (signal)` = "character",
    `Neste mottaksstasjon` = "character",
    `Forrige mottakstasjon` = "character",
    Linjenummer = "integer",
    `Art - FDIR (kode)` = "character",
    `Art - FDIR` = "character",
    `Art - gruppe (kode)` = "character",
    `Art - gruppe` = "character",
    `Art - hovedgruppe (kode)` = "character",
    `Art - hovedgruppe` = "character",
    `Art FAO (kode)` = "character",
    `Art FAO` = "character",
    `Produkttilstand (kode)` = "character",
    Produkttilstand = "character",
    `Konserveringsmate (kode)` = "character",
    Konserveringsmate = "character",
    `Landingsmate (kode)` = "character",
    Landingsmate = "character",
    `Kvalitet (kode)` = "character",
    Kvalitet = "character",
    `Storrelsesgruppering (kode)` = "character",
    `Anvendelse (kode)` = "character",
    Anvendelse = "character",
    `Anvendelse hovedgruppe (kode)` = "character",
    `Anvendelse hovedgruppe` = "character",
    `Antall stykk` = "integer",
    Bruttovekt = "numeric",
    Produktvekt = "numeric",
    Rundvekt = "numeric"
  )
  names(spec_land)[26] <- "Fart\u00F8ynavn"
  names(spec_land)[27] <- "Fart\u00F8y ID"
  names(spec_land)[30] <- "St\u00F8rste lengde"
  names(spec_land)[35] <- "Bygge\u00E5r"
  names(spec_land)[36] <- "Ombyggings\u00E5r"
  names(spec_land)[38] <- "Motorbygge\u00E5r"
  names(spec_land)[39] <- "Fart\u00F8y gjelder fra dato"
  names(spec_land)[40] <- "Fart\u00F8y gjelder til dato"
  names(spec_land)[41] <- "Fart\u00F8ytype (kode)"
  names(spec_land)[42] <- "Fart\u00F8ytype"
  names(spec_land)[43] <- "Kvotefart\u00F8y reg.merke"
  names(spec_land)[44] <- "Fart\u00F8ykommune (kode)"
  names(spec_land)[45] <- "Fart\u00F8ykommune"
  names(spec_land)[46] <- "Fart\u00F8yfylke (kode)"
  names(spec_land)[47] <- "Fart\u00F8yfylke"
  names(spec_land)[48] <- "Fart\u00F8ynasjonalitet (kode)"
  names(spec_land)[49] <- "Fart\u00F8ynasjonalitet"
  names(spec_land)[50] <- "Mottakende fart\u00F8y reg.merke"
  names(spec_land)[51] <- "Mottakende fart\u00F8y rkal"
  names(spec_land)[52] <- "Mottakende fart\u00F8ytype (kode)"
  names(spec_land)[54] <- "Mottakende fart\u00F8ynasj. (kode)"
  names(spec_land)[56] <- "Fangst\u00E5r"
  names(spec_land)[66] <- "Hovedomr\u00E5de (kode)"
  names(spec_land)[67] <- "Hovedomr\u00E5de"
  names(spec_land)[71] <- "Omr\u00E5degruppering"
  names(spec_land)[72] <- "Hovedomr\u00E5de FAO (kode)"
  names(spec_land)[73] <- "Hovedomr\u00E5de FAO"
  names(spec_land)[74] <- "Nord/s\u00F8r for 62 grader nord"
  names(spec_land)[93] <- "Konserveringsm\u00E5te (kode)"
  names(spec_land)[94] <- "Konserveringsm\u00E5te"
  names(spec_land)[95] <- "Landingsm\u00E5te (kode)"
  names(spec_land)[96] <- "Landingsm\u00E5te"
  names(spec_land)[99] <- "St\u00F8rrelsesgruppering (kode)"
  return(spec_land)
}

#' @noRd
convertToLssData <- function(LandingData=NULL, openFdirData=NULL){
  
  if (sum(c(!is.null(LandingData), !is.null(openFdirData))) != 1){
    stop("Provide argument for exactly one of the supported formats")
  }
  
  if (!is.null(LandingData)){
    return(RstoxData::convertToLssData(LandingData))
  }
  
  if (!is.null(openFdirData)){
    landspec <- getLssSpec()
    landspec[["Fart\u00F8y gjelder fra dato"]] = NULL
    landspec[["Fart\u00F8y gjelder til dato"]] = NULL
    
    if (!all(names(landspec) %in% names(openFdirData))){
      missing <- names(landspec)[!(names(landspec) %in% names(openFdirData))]
      warning(paste("Not all expected columns found. Missing: ", paste(missing, collapse=",")))
    }
    columns <- names(landspec)[names(landspec) %in% names(openFdirData)]
    conv <- openFdirData[,.SD, .SDcols=columns]
    conv[["Fart\u00F8y gjelder fra dato"]] <- ""
    conv[["Fart\u00F8y gjelder fra dato"]] <- as.character(NA)
    conv[["Fart\u00F8y gjelder til dato"]] <- ""
    conv[["Fart\u00F8y gjelder til dato"]] <- as.character(NA)
    
    order <- names(getLssSpec())
    return(conv[, .SD, .SDcols=order])
  }
}

#' @noRd
readFdirOpenLandings <- function(filename, encoding="UTF-8"){
 
  spec_land <- getLssSpec() 
  
  #
  # Above is identical to Lss, except some validitydates for vessel ids are removed,
  # and some other columns are added
  #
  
  spec_land[["Fart\u00F8y gjelder fra dato"]] = NULL
  spec_land[["Fart\u00F8y gjelder til dato"]] = NULL
  
  additions <- list(
    `Landingstidspunkt` = "character",
    `Mottaker ID`= "character",
    `Fisker ID`= "character",
    `Besetning` = "numeric",
    `Fartoynasjonalitet gruppe` = "character",
    `Redskap - gruppe (kode)` = "character",
    `Redskap - gruppe` = "character",
    `Lon (hovedomrade)` = "numeric",
    `Lat (hovedomrade)` = "numeric",
    `Lon (lokasjon)` = "numeric",
    `Lat (lokasjon)` = "numeric",
    `Omradegruppering (kode)` = "character",
    `Landingsmaned (kode)` = "character",
    `Landingsmaned` = "character",
    `Landingstidspunkt` = "character",
    `Art (kode)` = "character",
    `Art` = "character",
    `Produktvekt over kvote` = "numeric",
    `Rundvekt over kvote` = "numeric",
    `Enhetspris for kjoper` = "numeric",
    `Belop for kjoper` = "numeric",
    `Enhetspris for fisker` = "numeric",
    `Belop for fisker` = "numeric",
    `Stottebelop` = "numeric",
    `Lagsavgift` = "numeric",
    `Inndradd fangstverdi` = "numeric",
    `Etterbetaling` = "numeric",
    `Fangstverdi` = "numeric",
    `Oppdateringstidspunkt` = "character"
  )
  
  names(additions)[5] <- "Fart\u00F8ynasjonalitet gruppe"
  names(additions)[8] <- "Lon (hovedomr\u00E5de)"
  names(additions)[9] <- "Lat (hovedomr\u00E5de)"
  names(additions)[12] <- "Omr\u00E5degruppering (kode)"
  names(additions)[13] <- "Landingsm\u00E5ned (kode)" 
  names(additions)[14] <- "Landingsm\u00E5ned" 
  names(additions)[20] <- "Enhetspris for kj\u00F8per"
  names(additions)[21] <- "Bel\u00F8p for kj\u00F8per"
  names(additions)[23] <- "Bel\u00F8p for fisker"
  names(additions)[24] <- "St\u00F8ttebel\u00F8p"
  
  cols <- append(spec_land, additions)
  
  ss<-data.table::fread(filename, encoding = encoding, sep=";", dec=",", header = T, nrows = 1)
  
  if (!all(names(ss) %in% names(cols))){
    missing <- names(ss)[!(names(ss) %in% names(cols))]
    stop(paste("Unspecified columns found:", paste(missing, collapse=",")))
  }
  if (!all(names(cols) %in% names(ss))){
    missing <- names(cols)[!(names(cols) %in% names(ss))]
    warning(paste("Not all expected columns found:", paste(missing, collapse=",")))
  }
  
  colClasses <- unlist(cols[names(ss)])
  ss<-data.table::fread(filename, encoding = encoding, sep=";", dec=",", header = T, colClasses = colClasses,strip.white=TRUE, na.strings=c("", "na", "NA"))
  names(ss) <- names(colClasses)
  
  ss$`Siste fangstdato` <- as.POSIXct(ss$`Siste fangstdato`, format="%d.%m.%Y", tz="CET")

  return(ss)
}

#' Reads landings archive
#' @description 
#'  Reads aggregated sales notes from archive format deliver by FDIR to IMR. E.g. sluttseddel_1978_2004_medVerdi.csv
#' @param filename file to read the archive from
#' @param encoding encoding of the file identified by filename
#' @return LandingsArchiveData
#' @export
readFdirLandingsArchive <- function(filename, encoding = "Latin-1"){
  
  spec_arch <- list(
    AAR = "integer",
    AAR2 = "integer",
    FARTLAND = "character",
    LEVAAR = "integer",
    LEVMND = "integer",
    KYST = "character",
    HOMR = "character",
    LOK = "character",
    REDS = "character",
    LEVHERRD = "character",
    LEVHERRD2 = "character",
    LEVFYLKE = "character",
    FISK = "character",
    FISK_NAVN = "character",
    BIPROD = "character",
    ANVEND = "character",
    UTBET = "numeric",
    VEKT = "numeric"
  )
  
  landings <- data.table::fread(filename, encoding = encoding, na.strings = c("(null)"), sep=";", dec=",", header = T, colClasses = unlist(spec_arch))
  return(landings)
}