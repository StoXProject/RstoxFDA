% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/StoxDataTypes.R
\name{LocodeTable}
\alias{LocodeTable}
\title{LOCODE table (LocodeTable)}
\description{
Table of United Nations Codes for Trade and Transport Locations  (UN/LOCODE)
 
 \describe{
  \item{Country}{ISO 3166 alpha-2 Country Code for the country in which the location is placed.}
  \item{Location}{3-character code identifying the location within country}
  \item{NameWoDiacritics}{Name of location without diacritic signs.}
  \item{Subdivision}{ISO 1-3 character alphabetic and/or numeric code for the administrative division of the country concerned (the latter part of the complete ISO 3166-2 code element (after the hyphen).)}
  \item{Status}{code for status of the code. E.g. Codes starting with A are various official approvals.}
  \item{Function}{string with codes classifying the function of the location. If this contains 1, the location has a seaport.}
  \item{Coordinates}{latitude and longitude in degrees and minutes.}
  \item{Remarks}{general remark about location or change in coding}
  \item{latitude}{latitude in decimal degrees}
  \item{longitude}{longitude in decimal degrees}
 }
}
\concept{Data types}
