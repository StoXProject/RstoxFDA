# Logbooks (LstLogbookData)

Logbooks read from the lst format delivered by Directorate of Fisheries
(FDIR).

This format is not matched with WMS records and contains less detail
than the format read by
[`readErsFile`](https://rdrr.io/pkg/RstoxData/man/readErsFile.html)

Each row represent one fishing operation, which is defined in the
legislation current at the time of reporting.

Additional documentation (in Norwegian) can be found in the file:
doc/documentation_logbookdata_lst.xls

- FAAR:

  character. Year of catch (4 digits)

- REGM:

  character License number of fishing vessel (registreringsmerke)

- FM:

  character. Month of catch. number with leading zeroes. 1=January,
  12=December

- FD:

  character. Day of catch. number with leading zeroes

- DBNR:

  character. Logbook number.

- TUR:

  character. Trip number.

- FM:

  character. Month of departure (start of trip). number with leading
  zeroes. 1=January, 12=December

- FD:

  character. Day of departure (start of trip). number with leading
  zeroes.

- AH:

  character. Port of departure (start of trip). Code identifying port.

- LM:

  character. Month of landing of catch. number with leading zeroes.
  1=January, 12=December

- LD:

  character. Day of landing of catch. number with leading zeroes.

- LH:

  character. Port where catch was landed. Code identifying port.

- RE:

  character. Gear. Main gear for fishing operation definitions that
  allow several.

- MA:

  character. Mesh size (mm) for meshed gear.

- HA:

  character. Number of hauls/sets for fishing operation definitions that
  allow several.

- VAR:

  numeric. Total fishing time (hours).

- OMRA:

  character. International area code (ICES, NAFO, etc.)

- OKSO:

  character. Economic zone. Three letter code.

- HO:

  character. Main area of catch. As identified by the column
  'StratumName' in [`mainareaFdir2017`](mainareaFdir2017.md) or
  [`mainareaFdir2018`](mainareaFdir2018.md))

- LO:

  character. Location of catch. As identified by the column 'Lokasjon'
  in [`locationsFdir2017`](locationsFdir2017.md) or
  [`locationsFdir2018`](locationsFdir2018.md), together with Main area
  (the column HAVOMR)

- LENG:

  numeric. vessel length (m).

- BTON:

  character. Gross tonnage of vessel.

- TENH:

  character. Tonnage units of vessel.

- HEST:

  character. Engine effect of vessel (Hp)

- FISK:

  character. Code for the species landed. NS9400.

- VEKT:

  numeric Liveweight (Roundweight) in kg.
