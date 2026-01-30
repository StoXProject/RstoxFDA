# Landings archive (LandingsArchiveData)

Landings (aggregated sales notes). Format used for landings archive
delivered to IMR by FDIR. Additional documentation (in Norwegian) can be
found in the files:
docs/documentation_landingsdata_archive_norwegian.csv
docs/selected_code_lists_landingsdata_archive_norwegian.xlsx
docs/supplementary_documentation_landingsdata_archive_norwegian.csv

Note that 'AAR' and 'AAR2' denotes the year of catch, which may differ
from the year of landing. The convention has been adopted to use 13 for
'LEVMND' (month of landing), when catch has been landed in January the
year after catch.

- AAR:

  integer. Two last digits of year of catch

- AAR2:

  integer. Year of catch (4 digits)

- FARTLAND:

  character. Code for vessel flag (NOR for Norwegian, UTL for
  non-Norwegian)

- LEVAAR:

  integer. Two last digits of year of landing)

- LEVMND:

  integer. Month of landing (1=January, 12=December, 13=January
  following year)). Values larger than 13 are likely errors.

- KYST:

  integer. Code for whether the catch was caught within the coastal
  region (12 nautical miles from the coast). Code 0 denotes oceanic
  catch, code 8 and 9 denotes coastal catch.)

- HOMR:

  character. Main area of catch. As identified by the column
  'StratumName' in [`mainareaFdir2017`](mainareaFdir2017.md)), except
  that leading zeroes are not used for the areas 0-9.

- LOK:

  character. Location of catch. As identified by the column 'Lokasjon'
  in [`locationsFdir2017`](locationsFdir2017.md)), together with Main
  area (the column HAVOMR), except neither use leading zeroes.

- REDS:

  character. Gear code as defined by the standard NS9400. See code
  lists.

- LEVHERRD:

  character. Common official code for the muncipality (kommune) the
  catch was landed. Not using leading zeroes

- LEVHERRD2:

  character. Common official code for the muncipality (kommune) the
  catch was landed. Using leading zeroes

- LEVFYLKE:

  character. Common official code for the muncipality (fylke) the catch
  was landed. Same as the two leading digits in LEVHERRD2. Using leading
  zeroes

- FISK:

  character. Code for the species landed

- FISK_NAVN:

  character. Norwegian name of the species landed.

- BIPROD:

  character. Code for the product landed (0 codes for main-product, 1-8
  codes for bi-products.)

- ANVEND:

  character. Code for the usage of the landing (human consumption vs
  industiral usage). See code lists.)

- UTBET:

  numeric. The prize paid to fisher in Norwegian currency at the time of
  purchase. Consult supplementary documentation for details.

- VEKT:

  numeric. Live weight (Round weight) of landed catch in kg. Listed as
  zero for bi-products.
