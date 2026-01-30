# Metier table

Table
([`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html))
defining metiers.

## Details

Fishing activity metiers are approximate decompositions of fleets
commonly used in Europe. This table defines an approximate definition of
metiers by a custom gear code.

Metiers are used in EU-regulations for EU-member states, and are
therefore often required by ICES data formats Metiers are formal strings
encoding decomposition an idealized fleet or set of trips, where target
species and gear are clearly and unambigiously identified. The metier
system recognize that these parameters are not always clearly and
unamibigiously identified to the desired resolution, and allows for
grouping and omission of some parameters, and for coding that
information is missing. This makes the metier system very flexible, but
also provides poor standardization and ICES databases and data-calls may
provide code-lists of allowed metiers, sometimes with different metiers
being requested for different areas. In effect metier annotations cannot
be completely standardized, but must to be configurable through
conversion tables like this. Often the annotation has to be approximate.

The metier system is spesified by the EU - Data Collection Framework:
<https://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier>,
briefly a metier is fully specified by a string: \<gear\>\_\<target
species\>\_\<gear mesh size\>\_\<selectivity device\>\_\<selectivity
device mesh size\>\_\<vessel length class\>, with coding systems and
grouping conventions defined for each of the variables. Common
trunctations of these strings are also used. E.g.: Metier level 6 (no
truncation): \<gear\>\_\<target species\>\_\<gear mesh
size\>\_\<selectivity device\>\_\<selectivity device mesh size\> Metier
level 5 : \<gear\>\_\<target species\> Metier level 4 : \<gear\> The
term "metier" is also used for some derived systems, for instance some
data-calls requests that a code for intended usage of catch (industrial
vs human consumption) be appended to the metiers where gear and target
species is missing.

For example, in intercatch, the code OTB_DEF\_\>=120_0_0_all identifies
bottom trawl with otter boards (OTB) targeting demershal fish (DEF),
with mesh size larger than or equal to 120 mm, no selectivity device
(0_0) and all vessel size (all)

- metier:

  character() metier-string, e.g.: OTB_DEF\_\>=120_0_0_all

- gearcode:

  character() encoding gear

- target:

  character(), optional, target species

- meshedGear:

  logical(), optional, whether the gear is a meshed gear. Should be
  provided for all or none gears.

- lowerMeshSize:

  integer(), optional, the lower mesh size to include in this metier.
  Should be provided for all rows where meshedGear is True, and not for
  other rows.

- upperMeshSize:

  integer(), optional, the upper mesh size to include in this metier.
  Should be provided for all rows where meshedGear is True, and not for
  other rows.

- selectivityDevice:

  character(), optional, encoding selectivity device.

- meshedSelectivityDevice:

  logical(), optional, encoding selectivity device. Should be provided
  for all or none selectivity devices.

- selDevLowerMeshSize:

  integer(), optional, the lower mesh size of selectivity device to
  include in this metier. Should be provided for all rows where
  meshedSelectivityDevice is True, and not for other rows.

- selDevUpperMeshSize:

  integer(), optional, the upper mesh size of selectivity device to
  include in this metier. Should be provided for all rows where
  meshedSelectivityDevice is True, and not for other rows.

The metier-defining parameters are written in camelCase, parameters that
may be used to distinguish applicability of different metierdefinitions
are writter in UPPER case.
