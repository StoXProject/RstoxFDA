# Trip Partition

Partitioning of catch from a trip.

## Details

list with two members 'fractions' and 'groupDefinition'

'fractions' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns:

- tripid:

  trip identifier

- species:

  species identifier

- groupid:

  identifies the group the fraction is provided for. Groups are further
  specified in 'groupDefinition'

- fraction:

  fraction of the total catch of species in the given group

'groupDefinition' is a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns:

- groupid:

  identifies the group the defintion is provided for.

- ...:

  one or more custom columns definint the group
