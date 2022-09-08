# RstoxFDA
![R-CMD-check](https://github.com/StoXProject/RstoxFDA/workflows/R-CMD-check/badge.svg)
[![Codecov test coverage](https://codecov.io/gh/StoxProject/RstoxFDA/branch/master/graph/badge.svg)](https://codecov.io/gh/StoxProject/RstoxFDA?branch=master)

Fisheries Dependent Analysis with Rstox. Documentation can be found at: https://stoxproject.github.io/RstoxFDA/index.html . Functions, data formats, and data objects are documented under 'Reference' while tutorials to get started are provided under 'Articles'. 

## Installation
Install from the StoX project repository with:
install.packages("RstoxFDA", repos = "https://stoxproject.github.io/repo/").

## StoX
RstoxFDA contains functions that adheres to StoX 3 function-contracts so that they can be included in StoX-processes via the StoX user interfaces. This includes functions for a StoX-Reca template. RstoxFDA does not come bundled with StoX, and needs to be installed in addition to the StoX packages, by following the instructions above.

## Not StoX
RstoxFDA is an analysis-library that also works independently of StoX and not all functions are available in the StoX user interface. The functions that are available in the StoX user interface are starts with a capital letter, as does data formats and data objects.

## Reca
RstoxFDA contains functions for adapting data to Reca, running estimates, and plotting or tabulating results. These functions are formulated to be adaptable to other data formats than just the ones supported by Rstox.

Reca is primarily available at: https://github.com/NorskRegnesentral/Reca.
One may also consider installing from the fork at: https://github.com/StoXProject/reca, or at https://stoxproject.github.io/repo/: install.packages("Reca", repos=c("https://stoxproject.github.io/repo/")), but these resources should be considered experimental, and they are not backed by a maintenance policy.

## Maintainance policy
We are still in the process of developing a policy for exactly which versions of R and operating systems we will strive to keep RstoxFDA working for, and how often to revise that policy. This page will be updated as those policies av finalized, but users should be prepared that it will be necessary to keep their environment up to date for continued used of RstoxFDA. We will also make public a policy on backwards compatibility, in order for us to manage deprecation of functions.