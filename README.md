# RstoxFDA
![R-CMD-check](https://github.com/StoXProject/RstoxFDA/workflows/R-CMD-check/badge.svg)
[![Codecov test coverage](https://codecov.io/gh/StoxProject/RstoxFDA/branch/master/graph/badge.svg)](https://codecov.io/gh/StoxProject/RstoxFDA?branch=master)

Fisheries Dependent Analysis with Rstox.

Install from github with:
devtools::install_github("https://github.com/StoXProject/RstoxFDA").
Precompiled versions at https://stoxproject.github.io/repo/ may also be considered.

## Reca
RstoxFDA contains functions for adapting data to Reca, running estimates, and plotting or tabulating results. These functions are formulated to be adaptable to other data formats than just the ones supported by Rstox.

Reca is available at: https://github.com/NorskRegnesentral/Reca .
For Windows and Linux, Reca can be installed via devtools::install_github("https://github.com/NorskRegnesentral/Reca").
For Mac, one might consider the fork at: https://github.com/StoXProject/reca. Precompiled versions at https://stoxproject.github.io/repo/ may also be considered: install.packages("Reca", repos=c("https://stoxproject.github.io/repo/")).

## StoX
RstoxFDA contains functions that adheres to StoX 3.0 function-contracts so that they can be included in StoX-processes. This includes functions for a StoX-Reca template.
