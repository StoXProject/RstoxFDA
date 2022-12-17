# RstoxFDA
![R-CMD-check](https://github.com/StoXProject/RstoxFDA/workflows/R-CMD-check/badge.svg)
[![Codecov test coverage](https://codecov.io/gh/StoxProject/RstoxFDA/branch/master/graph/badge.svg)](https://codecov.io/gh/StoxProject/RstoxFDA?branch=master)

Fisheries Dependent Analysis with Rstox. Documentation can be found at: https://stoxproject.github.io/RstoxFDA/index.html . Functions, data formats, and data objects are documented under 'Reference' while tutorials to get started are provided under 'Articles'. 

## Installation

1. Install the latest release:
    ```r
    install.packages("RstoxFDA", repos = c("https://stoxproject.github.io/repo", "https://cloud.r-project.org"))
    ```

2. Install the latest version from GitHub:
    ```r
    remotes::install_github("https://github.com/StoXProject/RstoxFDA")
    ```

## StoX
RstoxFDA contains functions that adheres to StoX 3 function-contracts so that they can be included in StoX-processes via the StoX user interfaces. This includes functions for a StoX-Reca template. RstoxFDA does not come bundled with StoX, and needs to be installed in addition to the StoX packages, by following the instructions above.

## Not StoX
RstoxFDA is an analysis-library that also works independently of StoX and not all functions are available in the StoX user interface. The functions that are available in the StoX user interface starts with a capital letter and are listed at https://stoxproject.github.io/RstoxFDA/reference/index.html#stox-functions.

## Reporting problems
The preferred way to communicate problems is by raising an issue on the RstoxFDA github page. Users are encouraged to also report when documentation is lacking, erroneous or ambiguous.

When reporting bugs, please report the versions your were using of R, RstoxFDA, and operating system. Please also report any error messages, and if possible include instructions for how to reproduce the problem.

## Reca
Reca is a library for estimating total catch at age from commerical catches. RstoxFDA contains functions for adapting data to Reca, running estimates, and plotting or tabulating results. These functions are availble in the StoX user interface. In addition some functions are provided for adapting Reca to other data formats than just the ones supported by Stox.

Reca is primarily available at: https://github.com/NorskRegnesentral/Reca.


One may also consider installing from the fork at https://github.com/StoXProject/reca or from the StoX repository at https://stoxproject.github.io/repo/, but these resources should be considered experimental, and they are not backed by a maintenance policy: 

  ```r
  remotes::install_github("https://github.com/StoXProject/reca")
  ```

or

  ```r
  install.packages("Reca", repos=c("https://stoxproject.github.io/repo/"))
  ```


## Maintainance policy
We are still in the process of developing a policy for exactly which versions of R and operating systems we will strive to keep RstoxFDA working for, and how often to revise that policy. This page will be updated as those policies av finalized, but users should be prepared that it will be necessary to keep their environment up to date for continued used of RstoxFDA. We will also make public a policy on backwards compatibility, in order for us to manage deprecation of functions. 

Currently RstoxFDA is being tested for the following R versions:

* R 4.0
* R 4.1
* R 4.2
