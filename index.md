[![R-CMD-check](https://github.com/KWB-R/kwb.read/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.read/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.read/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.read/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.read/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.read)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.read)]()

Functions reading specific input data, e.g. files provided by BWB. 
E.g. rain data (5 min values) and rain correction tables (on daily basis).

## Installation

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

remotes::install_github("KWB-R/kwb.read")
```
