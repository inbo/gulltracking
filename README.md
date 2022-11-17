# gulltracking

<!-- badges: start -->
[![repo
status](https://www.repostatus.org/badges/latest/abandoned.svg)](https://www.repostatus.org/#abandoned)
[![R build status](https://github.com/inbo/gulltracking/workflows/R-CMD-check/badge.svg)](https://github.com/inbo/gulltracking/actions)
[![Codecov test coverage](https://codecov.io/gh/inbo/gulltracking/branch/master/graph/badge.svg)](https://codecov.io/gh/inbo/gulltracking?branch=master)
<!-- badges: end -->

gulltracking provides functionality to annotate GPS tracking data of gulls stored in [Movebank](https://www.movebank.org/). These data are collected by the LifeWatch [GPS tracking network for large birds](http://lifewatch.be/en/gps-tracking-network-large-birds).

Development for this package was abandoned because the intended functionality is maintained elsewhere (e.g. [Gull_tracking](https://github.com/ReinoudAllaert/Gull_tracking), [bird-tracking](https://github.com/inbo/bird-tracking), [movepub](https://github.com/inbo/movepub)). See the [bt-etl](https://github.com/inbo/gulltracking/releases/tag/bt-etl) release for the _bird-tracking-etl_ package this repository used to contain, to process and enrich bird tracking data from UvA-BiTS.

## Installation

You can install gulltracking from [GitHub](https://github.com/inbo/gulltracking) with:

```r
# install.packages("devtools")
devtools::install_github("inbo/gulltracking")
```

Then load the package with:

```r
library(gulltracking)
```

## Meta

* We welcome [contributions](.github/CONTRIBUTING.md) including bug reports.
* License: MIT
* Get citation information for `gulltracking` in R doing `citation("gulltracking")`.
* Please note that this project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
