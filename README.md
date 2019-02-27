# uvabits

> ✋ This package is not ready for use! It was previously called `BirdTrackingEtl` and used exclusively to process UvA-BiTS data for INBO researchers (see [release](https://github.com/inbo/uvabits/releases/tag/bt-etl) and [vignette](vignettes/bird-tracking-etl.Rmd) for that version). We are now expanding its functionality for more general use by UvA-BiTS users. Want to contribute? Check out our [contributing guidelines](.github/CONTRIBUTING.md).

uvabits provides an R interface to the [UvA-BiTS database](http://www.uva-bits.nl/), which stores bird movement data collected with UvA-BiTS GPS trackers. The package provides functionality to download data and metadata, calculate some metrics, and load the data into a  query-optimized SQLite database for local analysis. It also allows to download the data in a format that can easily be uploaded to [Movebank](https://www.movebank.org/), a free online database for animal tracking data.

## Installation

You can install uvabits from [GitHub](https://github.com/inbo/uvabits) with:

```r
# install.packages("devtools")
devtools::install_github("inbo/uvabits")
```

Then load the package with:

```r
library(uvabits)
```

## Meta

* We welcome [contributions](.github/CONTRIBUTING.md) including bug reports.
* License: MIT
* Get citation information for `uvabits` in R doing `citation("uvabits")`.
* Please note that this project is released with a [Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.