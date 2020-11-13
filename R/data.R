#' Sample of gull GPS data
#'
#' A sample dataset of gull GPS data (100 records from 2013) in Movebank format.
#'
#' @source [Stienen et al. 2020](https://doi.org/10.5281/zenodo.3968687)
#'
#' @examples
#' \dontrun{
#' library(readr)
#' temp <- tempfile()
#' download.file("https://zenodo.org/record/3968687/files/LBBG_ZEEBRUGGE-gps-2013.csv.zip", temp)
#' gps <- read_csv(unzip(temp))
#' gps <- head(gps, 100)
#' save(gps, file = "data/lbbg_gps.rda")
#' }
"lbbg_gps"

#' Sample of gull reference data
#'
#' A sample dataset of gull reference data in Movebank format.
#'
#' @source [Stienen et al. 2020](https://doi.org/10.5281/zenodo.3968687)
#'
#' @examples
#' \dontrun{
#' ref_data <- read_csv("https://zenodo.org/record/3968687/files/LBBG_ZEEBRUGGE-reference-data.csv")
#' save(ref_data, file = "data/lbbg_ref_data.rda")
#' }
"lbbg_ref_data"
