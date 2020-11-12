library(dplyr)
library(readr)

#' Movebank GPS data
#' Header and first 100 lines of GPS data from 2013
#' https://zenodo.org/record/3968687/files/LBBG_ZEEBRUGGE-gps-2013.csv.zip?download=1
gps_from_movebank <- read_csv(
  file = "./input_data_append_metadata/test_gps_data.csv")

#' Movebank reference data
#' https://zenodo.org/record/3968687/files/LBBG_ZEEBRUGGE-reference-data.csv?download=1
reference_from_movebank <- read_csv(
  file = "./input_data_append_metadata/test_reference_data.csv")

test_that("error arises if gps and/or references are of the wrong class", {
  expect_error(append_metadata(gps = 1,
                               reference_data = reference_from_movebank),
              "`gps` must be of class data.table, data.frame or matrix.")
  expect_error(append_metadata(gps = gps_from_movebank,
                               reference_data = "bad bad bad"),
               "`reference_data` must be of class data.table, data.frame or matrix.")
})

test_that("gps doesn't contain one or more of the mandatory columns", {

  gps_without_canonical_name <- gps_from_movebank %>%
    select(-"individual-taxon-canonical-name")

  expect_error(append_metadata(gps = gps_without_canonical_name,
                               reference_data = reference_from_movebank),
               "Can't find column(s) `individual-taxon-canonical-name` in `gps`.",
               fixed = TRUE)

  gps_without_canonical_name_identifier <- gps_from_movebank %>%
    select(-c("individual-taxon-canonical-name", "tag-local-identifier"))

  expect_error(append_metadata(gps = gps_without_canonical_name_identifier,
                               reference_data = reference_from_movebank),
               paste0(
                 "Can't find column(s) `individual-taxon-canonical-name`,",
                 "`tag-local-identifier` in `gps`."),
               fixed = TRUE)
})

test_that("reference_data doesn't contain mandatory columns", {

  ref_cols_too_few = c("animal-taxon")

  expect_error(append_metadata(gps = gps_from_movebank,
                               reference_data = reference_from_movebank,
                               reference_cols = ref_cols_too_few),
               paste0("reference_data column(s) `tag-id`,",
                      "`animal-id` must be selected."),
               fixed = TRUE)
})

test_that("reference_data doesn't contain all columns we want to add to gps", {

  reference_without_comments <-
    reference_from_movebank %>%
    select(-"animal-comments")
  ref_cols_test <- c("animal-taxon",
                     "tag-id",
                     "animal-id",
                     "animal-comments")

  expect_error(append_metadata(gps = gps_from_movebank,
                               reference_data = reference_without_comments,
                               reference_cols = ref_cols_test),
               "Can't find column(s) `animal-comments` in `reference_data`.",
               fixed = TRUE)
})

test_that("output is a data.table object",
          expect_true("data.table" %in%
                        class(append_metadata(gps_from_movebank,
                                              reference_from_movebank))))

test_that("output has all columns from gps and reference data in right order", {
  # Arrange
  cols_from_gps_minimal <- c("event-id",
                     "location-long",
                     "location-lat",
                     "gps:dop",
                     "gps:satellite-count")
  cols_from_gps_for_join <- c("individual-taxon-canonical-name",
                              "tag-local-identifier",
                              "individual-local-identifier")
  gps_minimal <-
    gps_from_movebank %>%
    select(one_of(c(cols_from_gps_minimal, cols_from_gps_for_join)))

  cols_from_ref_minimal <- c("animal-taxon",
                             "tag-id",
                             "animal-id",
                             "deploy-on-date",
                             "deploy-off-date",
                             "animal-nickname",
                             "animal-ring-id")
  ref_minimal <-
    reference_from_movebank %>%
    select(cols_from_ref_minimal)

  output_col_names <- c(cols_from_gps_minimal, cols_from_ref_minimal)

  # Act
  output <- append_metadata(gps_minimal,
                         ref_minimal,
                         reference_cols = cols_from_ref_minimal)

  # Assert
  expect_true(all(output_col_names == colnames(output)))
})
