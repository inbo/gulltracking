library(dplyr)

test_that("error arises if gps and/or references are of the wrong class", {
  expect_error(
    append_metadata(
      gps = 1,
      ref_data = lbbg_ref_data
    ),
    "`gps` must be of class data.table, data.frame or matrix."
  )
  expect_error(
    append_metadata(
      gps = lbbg_gps,
      ref_data = "bad bad bad"
    ),
    "`ref_data` must be of class data.table, data.frame or matrix."
  )
})

test_that("gps doesn't contain one or more of the mandatory columns", {
  gps_without_canonical_name <- lbbg_gps %>%
    select(-"individual-taxon-canonical-name")

  expect_error(append_metadata(
    gps = gps_without_canonical_name,
    ref_data = lbbg_ref_data
  ),
  "Can't find column(s) `individual-taxon-canonical-name` in `gps`.",
  fixed = TRUE
  )

  gps_without_canonical_name_identifier <- lbbg_gps %>%
    select(-c("individual-taxon-canonical-name", "tag-local-identifier"))

  expect_error(append_metadata(
    gps = gps_without_canonical_name_identifier,
    ref_data = lbbg_ref_data
  ),
  paste0(
    "Can't find column(s) `individual-taxon-canonical-name`,",
    "`tag-local-identifier` in `gps`."
  ),
  fixed = TRUE
  )
})

test_that("ref_data doesn't contain mandatory columns", {
  ref_cols_too_few <- c("animal-taxon")

  expect_error(append_metadata(
    gps = lbbg_gps,
    ref_data = lbbg_ref_data,
    ref_cols = ref_cols_too_few
  ),
  paste0(
    "ref_cols should (also) contain `tag-id`,",
    "`animal-id` to join data."
  ),
  fixed = TRUE
  )
})

test_that("ref_data doesn't contain all columns we want to add to gps", {
  reference_without_comments <-
    lbbg_ref_data %>%
    select(-"animal-comments")
  ref_cols_test <- c(
    "animal-taxon",
    "tag-id",
    "animal-id",
    "animal-comments"
  )

  expect_error(append_metadata(
    gps = lbbg_gps,
    ref_data = reference_without_comments,
    ref_cols = ref_cols_test
  ),
  "Can't find column(s) `animal-comments` in `ref_data`.",
  fixed = TRUE
  )
})

test_that(
  "output is a data.table object",
  expect_true("data.table" %in%
    class(append_metadata(
      lbbg_gps,
      lbbg_ref_data
    )))
)

test_that("output has all columns from gps and reference data in right order", {
  # Arrange
  cols_from_gps_minimal <- c(
    "event-id",
    "location-long",
    "location-lat",
    "gps:dop",
    "gps:satellite-count"
  )
  cols_from_gps_for_join <- c(
    "individual-taxon-canonical-name",
    "tag-local-identifier",
    "individual-local-identifier"
  )
  gps_minimal <-
    lbbg_gps %>%
    select(one_of(c(cols_from_gps_minimal, cols_from_gps_for_join)))

  cols_from_ref_minimal <- c(
    "animal-taxon",
    "tag-id",
    "animal-id",
    "deploy-on-date",
    "deploy-off-date",
    "animal-nickname",
    "animal-ring-id"
  )
  ref_minimal <-
    lbbg_ref_data %>%
    select(cols_from_ref_minimal)

  output_col_names <- c(cols_from_gps_minimal, cols_from_ref_minimal)

  # Act
  output <- append_metadata(gps_minimal,
    ref_minimal,
    ref_cols = cols_from_ref_minimal
  )

  # Assert
  expect_true(all(output_col_names == colnames(output)))
})


test_that("warning is returned if and only if gps and ref_data have one or more columns with same name", {

  # Arrange
  lbbg_gps[["sensor-type"]] <- "A"
  lbbg_gps[["sensor-model"]] <- "3"
  lbbg_ref_data[["sensor-type"]] <- "B"
  lbbg_ref_data[["sensor-model"]] <- "3"

  # Act
  output <- evaluate_promise(
    append_metadata(lbbg_gps,
                    lbbg_ref_data,
                    ref_cols = c(
                      "animal-taxon",
                      "tag-id",
                      "animal-id",
                      "sensor-type", # shared column
                      "sensor-model", # shared column
                      "animal-comments",
                      "animal-mass"
                    )
    )
  )

  # Assert
  expect_equal(output$warning,
               paste0("The following columns of ref_data will be dropped",
                           " as they are present in gps as well:",
                           " `sensor-type`,`sensor-model`.")
  )

  # if shared columns are not selected for join, no warnings is returned (NA)
  output <- evaluate_promise(append_metadata(lbbg_gps, lbbg_ref_data))
  expect_equal(length(output$warning), 0)
  # but they are still present in output as they are in gps
  expect_true(all(c("sensor-type", "sensor-model") %in% names(output$result)))

})
