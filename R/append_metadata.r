#' Merge Movebank GPS event data and reference data
#'
#' This function adds specific Movebank reference data to GPS event data in a given order. GPS and reference data can be downloaded directly from [movebank](https://www.movebank.org). Examples: [Lesser Black-backed Gulls](https://www.movebank.org/panel_embedded_movebank_webapp?gwt_fragment=page=studies,path=study985143423) and [Herring Gulls](https://www.movebank.org/panel_embedded_movebank_webapp?gwt_fragment=page=studies,path=study986040562)
#'
#' @param gps Movebank GPS data of class matrix, data.frame or data.table.  `gps` must contain at least the following columns:
#' - `individual-taxon-canonical-name`
#' - `tag-local-identifier`
#' - `individual-local-identifier`
#'
#' @param reference_data Movebank reference data of class matrix, data.frame or data.table. `reference_data` must contain at least the columns defined in `reference_cols`.
#' @param reference_cols Character. Vector with the names of the columns of `reference_data` to add to `gps`. `reference-cols` must contain at least the following columns:
#' - `animal-taxon`
#' - `tag-id`
#' - `animal-id`
#'
#' Default:
#' `c("animal-taxon", "tag-id","animal-id", "animal-comments",
#' "animal-life-stage", "animal-mass", "animal-sex", "deployment-comments")`
#'
#' @return a data.table
#'
#' @export
#'
#' @importFrom data.table data.table
#' @importFrom dplyr mutate tibble left_join rename one_of select
#' @importFrom assertthat assert_that
#' @examples
#' \dontrun{
#' }
append_metadata <- function(gps,
                            reference_data,
                            reference_cols = c("animal-taxon",
                                               "tag-id",
                                               "animal-id",
                                               "animal-comments",
                                               "animal-life-stage",
                                               "animal-mass",
                                               "animal-sex",
                                               "deployment-comments")) {

  # gps and reference_data are of the right class
  assert_that(
    any(c("matrix", "data.frame", "data.table") %in% class(gps)),
    msg = "`gps` must be of class data.table, data.frame or matrix."
  )
  assert_that(
    any(c("matrix", "data.frame", "data.table") %in% class(reference_data)),
    msg = "`reference_data` must be of class data.table, data.frame or matrix."
  )

  # colnames gps and reference_data
  cols_gps <- colnames(gps)
  cols_reference_data <- colnames(reference_data)

  # Define gps cols to have for join
  gps_cols_to_have <- c("individual-taxon-canonical-name",
                         "tag-local-identifier",
                         "individual-local-identifier")
  # Check that gps contains the columns declared above
  cols_not_present <-
    gps_cols_to_have[!gps_cols_to_have %in% cols_gps]
  assert_that(
    length(cols_not_present) == 0,
    msg = paste0(
      "Can't find column(s) `",
      paste0(cols_not_present, collapse = "`,`"), "` in `gps`.")
  )

  # cols from gps in output (all cols except those one used for join)
  cols_gps_in_output <- cols_gps[!cols_gps %in% gps_cols_to_have]

  # Check that reference_data contains all columns defined in reference_cols
  cols_not_present <-
    reference_cols[!reference_cols %in% cols_reference_data]
  assert_that(
    length(cols_not_present) == 0,
    msg = paste0(
      "Can't find column(s) `",
      paste0(cols_not_present, collapse = "`,`"), "` in `reference_data`.")
  )

  # reference cols to have for join
  reference_cols_to_have <- c("animal-taxon", "tag-id", "animal-id")
  cols_not_present <-
    reference_cols_to_have[!reference_cols_to_have %in% reference_cols]
  assert_that(
    length(cols_not_present) == 0,
    msg = paste0(
      "reference_data column(s) `",
      paste0(cols_not_present, collapse = "`,`"), "` must be selected.")
  )

  # convert gps and reference_data to data.table
  gps <- data.table(gps)
  reference_data <- data.table(reference_data)

  gps %>%
    # rename gps columns to join by
    rename("animal-taxon" = "individual-taxon-canonical-name",
           "tag-id" = "tag-local-identifier",
           "animal-id" = "individual-local-identifier") %>%
    # join reference data to gps data
    left_join(reference_data,
              by = c("animal-taxon",
                     "tag-id",
                     "animal-id")) %>%
    # order columns
    select(one_of(cols_gps_in_output), one_of(reference_cols))
}
