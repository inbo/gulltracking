#' Append Movebank reference data to Movebank GPS data
#'
#' This function joins Movebank GPS data with Movebank reference (meta)data, on
#' the shared columns: - `gps.individual-taxon-canonical-name` =
#' `ref_data.animal-taxon` - `gps.tag-local-identifier` = `ref_data.tag-id` -
#' `gps.individual-local-identifier` = `ref_data.animal-id`
#'
#' GPS and reference data can be downloaded from
#' [Movebank](https://www.movebank.org).
#'
#' @param gps data.frame, data.table or matrix. Movebank GPS data with at least
#'   the columns used for the join: `individual-taxon-canonical-name`,
#'   `tag-local-identifier` and `individual-local-identifier`.
#' @param ref_data data.frame, data.table or matrix. Movebank reference data
#'   with at least the columns defined in `ref_cols`.
#' @param ref_cols Character. Vector with the column names of `ref_data` to be
#'   added to `gps`. It must at least contain the columns used for the join:
#'   `animal-taxon`, `tag-id` and `animal-id`. Columns with same name of
#'   columns of `gps` are dropped with a warning.
#'
#'   Default: `c("animal-taxon", "tag-id","animal-id", "animal-comments",
#'   "animal-life-stage", "animal-mass", "animal-sex", "deployment-comments")`
#'
#' @return A data.table with the GPS data (all columns except those used in the
#'   join) appended with the reference data (all columns defined in
#'   `ref_cols`).
#'
#' @export
#'
#' @importFrom data.table data.table
#' @importFrom dplyr %>% mutate tibble left_join rename one_of select
#' @importFrom assertthat assert_that
#' @examples
#' # Default use
#' append_metadata(lbbg_gps, lbbg_ref_data)
#'
#' # Only include specific reference data columns
#' append_metadata(
#'   lbbg_gps,
#'   lbbg_ref_data,
#'   ref_cols = c("animal-taxon", "tag-id", "animal-id",
#'                      "animal-comments", "animal-life-stage")
#' )
append_metadata <- function(gps,
                            ref_data,
                            ref_cols = c("animal-taxon",
                                         "tag-id",
                                         "animal-id",
                                         "animal-comments",
                                         "animal-life-stage",
                                         "animal-mass",
                                         "animal-sex",
                                         "deployment-comments")) {

  # gps and ref_data are of the right class
  assert_that(
    any(c("matrix", "data.frame", "data.table") %in% class(gps)),
    msg = "`gps` must be of class data.table, data.frame or matrix."
  )
  assert_that(
    any(c("matrix", "data.frame", "data.table") %in% class(ref_data)),
    msg = "`ref_data` must be of class data.table, data.frame or matrix."
  )

  # colnames gps and ref_data
  cols_gps <- colnames(gps)
  cols_ref_data <- colnames(ref_data)

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

  # Check that ref_data contains all columns defined in ref_cols
  cols_not_present <-
    ref_cols[!ref_cols %in% cols_ref_data]
  assert_that(
    length(cols_not_present) == 0,
    msg = paste0(
      "Can't find column(s) `",
      paste0(cols_not_present, collapse = "`,`"), "` in `ref_data`.")
  )

  # reference cols to have for join
  ref_cols_to_have <- c("animal-taxon", "tag-id", "animal-id")
  cols_not_present <-
    ref_cols_to_have[!ref_cols_to_have %in% ref_cols]
  assert_that(
    length(cols_not_present) == 0,
    msg = paste0(
      "ref_cols should (also) contain `",
      paste0(cols_not_present, collapse = "`,`"), "` to join data.")
  )

  # cols of reference with same name in gps will be removed before join
  same_name_cols <- ref_cols[ref_cols %in% cols_gps]
  if (length(same_name_cols) > 0) {
    warning(paste0(
      "The following columns of ref_data will be dropped as they are present in gps as well: `",
      paste0(same_name_cols, collapse = "`,`"), "`."))
  }

  # convert gps and ref_data to data.table
  gps <- data.table(gps)
  ref_data <- data.table(ref_data)

  gps %>%
    # rename gps columns to join by
    rename("animal-taxon" = "individual-taxon-canonical-name",
           "tag-id" = "tag-local-identifier",
           "animal-id" = "individual-local-identifier") %>%
    # join reference data to gps data
    left_join(ref_data,
              by = c("animal-taxon",
                     "tag-id",
                     "animal-id")) %>%
    # order columns
    select(one_of(cols_gps_in_output), one_of(ref_cols))
}
