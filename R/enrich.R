#' Join tracks and metadata
#' @description Join tracking data with bird metadata. If tracking records
#' are found that cannot be matched with bird metadata, the function stops.
#' 
#' @param tracking_data Tracking data as data table
#' @param bird_data Bird metadata as data table
#' @return data table containing the joined input data tables
#' @export
#' @examples 
#' \dontrun{
#' join_tracks_and_metadata(tracks_validated, birds_validated)
#' }
join_tracks_and_metadata <- function(tracking_data, bird_data) {
	setkey(tracking_data, device_info_serial)
	setkey(bird_data, device_info_serial)
	joined <- bird_data[tracking_data, nomatch=0]
	if (length(joined$device_info_serial) != length(tracking_data$device_info_serial)) {
		msg <- paste(c("Error while joining tracking data and bird metadata.",
				"... tracking data found that could not be matched with bird metadata."),
				sep="\n")
		stop(msg)
	}
	return(joined)
}

#' Delete test records
#' @description Remove data that was recorded by a device before it was
#' mounted on the bird
#'
#' @param data Tracking data as a data table. Data should already be joined
#' using the join_tracks_and_metadata function as both the date_time and the
#' tracking_start_date_time column are needed.
#' @return New data table without the test records
#' @export
#' @examples 
#' \dontrun{
#' delete_test_records(joined_data)
#' }
delete_test_records <- function(data) {
	return(data[date_time >= tracking_start_date_time])
}

#' Add time since previous fix
#' @description Calculates the time (in seconds) since the birds last fix.
#' Data is first ordered by individual and date_time. Next, for each individual
#' the time difference between a fix and its previous fix is calculated.
#' 
#' @param datatable A data.table with tracking data. Should at least include
#' a column `device_info_serial` and `date_time`
#' @return a new datatable with the time difference column added to it.
#' @export
#' @examples 
#' \dontrun{
#' add_time_since_previous_fix
#' }
add_time_since_previous_fix <- function(datatable) {
	datatable <- datatable[order(device_info_serial, date_time)]
	diffs <- datatable[, diff(date_time), by=device_info_serial]
	diff_col <- diffs[, c(NA, V1), by=device_info_serial]
	datatable <- datatable[, time_diff:=diff_col$V1]
	return(datatable)
}

# add_dist_travelled()
# add_speed()
# add_dist_to_colony()
# flag_outliers()
# link_with_corine()

#' Enrich data
#' @description Enrich the bird tracking data by precalculating attributes
#' and joining data with other sources. See the package vignette for a complete
#' description of the procedure.
#' 
#' @param tracking_data Data table obtained by using the validate_tracks_data
#' function
#' @param bird_data Bird metadata obtained by using the validate_bird_data
#' function
#' @return Data table containing enriched data
#' @export
#' @examples 
#' \dontrun {
#' enrich_data(tracking_data, bird_data)
#' }
enrich_data <- function(tracking_data, bird_data) {
	dt <- join_tracks_and_metadata(tracking_data, bird_data)
	dt <- delete_test_records(dt)
	dt <- add_time_since_previous_fix(dt)
	dt <- add_dist_travelled(dt)
	# dt <- add_speed(dt)
}