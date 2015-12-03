#' Profile UvA-BiTS ETL processing steps (Level 1)
#' @description Profile the different processing steps in the UvaBitsETL
#' package to find bottlenecks.
#' 
#' @param tracks_file CSV file containing tracking data used for profiling.
#' Don't use a file that is too big (because it will take some time), but don't
#' make it too small either (because that can introduces biases that are not
#' representative)
#' @param birds_file CSV file containing bird tracking metadata.
#' @return Data.Frame with two columns: "step" and "timing"
#' @export
#' @importFrom data.table melt
profiling_L1 <- function(tracks_file, birds_file) {
	reading_tracks_time <- system.time(tr_data <- load_tracks_file(tracks_file))
	reading_birds_time <- system.time(bird_data <- load_bird_file(birds_file))
	track_validation_time <- system.time(v_tr_data <- validate_tracks_data(tr_data))
	bird_validation_time <- system.time(v_bird_data <- validate_bird_data(bird_data))
	
	join_time <- system.time(joined_data <- join_tracks_and_metadata(v_tr_data, v_bird_data))
	delete_time <- system.time(subset <- delete_test_records(joined_data))
	setkey_time <- system.time(setkey(subset, device_info_serial, date_time))
	time_calc_time <- system.time(add_time_since_previous_fix(subset))
	dist_travld_time <- system.time(add_dist_travelled(subset))
	speed_time <- system.time(add_speed(subset))
	
	plt_data <- data.frame(
		tracks_reading=c(as.numeric(reading_tracks_time[3])),
		birds_reading=c(as.numeric(reading_birds_time[3])),
		track_validation=c(as.numeric(track_validation_time[3])),
		bird_validation=c(as.numeric(bird_validation_time[3])),
		join=c(as.numeric(join_time[3])),
		delete_test_records=c(as.numeric(delete_time[3])),
		set_keys=c(as.numeric(setkey_time[3])),
		time_previous_fix=c(as.numeric(time_calc_time[3])),
		distance_travelled=c(as.numeric(dist_travld_time[3])),
		speed=c(as.numeric(speed_time[3]))
	)
	
	d <- melt(plt_data,variable.name="step", value.name="timing")
	return(d)
}