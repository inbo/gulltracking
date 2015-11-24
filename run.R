library(UvaBitsWarehouse)



run_and_profile <- function(tracks_file, birds_file, corine_filename) {
	timing_load_birds <- system.time(birds <- load_bird_file(birds_file))[3]
	timing_validate_birds <- system.time(birds <- validate_bird_data(birds))[3]
	timing_load_tracks <- system.time(tracks <- load_tracks_file(tracks_file))[3]
	nr_track_records <- length(tracks$device_info_serial)
	timing_validate_tracks <- system.time(tracks <- validate_tracks_data(tracks))[3]
	timing_read_raster <- system.time(corine <- read_raster_data(corine_filename, data.CRS="+init=epsg:3035"))[3]
	timing_join <- system.time(dt <- join_tracks_and_metadata(tracks, birds))[3]
	timing_delete_test_records <- system.time(dt <- delete_test_records(dt))[3]
	timing_setkeys <- system.time(setkey(dt, device_info_serial, date_time))[3]
	timing_add_time <- system.time(add_time_since_previous_fix(dt))[3]
	timing_add_dist_travelled <- system.time(add_dist_travelled(dt))[3]
	timing_add_speed <- system.time(add_speed(dt))[3]
	timing_dist_to_colony <- system.time(add_dist_to_colony(dt))[3]
	timing_flag_outliers <- system.time(flag_outliers(dt))[3]
	timing_raster_join <- system.time(raster_join(dt, corine))[3]
	
	
	df <- data.frame(
	timing=c(
		  nr_track_records, timing_load_birds, timing_validate_birds,
		  timing_load_tracks, timing_validate_tracks, timing_read_raster,
		  timing_join, timing_delete_test_records, timing_setkeys,
		  timing_add_time, timing_add_dist_travelled, timing_add_speed,
		  timing_dist_to_colony, timing_flag_outliers, timing_raster_join
	)
	)
	return(df)
}

f <- function(x) {
	birds_filename <- "/Users/bart_aelterman/Projects/LW-006-Zendernetwerk/exploring/data/bird_tracking_devices.csv"
	tracks_filename <- paste("/Users/bart_aelterman/Projects/LW-006-Zendernetwerk/exploring/bigFiles/", x, sep="")
	corine_filename <- "/Users/bart_aelterman/Projects/LW-006-Zendernetwerk/ext_data/g100_06.tif"
	d <- run_and_profile(tracks_filename, birds_filename, corine_filename)
	return(d)
}

run_and_profile_testfiles <- function() {
	testfiles <- c("dump_10k.csv", "dump_50k.csv", "dump_100k.csv", "dump_500k.csv")
	results <- sapply(testfiles, f)
	result_df <- data.frame(a=results$dump_10k.csv,
													b=results$dump_50k.csv,
													c=results$dump_100k.csv,
													d=results$dump_500k.csv)
	colnames(result_df) <- c(10, 50, 100, 500)
	
	result_df$step <- c(
		"nr_track_records", "load_birds", "validate_birds",
		"load_tracks", "validate_tracks", "read_raster",
		"join_birds_tracks", "delete_test_records", "setkeys",
		"add_time", "add_dist_travelled", "add_speed",
		"add_dist_to_colony", "flag_outliers", "raster_join"
	)
	plot.data <- melt(result_df[result_df$step != "nr_track_records",],
										id=c("step"),
										variable.name="input.lines.x1000",
										value.name="time")
	return(plot.data)
	# p <- ggplot(plot.data, aes(x=step, y=time, colour=input.lines.x1000))
	# p + geom_point() + theme(axis.text.x = element_text(angle=-90))
}

run_all <- function() {
	birds_filename <- "/Users/bart_aelterman/Projects/LW-006-Zendernetwerk/exploring/data/bird_tracking_devices.csv"
	tracks_filename <- "/Users/bart_aelterman/Projects/LW-006-Zendernetwerk/exploring/bigFiles/dump.csv"
	corine_filename <- "/Users/bart_aelterman/Projects/LW-006-Zendernetwerk/ext_data/g100_06.tif"
	birds <- load_bird_file(birds_filename)
	birds <- validate_bird_data(birds)
	tracks <- load_tracks_file(tracks_filename)
	tracks <- validate_tracks_data(tracks)
	corine <- read_raster_data(corine_filename)
	proj4string(corine) <- CRS("+init=epsg:3035")
	dt <- enrich_data(tracks, birds, corine)
	return(dt)
}