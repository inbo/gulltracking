#' Calculate sunrise and sunset
#' @description This function calculate the sunrise and sunset in UTC
#' for a given location and date. Function was slightly modified but inspired
#' from this post: http://www.r-bloggers.com/approximate-sunrise-and-sunset-times/
#'
#' @param dates vector containing POSIX dates
#' @param Lat Latitudes in WGS84
#' @param Long Longitudes in WGS84
#' @return named list with elements "sunrise" and "sunset"
#' @export
#' @importFrom maptools sunriset
#' @examples
#' {
#' suncalc.custom(ymd("2015-01-01"), Lat=50.821, Long=4.366)
#' }
suncalc.custom <- function(dates,Lat,Long){
	sunrise <- sunriset(matrix(c(Long, Lat), ncol = 2),
										 with_tz(dates, "UTC"),
										 direction = "sunrise",
										 POSIXct.out = TRUE)
	sunset <- sunriset(matrix(c(Long, Lat), ncol = 2),
										 with_tz(dates, "UTC"),
										 direction = "sunset",
										 POSIXct.out = TRUE)
	return(list("sunrise" = with_tz(sunrise$time, "UTC"),
							"sunset" = with_tz(sunset$time, "UTC")))
}

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
    # pre-check for device_info_serial -> are devices in logs present in the metadata
    log_devices <- unique(tracking_data$device_info_serial)
    known_devices <- unique(bird_data$device_info_serial)
	if (sum(log_devices %in% known_devices) < length(log_devices)) {
		msg <- paste(c("Error while joining tracking data and bird metadata.",
				"... tracking data found that could not be matched with bird metadata.",
				"The following devices are unknown:",
				paste(log_devices[!log_devices %in% known_devices],
				      collapse = ", ")),
				collapse = "\n")
		stop(msg)
	    }

    # Prepare coordinates information
	bird_data[, colony_latitude := latitude]
	bird_data[, colony_longitude := longitude]
	bird_data[, latitude := NULL]
	bird_data[, longitude := NULL]

	#add dummy column with end date equal to the start date for tracking log
	tracking_data[, dummy_date_time := date_time]

	#add dummy column to incorporate end dates to end bird log
	bird_data[, dummy_tracking_ended_at := tracking_ended_at]
	# replace NaN values to 'NOW' values in dummy column...
	bird_data[is.na(bird_data$dummy_tracking_ended_at)]$dummy_tracking_ended_at <- Sys.time()

	# define merging keys: device_id + date containing columns
	setkeyv(tracking_data, c("device_info_serial", "date_time",
	                         "dummy_date_time"))
	setkeyv(bird_data, c("device_info_serial", "tracking_started_at",
	                     "dummy_tracking_ended_at"))
	# join by the overlap of the time sequence
	joined <- foverlaps(tracking_data, bird_data,
	                    c("device_info_serial", "date_time", "dummy_date_time"),
	                    c("device_info_serial", "tracking_started_at",
	                      "dummy_tracking_ended_at"),
	                    type = "within",
	                    mult = "all")
	# remove dummy date columns
	joined[, dummy_date_time := NULL]
	joined[, dummy_tracking_ended_at := NULL]

	# remaining none-matched records are device-logs outside the provided time
	# range and can be excluded
	joined <- joined[!is.na(joined$ring_code), ]
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
	return(data[date_time >= tracking_started_at])
}

#' Add year, month, hour columns
#' @description Add the year, month and hour of the GPS fix in separate columns
#'
#' @param dt Tracking data as a data.table.
#' @return Nothing, columns are added in place
#' @export
#' @examples
#' \dontrun{
#' add_year_month_hour(tracks_data)
#' }
add_year_month_hour <- function(data) {
	data[, calc_year := year(date_time)]
	data[, calc_month := month(date_time)]
	data[, calc_hour := hour(date_time)]
}

#' Add time since previous fix
#' @description Calculates the time (in seconds) since the birds last fix.
#' Data is first ordered by individual and date_time. Next, for each individual
#' the time difference between a fix and its previous fix is calculated.
#'
#' @param datatable A data.table with tracking data. Should at least include
#' a column `device_info_serial` and `date_time`
#' @return a new datatable with the time difference column (`calc_time_diff`) added to it.
#' @export
#' @examples
#' \dontrun{
#' add_time_since_previous_fix(tracking_data)
#' }
add_time_since_previous_fix <- function(datatable) {
	datatable[, calc_time_diff := difftime(date_time,
	                                       data.table::shift(date_time),
	                                       units = "secs"),
						by = device_info_serial]
}


#' Add Distance travelled
#' @description will calculate the distance travelled since previous GPS fix
#'
#' @param dt tracking data as data.table
#' @return nothing. Distance (in meters) is added in place (`calc_distance_diff`)
#' @export
#' @examples
#' \dontrun{
#' add_dist_travelled(tracking_data)
#' }
#' @importFrom geosphere distCosine
add_dist_travelled <- function(dt) {
	dt[, tmp.select := device_info_serial == data.table::shift(device_info_serial)]
	distances <- distCosine(
		  cbind(dt$longitude, dt$latitude),
		  cbind(c(1, data.table::shift(dt$longitude)[-1]),
				c(1, data.table::shift(dt$latitude)[-1])
			)
		)
	distances[!dt$tmp.select | is.na(dt$tmp.select)] <- NA
	dt[, calc_distance_diff := distances]
	dt[, tmp.select := NULL]
}

#' Add speed
#' @description calculates the average 2 dimensional speed of the individual since the
#' previous GPS fix
#'
#' @param dt tracking data as data.table. Should contain column `calc_distance_diff`
#' and column `calc_time_diff`
#' @return nothing. Data is added in place as column `calc_speed_2d`
#' @export
#' @examples
#' \dontrun{
#' add_speed(tracking_data)
#' }
add_speed <- function(dt) {
	dt[, calc_speed_2d := (calc_distance_diff)/(as.numeric(calc_time_diff))]
}

#' Add distance to colony
#' @description calculates the distance from the GPS position to the colony
#'
#' @param dt tracking data as data.table. Should contain columns `latitude`,
#' `longitude`, `colony_latitude`, `colony_longitude`
#' @return nothing. Adds distance (in meters) in place as columns `calc_distance_to_colony`
#' @export
#' @examples
#' \dontrun{
#' add_dist_to_colony(tracking_data)
#' }
#' @importFrom geosphere distCosine
add_dist_to_colony <- function(dt) {
	dt[, calc_distance_to_colony := distCosine(
		cbind(longitude, latitude),
		cbind(colony_longitude, colony_latitude)
	)]
}

#' Presence of sunlight
#' @description calculate the presence of sunlight for every GPS fix. This is done
#' using the `suncalc` function of the package `RAtmosphere`. If the date_time of
#' the GPS fix is after sunrise and before sunset, presence of sunlight is 1. Otherwise
#' it is set to 0.
#'
#' @param dt tracking data as data.table. Should contain columns `latitude`, `longitude`
#' and `date_time`.
#' @return nothing. Column `calc_sunlight` is added in place. This is a logical vector, indicating
#' wether sunlight was present at time and location of the GPS fix.
#' @export
#' @examples
#' \dontrun{
#' add_sunlight(tracking_data)
#' }
add_sunlight <- function(dt) {
	results <- suncalc.custom(dt$date_time, dt$latitude, dt$longitude)
	dt[, calc_sunlight := date_time > results$sunrise & date_time < results$sunset]
	#print(dt)
}


#' Flag outliers on raw data
#' @description Flag records that are suspect to be erronous on the raw records.
#' The following checks are made:
#'     - date_time < current date
#'     - altitude < 1000 km
#'     - height_accuracy < 1000
#' If one of these fails, the record gets flagged.
#'
#' @param dt tracking data as data.table.
#' @return nothing. Flagging happens in place. New columns is called `calc_outlier`
#' and contains logical values.
#' @export
#' @examples
#' \dontrun{
#' flag_outliers_raw(tracking_data)
#' }
flag_outliers_raw <- function(dt) {
	today <- now()
	dt[, calc_outlier := altitude > 10000 |
		 	h_accuracy > 1000 |
		 	date_time > today
		 ]
}

#' Flag outliers on derived data
#' @description Flag records that are suspect to be erronous on the raw records.
#' The following checks are made:
#'     - speed > 33.3333 meters per second (= 120 km per hour)
#'     - speed < 0  meters per second
#' If one of these fails, the record gets flagged.
#'
#' @param dt tracking data as data.table.
#' @return nothing. Flagging happens in place. New columns is called `calc_outlier`
#' and contains logical values.
#' @export
#' @examples
#' \dontrun{
#' flag_outliers_derived(tracking_data)
#' }
flag_outliers_derived <- function(dt) {
    today <- now()
    dt[, calc_outlier := calc_speed_2d < 0 |
           calc_speed_2d > 33.33333
       ]
}

#' Raster join
#' @description Join tracks with raster data
#'
#' @param dt Data table with tracking data. Geospatial points are created based on the
#' latitude and longitude field and WGS84 datum is expected.
#' @param raster_data A raster data object. See the package "raster".
#' @return nothing. The value of the raster for every point is added to the data
#' table in place
#' @export
raster_join <- function(dt, raster_data) {
	pts <- SpatialPoints(data.frame(x = dt$longitude, y = dt$latitude),
	                     proj4string = CRS("+init=epsg:4326"))
	conv <- spTransform(pts, CRSobj = CRS(proj4string(raster_data)))
	results <- extract(raster_data, conv)
	dt[, calc_raster_value := results]
}

#' Join raster value with legend
#' @description Join the raster value with its legend
#'
#' @param dt Data table with tracking data. Expected to contain a column `calc_raster_value`
#' which is the result of joining this table with a raster layer.
#' @param legend a data table with the legend of the raster layer. It should contain a column
#' `id` and a column `value`. The `id` column should contain the values that are used in the
#' raster layer, while the `value` column contains the labels for these values.
#' @return New data table with an additional column "calc_raster_legend"
#' @export
#' @examples
#' \dontrun {
#' join_raster_value_with_legend(datatable, legendcsv)
#' }
join_raster_value_with_legend <- function(dt, legend) {
	setkey(dt, calc_raster_value)
	colnames(legend) <- c("calc_raster_value", "calc_raster_legend")
	setkey(legend, calc_raster_value)
	newdt <- merge(dt, legend, all.x = TRUE)
	return(newdt)
}

#' Enrich data
#' @description Enrich the bird tracking data by precalculating attributes
#' and joining data with other sources. See the package vignette for a complete
#' description of the procedure.
#'
#' @param tracking_data Data table obtained by using the validate_tracks_data
#' function
#' @param bird_data Bird metadata obtained by using the validate_bird_data
#' function
#' @param corine_raster_data Filename containing corine raster data to be joined
#' with bird tracking data.
#' @return Data table containing enriched data
#' @export
#' @examples
#' \dontrun {
#' enrich_data(tracking_data, bird_data)
#' }
enrich_data <- function(tracking_data, bird_data, corine_raster_data, corine_legend) {
	dt <- join_tracks_and_metadata(tracking_data, bird_data)
	dt <- delete_test_records(dt) # actually redundant due to date-based join
	setkey(dt, device_info_serial, date_time) # will sort on those columns
	flag_outliers_raw(dt)
	add_year_month_hour(dt)
	add_time_since_previous_fix(dt)
	add_dist_travelled(dt)
	add_speed(dt)
	add_dist_to_colony(dt)
	add_sunlight(dt)
	flag_outliers_derived(dt)
	raster_join(dt, corine_raster_data)
	dt <- join_raster_value_with_legend(dt, corine_legend)
	setkey(dt, device_info_serial, date_time) # will sort on those columns
	setnames(dt, "calc_raster_value", "calc_corine_value")
	setnames(dt, "calc_raster_legend", "calc_corine_legend")
	return(dt)
}