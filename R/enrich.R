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
	bird_data[, colony_latitude:=latitude]
	bird_data[, colony_longitude:=longitude]
	bird_data[, latitude:=NULL]
	bird_data[, longitude:=NULL]
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
	return(data[date_time >= tracking_started_at])
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
#' add_time_since_previous_fix(tracking_data)
#' }
add_time_since_previous_fix <- function(datatable) {
	datatable[, time_diff:=difftime(date_time, shift(date_time), units="secs"),
						by=device_info_serial]
}


#' Add Distance travelled
#' @description will calculate the distance travelled since previous GPS fix
#' 
#' @param dt tracking data as data.table
#' @return nothing. Distance (in meters) is added in place
#' @export
#' @examples 
#' \dontrun{
#' add_dist_travelled(tracking_data)
#' }
#' @importFrom geosphere distCosine
add_dist_travelled <- function(dt) {
	dt[, tmp.select:=device_info_serial==shift(device_info_serial)]
	distances <- distCosine(
		  cbind(dt$longitude, dt$latitude),
		  cbind(c(1, shift(dt$longitude)[-1]),
						c(1, shift(dt$latitude)[-1])
			)
		)
	distances[!dt$tmp.select | is.na(dt$tmp.select)] <- NA
	dt[, distance:=distances]
	dt[, tmp.select:=NULL]
}

#' Add speed
#' @description calculates the average speed of the individual since the
#' previous GPS fix
#' 
#' @param dt tracking data as data.table. Should contain column `distance`
#' and column `time_diff`
#' @return nothing. Data is added in place
#' @export
#' @examples
#' \dontrun{
#' add_speed(tracking_data)
#' }
add_speed <- function(dt) {
	dt[, speed_km_h:=(distance/1000)/(as.numeric(time_diff)/3600)]
}

#' Add distance to colony
#' @description calculates the distance from the GPS position to the colony
#' 
#' @param dt tracking data as data.table. Should contains columns `latitude`,
#' `longitude`, `colony_latitude`, `colony_longitude`
#' @return nothing. Adds distance (in meters) in place
#' @export
#' @examples
#' \dontrun{
#' add_dist_to_colony(tracking_data)
#' }
#' @importFrom geosphere distCosine
add_dist_to_colony <- function(dt) {
	dt[, dist_to_colony:=distCosine(
		cbind(longitude, latitude),
		cbind(colony_longitude, colony_latitude)
	)]
}


#' Flag outliers
#' @description Flag records that are suspect to be erronous.
#' The following checks are made:
#'     - date_time < current date
#'     - altitude < 1000 km
#'     - speed < 120 km per hour
#'     - height_accuracy < 1000
#' If one of these fails, the record gets flagged.
#' 
#' @param dt tracking data as data.table.
#' @return nothing. Flagging happens in place
#' @export
#' @examples
#' \dontrun{
#' flag_outliers(tracking_data)
#' }
flag_outliers <- function(dt) {
	today <- now()
	dt[, outlier:=speed_km_h<0 |
		 	speed_km_h>120 |
		 	altitude>10000 |
		 	h_accuracy>1000 |
		 	date_time>today
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
	pts <- SpatialPoints(data.frame(x=dt$longitude, y=dt$latitude), proj4string=CRS("+init=epsg:4326"))
	conv <- spTransform(pts, CRSobj=CRS(proj4string(raster_data)))
	results <- extract(raster_data, conv)
	dt[, raster_value:=results]
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
#' @return Data table containing enriched data
#' @export
#' @examples 
#' \dontrun {
#' enrich_data(tracking_data, bird_data)
#' }
enrich_data <- function(tracking_data, bird_data, raster_data) {
	dt <- join_tracks_and_metadata(tracking_data, bird_data)
	dt <- delete_test_records(dt)
	setkey(dt, device_info_serial, date_time) # will sort on those columns
	add_time_since_previous_fix(dt)
	add_dist_travelled(dt)
	add_speed(dt)
	add_dist_to_colony(dt)
	flag_outliers(dt)
	raster_join(dt, raster_data)
	return(dt)
}