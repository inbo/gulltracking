#' data2table
#' @description Load all the enriched data to a RDBMS table 
#' 
#' @param dbConnection a DBI database connection
#' @param data A data table containing all data. See the vignette to see the required
#' columns and how they are written to the database.
#' @return Nothing
#' @examples
#' \dontrun{data2table(dbConnection, datatable)}
#' @export
#' @importFrom DBI dbWriteTable
data2table <- function(dbConnection, data) {
	# map column names. This way, we are sure what column from the data is written to which
	# column in the database
	df <- data.frame(
		id=rownames(data),
		project_leader=data$project_leader,
		device_info_serial=data$device_info_serial,
		bird_name=data$bird_name,
		ring_code=data$ring_code,
		colour_ring_code=data$colour_ring_code,
		species_code=data$species_code,
		scientific_name=data$scientific_name,
		catch_weight=data$catch_weight,
		sex=data$sex,
		catch_location=data$catch_location,
		tracking_started_at=data$tracking_started_at,
		tracking_ended_at=data$tracking_ended_at,
		colony_latitude=data$colony_latitude,
		colony_longitude=data$colony_longitude,
		is_active=data$is_active, # might need to map for boolean type
		date_time=data$date_time,
		latitude=data$latitude,
		longitude=data$longitude,
		altitude=data$altitude,
		pressure=data$pressure,
		temperature=data$temperature,
		satellites_used=data$satellites_used,
		gps_fixtime=data$gps_fixtime,
		positiondop=data$positiondop,
		h_accuracy=data$h_accuracy,
		v_accuracy=data$v_accuracy,
		x_speed=data$x_speed,
		y_speed=data$y_speed,
		z_speed=data$z_speed,
		speed_accuracy=data$speed_accuracy,
		userflag=data$userflag,
		speed_3d=data$speed_3d,
		speed_2d=data$speed_2d,
		direction=data$direction,
		altitude_agl=data$altitude_agl,
		calc_year=data$calc_year,
		calc_month=data$calc_month,
		calc_hour=data$calc_hour,
		calc_time_diff=data$calc_time_diff,
		calc_distance_diff=data$calc_distance_diff,
		calc_speed_2d=data$calc_speed_2d,
		calc_distance_to_colony=data$calc_distance_to_colony,
		calc_sunlight=data$calc_sunlight,
		calc_outlier=data$calc_outlier, # might need to map for boolean type
		calc_corine_category=data$calc_raster_value
	)
	dbWriteTable(dbConnection, "tracking_warehouse", df, row.names=FALSE)
}
