#' Check numeric values
#' @description Check whether values in given column can be converted to
#' numeric types. If not, this function will call stop().
#' 
#' @param colname Name of the column to be tested
#' @param col Column containing values to be tested
#' @return col if all values can be converted to numeric. Otherwise error.
#' @export
#' @importFrom taRifx destring
#' @examples
#' check_numeric_values("testcol", c(1, 2, 3))
check_numeric_values <- function(colname, col) {
	testcol <- col[!is.na(col)]# drop NA's
	if (length(testcol[is.na(destring(testcol))]) > 0) {
		stop(paste("Non numeric values found for column ", colname))
	}
	return(col)
}


#' Load file containing bird tracking data
#' @description Load a file containing bird tracking data. This file can
#' be obtained by requesting a dump from the UvA-BiTS virtual lab.
#'
#' @param filename The file containing bird tracking data in csv
#' format. (','-delimited, header included)
#' @return A data table (not a data frame!) containing the tracking data
#' @export
#' @examples
#' \dontrun{
#' load_tracks_file(inputFile)
#' }
#' @import data.table
load_tracks_file <- function(filename) {
	data <- fread(filename, dec=".", header=TRUE, sep=",", na.strings=c("\\N"))
}


#' Validate tracking data
#' @description Validate the data coming either from a csv file or
#' from the UvA-BiTS virtual lab directly.
#' 
#' @param tracks_data The tracking data as a data table
#' @return validated tracking data as a data table if no errors are found.
#' @export
#' @examples
#' \dontrun{
#' validate_tracks_data(tracking_data)
#' }
validate_tracks_data <- function(tracks_data)	{
	issues <- c()
	# set data types for non-character columns
	nas_in_date_time <- sum(is.na(tracks_data$date_time))
	tracks_data[, date_time:=lubridate::ymd_hms(date_time)]
	if (sum(is.na(tracks_data$date_time)) > nas_in_date_time) {
		issues <- c(issues, "unparsable values found in column date_time")
	}
	
	# check whether columns can be converted to numeric
	numeric_cols <- c("device_info_serial", "latitude", "longitude", "altitude", "pressure",
										"temperature", "satellites_used", "gps_fixtime", "positiondop",
										"h_accuracy", "v_accuracy", "x_speed", "y_speed", "z_speed",
										"speed_accurracy", "speed_3d", "speed_2d", "direction", "altitude_agl")
	lapply(numeric_cols, function(x) {
		tryCatch({
			check_numeric_values(x, tracks_data[[x]])
		}, error=function(e) {
				issues <<- append(issues, paste("non numeric values found in column ", x))
		}
		)
	})
	
	# convert to numeric columns
	tracks_data[, device_info_serial:=as.numeric(device_info_serial)]
	tracks_data[, latitude:=as.numeric(latitude)]
	tracks_data[, longitude:=as.numeric(longitude)]
	tracks_data[, altitude:=as.numeric(altitude)]
	tracks_data[, pressure:=as.numeric(pressure)]
	tracks_data[, temperature:=as.numeric(temperature)]
	tracks_data[, satellites_used:=as.numeric(satellites_used)]
	tracks_data[, gps_fixtime:=as.numeric(gps_fixtime)]
	tracks_data[, positiondop:=as.numeric(positiondop)]
	tracks_data[, h_accuracy:=as.numeric(h_accuracy)]
	tracks_data[, v_accuracy:=as.numeric(v_accuracy)]
	tracks_data[, x_speed:=as.numeric(x_speed)]
	tracks_data[, y_speed:=as.numeric(y_speed)]
	tracks_data[, z_speed:=as.numeric(z_speed)]
	tracks_data[, speed_accuracy:=as.numeric(speed_accuracy)]
	tracks_data[, speed_3d:=as.numeric(speed_3d)]
	tracks_data[, speed_2d:=as.numeric(speed_2d)]
	tracks_data[, direction:=as.numeric(direction)]
	tracks_data[, altitude_agl:=as.numeric(altitude_agl)]
	
	# drop unused columns
	# ... location: this is a binary blob from a geometric data type. Cannot be used.
	tracks_data[, location:=NULL]
	
	if (length(issues) > 0) {
		print(paste(issues, sep="\n"))
		stop("Validation failed")
	}
	return(tracks_data)
}

#' Load bird metadata file
#' @description Load a file containing bird metadata. This file is managed at the
#' INBO using CartoDB Create a csv export of that file, and make sure it is
#' "," delimited.
#' 
#' @param filename The name of the file containing bird metadata
#' @return A data table (not a data frame!) containing the bird metadata
#' @export
#' @examples 
#' \dontrun{
#' load_bird_file(inputFile)
#' }
#' @import data.table
load_bird_file <- function(filename) {
	data <- fread(filename, dec=".", header=TRUE, sep=",")
}


#' Validate bird data
#' @description Validate the bird metadata
#' 
#' @param bird_data The bird metadata as a data table
#' @return validated bird metadata as a data table if no errors are found.
#' @export
validate_bird_data <- function(bird_data) {
	issues <- c()
	# set date time data types
	nas_in_tr_start_time <- sum(is.na(bird_data$tracking_start_date_time))
	bird_data[, tracking_start_date_time:=lubridate::ymd_hms(tracking_start_date_time)]
	if (sum(is.na(bird_data$tracking_start_date_time)) > nas_in_tr_start_time) {
		issues <- c(issues, "unparsable values found in column tracking_start_date_time")
	}
	nas_in_tr_end_time <- sum(is.na(bird_data$tracking_end_date_time))
	bird_data[, tracking_end_date_time:=lubridate::ymd_hms(tracking_end_date_time)]
	if (sum(is.na(bird_data$tracking_end_date_time)) > nas_in_tr_end_time) {
		issues <- c(issues, "unparsable values found in column tracking_end_date_time")
	}
	nas_in_created_at <- sum(is.na(bird_data$created_at))
	bird_data[, created_at:=lubridate::ymd_hms(created_at)]
	if (sum(is.na(bird_data$created_at)) > nas_in_created_at) {
		issues <- c(issues, "unparsable values found in column created_at")
	}
	nas_in_updated_at <- sum(is.na(bird_data$updated_at))
	bird_data[, updated_at:=lubridate::ymd_hms(updated_at)]
	if (sum(is.na(bird_data$updated_at)) > nas_in_updated_at) {
		issues <- c(issues, "unparsable values found in column created_at")
	}
	
	# convert enumeration columns to factors
	# note that the allowed choices are saved as package data in 'data/'
	bird_data[, sex:=as.factor(sex)]
	if (!all(levels(bird_data$sex) %in% sex_choices)) {
			issues <- append(issues, paste("value found in column sex that does not match one of: ",
									paste(sex_choices, collapse=", ")
									)
						)
	}
	bird_data[, species:=as.factor(species)]
	if (!all(levels(bird_data$species) %in% species_choices)) {
			issues <- append(issues, paste("value found in column species that does not match one of: ",
									paste(species_choices, collapse=", ")
						  	)
			)
	}
	
	# check whether columns can be converted to numeric
	numeric_cols <- c("device_info_serial", "weight_in_g", "colony_latitude",
										"colony_longitude")
	lapply(numeric_cols, function(x) {
		tryCatch({
			check_numeric_values(x, bird_data[[x]])
		}, error=function(e) {
			issues <<- append(issues, paste("non numeric values found in column ", x))
		}
		)
	})
	
	# convert to numeric columns
	bird_data[, device_info_serial:=as.numeric(device_info_serial)]
	bird_data[, weight_in_g:=as.numeric(weight_in_g)]
	bird_data[, colony_latitude:=as.numeric(colony_latitude)]
	bird_data[, colony_longitude:=as.numeric(colony_longitude)]
	
	# drop unused columns
	# ... cartodb_id: this id has no use outside of CartoDB.
	# ... the_geom: this is a binary blob from a geometric data type. Cannot be used.
	bird_data[, cartodb_id:=NULL]
	bird_data[, the_geom:=NULL]
	
	if (length(issues) > 0) {
		print(paste(issues, sep="\n"))
		stop("Validation failed")
	}
	return(bird_data)
}