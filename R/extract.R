#' Check numeric values
#' @description Check whether values in given column can be converted to
#' numeric types. If not, this function will call stop().
#' 
#' @param colname Name of the column to be tested
#' @param col Column containing values to be tested
#' @return col if all values can be converted to numeric. Otherwise error.
#' @export
#' @examples
#' check_numeric_values("testcol", c(1, 2, 3))
check_numeric_values <- function(colname, col) {
	testcol <- col[!is.na(col)]# drop NA's
	if (length(testcol[is.na(as.numeric(testcol))]) > 0) {
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
	tracks_data[, date_time:=lubridate::fast_strptime(date_time, "%Y-%m-%d %H:%M:%OS", lt=FALSE)]
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
#' INBO on Google Drive. Create a csv export of that file, and make sure it is
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
	data <- fread(filename, dec=".", header=TRUE, sep=",", na.strings=c(""))
}


#' Validate bird data
#' @description Validate the bird metadata
#' 
#' @param bird_data The bird metadata as a data table
#' @return validated bird metadata as a data table if no errors are found.
#' @export
validate_bird_data <- function(bird_data) {
	issues <- c()
	# set date time data types. Note that the timezone format string is Ou, which only matches the "Z"
	# string, indicating "Zulu" (UTC) time zone. "+00" will not match.
	nas_in_tr_start_time <- sum(is.na(bird_data$tracking_started_at))
	bird_data[, tracking_started_at:=lubridate::fast_strptime(as.character(tracking_started_at),
																														"%Y-%m-%dT%H:%M:%OS%Ou", lt=FALSE)]
	if (sum(is.na(bird_data$tracking_started_at)) > nas_in_tr_start_time) {
		issues <- c(issues, "unparsable values found in column tracking_started_at")
	}
	nas_in_tr_end_time <- sum(is.na(bird_data$tracking_ended_at))
	bird_data[, tracking_ended_at:=lubridate::fast_strptime(as.character(tracking_ended_at),
																													"%Y-%m-%dT%H:%M:%OS%Ou", lt=FALSE)]
	if (sum(is.na(bird_data$tracking_ended_at)) > nas_in_tr_end_time) {
		issues <- c(issues, "unparsable values found in column tracking_ended_at")
	}
	
	# convert logical columns to Logical
	tmp_is_active <- as.factor(bird_data$is_active)
	tryCatch({
		levels(tmp_is_active) <- c("TRUE", "FALSE")	
	}, error=function(e) {
		issues <<- append(issues, paste("could not parse boolean values from column is_active"))
	})
	bird_data[, is_active:=as.logical(tmp_is_active)]
	
	# convert enumeration columns to factors
	# note that the allowed choices are saved as package data in 'data/'
	bird_data[, sex:=as.factor(sex)]
	if (!all(levels(bird_data$sex) %in% sex_choices)) {
			issues <- append(issues, paste("value found in column sex that does not match one of: ",
									paste(sex_choices, collapse=", ")
									)
						)
	}
	bird_data[, scientific_name:=as.factor(scientific_name)]
	if (!all(levels(bird_data$scientific_name) %in% species_choices)) {
			issues <- append(issues, paste("value found in column scientific_name that does not match one of: ",
									paste(species_choices, collapse=", ")
						  	)
			)
	}
	
	# check whether columns can be converted to numeric
	numeric_cols <- c("device_info_serial", "catch_weight", "latitude", "longitude")
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
	bird_data[, catch_weight:=as.numeric(catch_weight)]
	bird_data[, latitude:=as.numeric(latitude)]
	bird_data[, longitude:=as.numeric(longitude)]
	
	if (length(issues) > 0) {
		print(paste(issues, sep="\n"))
		stop("Validation failed")
	}
	return(bird_data)
}


#' Read raster data
#' @description Read raster data using the raster package. By default
#' this function will set the CRS of this data to EPSG4326 (WGS 84).
#' Use the data.CRS parameter to override this. 
#' 
#' @param filename Name of the file containing raster data
#' @param data.CRS Coordinate Reference System of the data
#' @return raster data as RasterLayer class
#' @export
#' @examples
#' \dontrun{
#' read_raster_data("raster_file.tiff", data.CRS="+init=epsg:2056")
#' }
#' @import raster
read_raster_data <- function(filename, data.CRS="+init=epsg:4326") {
	r <- raster(filename)
	proj4string(r) <- CRS(data.CRS)
	return(r)
}

#' Read the raster legend
#' @description Read the raster legend. The legend is expected to contain two columns:
#' `id` containing the actual values used in the raster layer, and `value` which contains
#' the labels.
#' 
#' @param filename Name of the csv file containing the raster legend
#' @return a data table containing the raster legend
#' @export
#' @examples
#' \dontrun{
#' read_raster_legend("raster_legend.csv")
#' }
read_raster_legend <- function(filename) {
	r <- fread(filename, dec=".", header=TRUE, sep=",", na.strings=c(""))
	return(r)
}