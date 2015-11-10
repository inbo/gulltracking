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
#' @return A dataframe containing the tracking data
#' @examples
#' \dontrun{
#' load_tracks_file(inputFile)
#' }
#' @import data.table
load_tracks_file <- function(filename) {
	issues <- c()
	data <- fread(filename, dec=".", header=TRUE, sep=",", na.strings=c("\\N"))
	
	# set data types for non-character columns
	data[, date_time:=lubridate::ymd_hms(date_time)]
	
	# check whether columns can be converted to numeric
	numeric_cols <- c("latitude", "longitude", "altitude", "pressure", "temperature",
										"satellites_used", "gps_fixtime", "positiondop", "h_accuracy",
										"v_accuracy", "x_speed", "y_speed", "z_speed", "speed_accurracy",
										"speed_3d", "speed_2d", "direction", "altitude_agl")
	lapply(numeric_cols, function(x) {
		tryCatch({
			print(paste(x, "..."))
			check_numeric_values(x, data[[x]])
		}, error=function(e) {
					print(paste("non numeric values found in column ", x))
		}
		)
	})
	if (length(issues) > 0) {
		print(issues)
		stop()
	}
	
	# convert to numeric columns
	data[, latitude:=as.numeric(latitude)]
	data[, longitude:=as.numeric(longitude)]
	data[, altitude:=as.numeric(altitude)]
	data[, pressure:=as.numeric(pressure)]
	data[, temperature:=as.numeric(temperature)]
	data[, satellites_used:=as.numeric(satellites_used)]
	data[, gps_fixtime:=as.numeric(gps_fixtime)]
	data[, positiondop:=as.numeric(positiondop)]
	data[, h_accuracy:=as.numeric(h_accuracy)]
	data[, v_accuracy:=as.numeric(v_accuracy)]
	data[, x_speed:=as.numeric(x_speed)]
	data[, y_speed:=as.numeric(y_speed)]
	data[, z_speed:=as.numeric(z_speed)]
	data[, speed_accuracy:=as.numeric(speed_accuracy)]
	data[, speed_3d:=as.numeric(speed_3d)]
	data[, speed_2d:=as.numeric(speed_2d)]
	data[, direction:=as.numeric(direction)]
	data[, altitude_agl:=as.numeric(altitude_agl)]
	
	# drop unused columns
	# ... location: this is a binary blob from a geometric data type. Cannot be used.
	data[, location:=NULL]
	return(data)
}